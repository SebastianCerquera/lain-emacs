import unittest
import time
from unittest.mock import MagicMock
from orgparse.node import OrgNode
from opensearchpy import OpenSearch, ConnectionError
from testcontainers.opensearch import OpenSearchContainer
import os
import sys
import random
import logging
from typing import List

# Add the src directory to the Python path to import project modules
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../src')))
from lain.lain_org_utils import OrgFile, OrgTask, OrgThread, OrgParser, OrgFileDiscovery, OrgDatabase, CleaningVisitor, OrgParserVisitor

# Initialize logger for this module
logger = logging.getLogger(__name__)


class TestOpenSearchIntegration(unittest.TestCase): # Renamed class

    opensearch_container = None
    os_client = None
    org_database_visitor = None
    test_index_name = "test-org-documents"
    test_mappings_index_name = "test-org-id-mappings" # Added mappings index name

    @classmethod
    def setUpClass(cls):
        # Configure logging: set root level to ERROR to be very quiet by default
        logging.basicConfig(level=logging.ERROR, 
                            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')

        # Set our module logger to INFO
        logger.setLevel(logging.INFO)
        # Also set the library we are testing to INFO
        logging.getLogger("lain").setLevel(logging.INFO)

        # Suppress high verbosity from external libraries
        for noisy_logger in ["opensearch", "urllib3", "testcontainers", "docker", "paramiko"]:
            logging.getLogger(noisy_logger).setLevel(logging.ERROR)


        # Start the OpenSearch container
        cls.opensearch_container = OpenSearchContainer("opensearchproject/opensearch:3.2.0")
        cls.opensearch_container.with_env("OPENSEARCH_INITIAL_ADMIN_PASSWORD", "myStrongPassword1!")
        cls.opensearch_container.with_env("plugins.security.disabled", "true") # Explicitly disable security
        cls.opensearch_container.start()

        host = cls.opensearch_container.get_container_host_ip()
        port = cls.opensearch_container.get_exposed_port(9200)
        auth = ('admin', 'myStrongPassword1!') # Still provide auth, but it should be ignored if security is disabled
        
        
        # Set the OPENSEARCH_ENDPOINT environment variable for the OrgDatabase
        os.environ["OPENSEARCH_ENDPOINT"] = f"http://{host}:{port}"

        # Initialize the OpenSearch client once
        cls.os_client = OpenSearch(
            hosts=[{'host': host, 'port': port}],
            http_auth=auth,
            use_ssl=False,
            verify_certs=False,
            request_timeout=30 # Add a timeout for the client
        )

        max_tries = 60
        for i in range(max_tries):
            try:
                # Use cls.os_client for readiness checks
                health = cls.os_client.cluster.health()
                if health['status'] in ['green', 'yellow']:
                    logger.info(f"OpenSearch cluster is ready after {i+1} tries. Status: {health['status']}")
                    break
            except ConnectionError as e:
                logger.debug(f"Attempt {i+1}/{max_tries}: OpenSearch not ready yet - {e}")
            except Exception as e:
                logger.warning(f"Attempt {i+1}/{max_tries}: OpenSearch operational check failed - {e}")
            time.sleep(5)
        else:
            raise Exception("OpenSearch did not become fully operational within the expected time.")

        # Initialize the OrgDatabase visitor, which creates the indices if they don't exist
        cls.org_database_visitor = OrgDatabase(opensearch_client=cls.os_client, index_name=cls.test_index_name, mappings_index_name=cls.test_mappings_index_name)
        
        # Explicitly wait for the indices to be created and become healthy
        max_index_wait_tries = 30
        for i in range(max_index_wait_tries):
            try:
                if cls.os_client.indices.exists(index=cls.test_index_name):
                    cls.os_client.cluster.health(index=cls.test_index_name, wait_for_status='yellow', timeout=10)
                    logger.info(f"Main index '{cls.test_index_name}' is ready.")
                    break
            except Exception as e:
                logger.debug(f"Waiting for main index readiness: {e}")
            time.sleep(2)
        else:
            raise Exception(f"Main index '{cls.test_index_name}' did not become ready within the expected time.")

        for i in range(max_index_wait_tries):
            try:
                if cls.os_client.indices.exists(index=cls.test_mappings_index_name):
                    cls.os_client.cluster.health(index=cls.test_mappings_index_name, wait_for_status='yellow', timeout=10)
                    logger.info(f"Mappings index '{cls.test_mappings_index_name}' is ready.")
                    break
            except Exception as e:
                logger.debug(f"Waiting for mappings index readiness: {e}")
            time.sleep(2)
        else:
            raise Exception(f"Mappings index '{cls.test_mappings_index_name}' did not become ready within the expected time.")

        # Ensure indices are refreshed after creation
        cls.os_client.indices.refresh(index=cls.test_index_name)
        cls.os_client.indices.refresh(index=cls.test_mappings_index_name)

    @classmethod
    def tearDownClass(cls):
        # Stop the OpenSearch container
        if cls.opensearch_container:
            cls.opensearch_container.stop()
        
        # Ensure the test indices are deleted after tests
        try:
            if cls.os_client and cls.os_client.indices.exists(index=cls.test_index_name):
                cls.os_client.indices.delete(index=cls.test_index_name)
            if cls.os_client and cls.os_client.indices.exists(index=cls.test_mappings_index_name): # Delete mappings index
                cls.os_client.indices.delete(index=cls.test_mappings_index_name)
        except ConnectionError:
            logger.warning("Could not connect to OpenSearch to delete indices during tearDownClass. It might have been stopped already.")

        # Clean up the environment variable
        if "OPENSEARCH_ENDPOINT" in os.environ:
            del os.environ["OPENSEARCH_ENDPOINT"]

    def test_ingest_org_files_and_verify(self):
        sample_files_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '../../sample_files'))
        org_file_paths = OrgFileDiscovery.discover_files(sample_files_dir)
        self.assertGreater(len(org_file_paths), 0, "No .org files found in sample_files directory.")

        all_collected_links = {} # To collect all links from all files

        for file_path in org_file_paths:
            org_file, cleaning_visitor = OrgParser.parse(file_path)
            org_file.accept(self.org_database_visitor) # OrgDatabase directly indexes threads
            self.org_database_visitor.index_mappings(cleaning_visitor.links) # Index mappings
            self.os_client.indices.refresh(index=self.test_mappings_index_name) # Explicitly refresh mappings index

            all_collected_links.update(cleaning_visitor.links) # Collect links

        # Refresh both indices to make documents searchable
        self.os_client.indices.refresh(index=self.test_index_name)
        self.os_client.indices.refresh(index=self.test_mappings_index_name) # Refresh mappings index

        # Verify total count of documents in the main index
        count_result = self.os_client.count(index=self.test_index_name)
        self.assertGreater(count_result['count'], 0, "No documents were ingested into OpenSearch.")

        # Get all ingested documents to pick a random one
        search_result_all = self.os_client.search(index=self.test_index_name, body={"query": {"match_all": {}}})
        ingested_documents = search_result_all['hits']['hits']
        self.assertGreater(len(ingested_documents), 0, "No documents found in OpenSearch to pick a random one.")

        # Pick a random ingested document and verify its presence and task_title
        random_doc_source = random.choice(ingested_documents)['_source']
        random_task_id = random_doc_source['task_id']
        random_thread_content = random_doc_source['thread_body']
        
        self.assertIn('task_title', random_doc_source, "Ingested document should contain 'task_title'.")
        self.assertIsNotNone(random_doc_source['task_title'], "task_title should not be None.")
        self.assertGreater(len(random_doc_source['task_title']), 0, "task_title should not be an empty string.")
        
        search_body = {
            "query": {
                "bool": {
                    "must": [
                        {"term": {"task_id.keyword": random_task_id}},
                        {"match": {"thread_body": random_thread_content}}
                    ]
                }
            }
        }
        search_result = self.os_client.search(index=self.test_index_name, body=search_body)
        
        self.assertGreater(search_result['hits']['total']['value'], 0, f"Random thread with task_id {random_task_id} and content '{random_thread_content}' not found.")
        self.assertTrue(any(hit['_source']['task_id'] == random_task_id and 
                            hit['_source']['thread_body'] == random_thread_content 
                            for hit in search_result['hits']['hits']), 
                            "Found a document, but it doesn't match the random thread's content.")

        # Verify mappings index
        # Refresh mappings index one last time
        self.os_client.indices.refresh(index=self.test_mappings_index_name)
        time.sleep(2) # Give it a bit more time to settle

        mappings_count_result = self.os_client.count(index=self.test_mappings_index_name)
        self.assertGreaterEqual(mappings_count_result['count'], len(all_collected_links), 
                         f"Expected at least {len(all_collected_links)} mappings but found {mappings_count_result['count']}.")

        # Verify a sample of mappings
        sample_links = list(all_collected_links.items())
        random.shuffle(sample_links)
        
        # We'll check at most 10 mappings to avoid taking too long
        for original_value, hashed_id in sample_links[:10]:
            max_retries = 10
            found = False
            for retry in range(max_retries):
                # Use a match query on hashed_id
                search_body_mapping = {
                    "query": {
                        "match": {
                            "hashed_id": hashed_id
                        }
                    }
                }
                mapping_search_result = self.os_client.search(index=self.test_mappings_index_name, body=search_body_mapping)
                if mapping_search_result['hits']['total']['value'] > 0:
                    found = True
                    break
                time.sleep(1)
            
            self.assertTrue(found, 
                               f"Mapping for hashed_id {hashed_id} (original: {original_value}) not found in mappings index after {max_retries} retries.")

    def test_thread_date_has_timezone_offset(self):
        # Index a single thread with a known date
        node = MagicMock(spec=OrgNode)
        node.heading = "Timezone Test Task"
        task = OrgTask(node)
        task.id = "TASKID_TZ_TEST"
        thread = OrgThread("- <2024-05-18> Timezone test thread", task=task)
        
        # We need to run the parser and cleaner visitors
        # We also need to manually set the raw content
        thread.raw = "- <2024-05-18> Timezone test thread"
        
        parser_visitor = OrgParserVisitor()
        thread.accept(parser_visitor)
        
        cleaning_visitor = CleaningVisitor()
        thread.accept(cleaning_visitor)
        
        # Index the thread
        self.org_database_visitor.visit_org_thread(thread)
        self.os_client.indices.refresh(index=self.test_index_name)
        
        # Search for the thread
        search_body = {
            "query": {
                "term": {"task_id.keyword": "TASKID_TZ_TEST"}
            }
        }
        search_result = self.os_client.search(index=self.test_index_name, body=search_body)
        
        self.assertEqual(search_result['hits']['total']['value'], 1)
        doc = search_result['hits']['hits'][0]['_source']
        
        # OpenSearch stores dates as strings in ISO format
        thread_date = doc['thread_date']
        self.assertTrue(thread_date.endswith("-05:00"), f"Expected thread_date to end with -05:00, got {thread_date}")
        self.assertIn("2024-05-18T00:00:00-05:00", thread_date)

    def test_retrieve_entire_tree_by_thread_id(self):
        # given:
        node = MagicMock(spec=OrgNode)
        node.heading = "Tree Retrieval Task"
        task = OrgTask(node)
        task.id = "TASKID_TREE_TEST"
        
        # Structure:
        # - Parent (Root)
        #   - Child 1
        #   - Child 2
        #     - Grandchild 2.1
        
        parent = OrgThread("- Parent", task)
        child1 = OrgThread("- Child 1", task, parent=parent)
        child2 = OrgThread("- Child 2", task, parent=parent)
        grandchild21 = OrgThread("- Grandchild 2.1", task, parent=child2)
        
        parent.add_child(child1)
        parent.add_child(child2)
        child2.add_child(grandchild21)
        
        # Set raw for parsing
        parent.raw = "- Parent"
        child1.raw = "- Child 1"
        child2.raw = "- Child 2"
        grandchild21.raw = "- Grandchild 2.1"
        
        # Set content for ID generation (simulating CleaningVisitor)
        parent.content = "Parent"
        child1.content = "Child 1"
        child2.content = "Child 2"
        grandchild21.content = "Grandchild 2.1"
        
        from lain.lain_org_utils import HierarchyVisitor
        visitor = HierarchyVisitor()
        visitor.visit_org_task(task)
        parent.accept(visitor)
        
        # Index all threads
        self.org_database_visitor.visit_org_thread(parent)
        self.org_database_visitor.visit_org_thread(child1)
        self.org_database_visitor.visit_org_thread(child2)
        self.org_database_visitor.visit_org_thread(grandchild21)
        
        self.os_client.indices.refresh(index=self.test_index_name)
        
        # when: Retrieve all threads by thread_id
        thread_id = parent.node_id
        search_body = {
            "query": {
                "term": {"thread_id": thread_id}
            }
        }
        search_result = self.os_client.search(index=self.test_index_name, body=search_body)
        
        # then:
        self.assertEqual(search_result['hits']['total']['value'], 4)
        
        # Reconstruct in memory
        hits = [hit['_source'] for hit in search_result['hits']['hits']]
        
        # Basic check
        for hit in hits:
            self.assertEqual(hit['thread_id'], thread_id)
            
        # Verify specific links
        hit_map = {hit['node_id']: hit for hit in hits}
        
        parent_hit = hit_map[parent.node_id]
        child1_hit = hit_map[child1.node_id]
        child2_hit = hit_map[child2.node_id]
        grandchild_hit = hit_map[grandchild21.node_id]
        
        self.assertEqual(parent_hit['parent_id'], task.id)
        self.assertEqual(child1_hit['parent_id'], parent.node_id)
        self.assertEqual(child2_hit['parent_id'], parent.node_id)
        self.assertEqual(grandchild_hit['parent_id'], child2.node_id)

if __name__ == "__main__":
    unittest.main()
