import unittest
import time
from opensearchpy import OpenSearch, ConnectionError
from testcontainers.opensearch import OpenSearchContainer
import os
import sys
import random
from typing import List

# Add the src directory to the Python path to import project modules
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../src')))
from lain.lain_org_utils import OrgFile, OrgTask, OrgThread, OrgParser, OrgFileDiscovery, OrgDatabase, CleaningVisitor, OrgParserVisitor


class TestOpenSearchIngestion(unittest.TestCase):

    opensearch_container = None
    os_client = None
    org_database_visitor = None
    test_index_name = "test-org-documents"

    @classmethod
    def setUpClass(cls):
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

        max_tries = 60
        for i in range(max_tries):
            try:
                temp_client = OpenSearch(
                    hosts=[{'host': host, 'port': port}],
                    http_auth=auth,
                    use_ssl=False,
                    verify_certs=False,
                    request_timeout=30 # Add a timeout for the client
                )
                # Check cluster health
                health = temp_client.cluster.health()
                if health['status'] in ['green', 'yellow']:
                    # Attempt to create an index, index a document, and search for it
                    readiness_test_index_name = "readiness-test-index"
                    test_doc_id = "readiness-doc-1"
                    test_document = {"message": "OpenSearch is ready for operations"}

                    temp_client.indices.create(index=readiness_test_index_name, ignore=400) # ignore 400 if index already exists
                    temp_client.index(index=readiness_test_index_name, id=test_doc_id, body=test_document, refresh=True)
                    search_result = temp_client.search(index=readiness_test_index_name, body={"query": {"match_all": {}}})
                    
                    if search_result['hits']['total']['value'] > 0:
                        print(f"OpenSearch is fully operational after {i+1} tries.")
                        temp_client.indices.delete(index=readiness_test_index_name)
                        break
            except ConnectionError as e:
                print(f"Attempt {i+1}/{max_tries}: OpenSearch not ready yet - {e}")
            except Exception as e:
                print(f"Attempt {i+1}/{max_tries}: OpenSearch operational check failed - {e}")
            time.sleep(5)
        else:
            raise Exception("OpenSearch did not become fully operational within the expected time.")

        # Initialize the OpenSearch client and OrgDatabase visitor
        cls.os_client = OpenSearch(
            hosts=[{'host': host, 'port': port}],
            http_auth=auth,
            use_ssl=False,
            verify_certs=False,
            request_timeout=30 # Add a timeout for the client
        )
        cls.org_database_visitor = OrgDatabase(index_name=cls.test_index_name)

    @classmethod
    def tearDownClass(cls):
        # Stop the OpenSearch container
        if cls.opensearch_container:
            cls.opensearch_container.stop()
        
        # Ensure the test index is deleted after tests
        try:
            if cls.os_client and cls.os_client.indices.exists(index=cls.test_index_name):
                cls.os_client.indices.delete(index=cls.test_index_name)
        except ConnectionError:
            print(f"Warning: Could not connect to OpenSearch to delete index {cls.test_index_name} during tearDownClass. It might have been stopped already.")

        # Clean up the environment variable
        if "OPENSEARCH_ENDPOINT" in os.environ:
            del os.environ["OPENSEARCH_ENDPOINT"]

    def test_ingest_org_files_and_verify(self):
        sample_files_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '../../sample_files'))
        org_file_paths = OrgFileDiscovery.discover_files(sample_files_dir)
        self.assertGreater(len(org_file_paths), 0, "No .org files found in sample_files directory.")

        for file_path in org_file_paths:
            org_file = OrgParser.parse(file_path)
            org_file.accept(self.org_database_visitor) # Ingests threads using the visitor pattern
        
        # Refresh the index to make documents searchable
        self.os_client.indices.refresh(index=self.test_index_name)

        # Verify total count of documents
        count_result = self.os_client.count(index=self.test_index_name)
        self.assertGreater(count_result['count'], 0, "No documents were ingested into OpenSearch.")

        # Get all ingested documents to pick a random one
        search_result_all = self.os_client.search(index=self.test_index_name, body={"query": {"match_all": {}}})
        ingested_documents = search_result_all['hits']['hits']
        self.assertGreater(len(ingested_documents), 0, "No documents found in OpenSearch to pick a random one.")

        # Pick a random ingested document and verify its presence
        random_doc_source = random.choice(ingested_documents)['_source']
        random_task_id = random_doc_source['task_id']
        random_thread_content = random_doc_source['thread_body']
        
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
        self.os_client.indices.refresh(index=self.test_index_name)
        search_result = self.os_client.search(index=self.test_index_name, body=search_body)
        
        self.assertGreater(search_result['hits']['total']['value'], 0, f"Random thread with task_id {random_task_id} and content '{random_thread_content}' not found.")
        self.assertTrue(any(hit['_source']['task_id'] == random_task_id and 
                            hit['_source']['thread_body'] == random_thread_content 
                            for hit in search_result['hits']['hits']), 
                            "Found a document, but it doesn't match the random thread's content.")