import unittest
import os
import time
import random
import string

from opensearchpy import OpenSearch, ConnectionError
from testcontainers.opensearch import OpenSearchContainer

from lain.lain_org_utils import OrgModule, OrgDatabase, OrgThread, OrgFile, OrgParser, OrgThreadContentCollector, OrgFileDiscovery

class TestOpenSearchIntegration(unittest.TestCase):

    opensearch_container = None
    opensearch_client = None

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

        # Initialize the OpenSearchClient with container details
        cls.opensearch_client = OpenSearch(
            hosts=[{'host': host, 'port': port}],
            http_auth=auth,
            use_ssl=False, # Testcontainers might not set up SSL by default
            verify_certs=False,
            request_timeout=30 # Add a timeout for the client
        )
        print("OpenSearch container started and client initialized.")

    @classmethod
    def tearDownClass(cls):
        print("Stopping OpenSearch container...")
        if cls.opensearch_container:
            cls.opensearch_container.stop()
        
        # Clean up the environment variable
        if "OPENSEARCH_ENDPOINT" in os.environ:
            del os.environ["OPENSEARCH_ENDPOINT"]
        print("OpenSearch container stopped.")

    def test_org_file_ingestion(self):
        index_name = 'my-org-index-2024-05-21--1' # Hardcoded in OrgDatabase
        
        # Ensure the index is fresh for this test run
        if self.opensearch_client.indices.exists(index=index_name):
            self.opensearch_client.indices.delete(index=index_name)
            
        org_database_visitor = OrgDatabase(index_name=index_name) # OrgDatabase now gets client from env var

        # Collect all task IDs during parsing
        all_task_ids = []
        all_expected_thread_contents = set()

        files = OrgFileDiscovery.discover_files("sample_files")
        self.assertGreater(len(files), 0, "No .org files found in sample_files directory.")

        for file_path in files:
            org_file = OrgParser.parse(file_path)
            
            # Collect task IDs from the parsed org_file
            for task in org_file.tasks:
                if hasattr(task, 'id'): # Ensure the task has an ID after CleaningVisitor
                    all_task_ids.append(task.id)
            
            # Collect expected thread contents for verification later
            collector = OrgThreadContentCollector()
            org_file.accept(collector)
            all_expected_thread_contents.update(collector.thread_contents)

            # Ingest threads into OpenSearch
            org_file.accept(org_database_visitor)
            
        # Give OpenSearch some time to refresh its index, if needed
        self.opensearch_client.indices.refresh(index=index_name)

        # Verify that documents have been ingested
        search_result = self.opensearch_client.search(index=index_name, body={"query": {"match_all": {}}})
        self.assertGreater(search_result['hits']['total']['value'], 0, "No documents were ingested into OpenSearch.")

        # Collect all unique task IDs from the actually ingested documents
        ingested_task_ids = {hit['_source']['task_id'] for hit in search_result['hits']['hits']}
        self.assertGreater(len(ingested_task_ids), 0, "No task IDs found in ingested documents.")
        
        # Pick a random task ID from the ingested ones
        random_task_id = random.choice(list(ingested_task_ids))

        # Check if the ingested documents' thread bodies are in the expected_thread_contents
        ingested_thread_bodies = {hit['_source']['thread_body'] for hit in search_result['hits']['hits']}

        # Assert that all ingested documents' bodies are among the expected contents
        for ingested_body in ingested_thread_bodies:
            # We allow empty content to be indexed, so only assert if content is not empty
            if ingested_body is not None and ingested_body.strip() != "":
                self.assertIn(ingested_body, all_expected_thread_contents, f"Ingested content '{ingested_body}' not found in expected contents.")
        
        search_body_task = {
            "query": {
                "match": {
                    "task_id.keyword": random_task_id
                }
            }
        }
        task_search_result = self.opensearch_client.search(index=index_name, body=search_body_task)
        self.assertGreater(task_search_result['hits']['total']['value'], 0, 
                           f"No documents found for random task ID: {random_task_id}")

        print(f"Successfully verified documents for random task ID: {random_task_id}")
