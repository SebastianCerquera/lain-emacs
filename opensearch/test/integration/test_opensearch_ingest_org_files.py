import unittest
import time
import os

from opensearchpy import OpenSearch
from testcontainers.opensearch import OpenSearchContainer

from src.lain.lain_org_utils import OrgFileDiscovery, OrgParser, OrgDatabase

class TestOpenSearchOrgIngestion(unittest.TestCase):

    opensearch_container = None
    opensearch_client = None
    
    @classmethod
    def setUpClass(cls):
        print("\nStarting OpenSearch container for Org Ingestion tests...")
        cls.opensearch_container = OpenSearchContainer("opensearchproject/opensearch:3.2.0")
        cls.opensearch_container.with_env("OPENSEARCH_INITIAL_ADMIN_PASSWORD", "myStrongPassword1!")
        cls.opensearch_container.with_env("plugins.security.disabled", "true") 
        cls.opensearch_container.start()

        host = cls.opensearch_container.get_container_host_ip()
        port = cls.opensearch_container.get_exposed_port(9200)
        auth = ('admin', 'myStrongPassword1!') 
        
        # Set the OPENSEARCH_ENDPOINT environment variable for the OrgDatabase
        os.environ["OPENSEARCH_ENDPOINT"] = f"http://{host}:{port}"

        # Initialize the OpenSearch client for readiness checks and subsequent use
        cls.opensearch_client = OpenSearch(
            hosts=[{'host': host, 'port': port}],
            http_auth=auth,
            use_ssl=False,
            verify_certs=False,
            request_timeout=30 # Add a timeout for the client
        )

        max_tries = 60
        for i in range(max_tries):
            try:
                health = cls.opensearch_client.cluster.health()
                if health['status'] in ['green', 'yellow']:
                    test_index_name = "readiness-test-index"
                    test_doc_id = "readiness-doc-1"
                    test_document = {"message": "OpenSearch is ready for operations"}

                    cls.opensearch_client.indices.create(index=test_index_name, ignore=400)
                    cls.opensearch_client.index(index=test_index_name, id=test_doc_id, body=test_document, refresh=True)
                    search_result = cls.opensearch_client.search(index=test_index_name, body={"query": {"match_all": {}}})
                    
                    if search_result['hits']['total']['value'] > 0:
                        print(f"OpenSearch is fully operational after {i+1} tries.")
                        cls.opensearch_client.indices.delete(index=test_index_name)
                        break
            except Exception as e:
                print(f"Attempt {i+1}/{max_tries}: OpenSearch operational check failed - {e}")
            time.sleep(5)
        else:
            raise Exception("OpenSearch did not become fully operational within the expected time.")

        print("OpenSearch container started and client initialized.")

    @classmethod
    def tearDownClass(cls):
        print("Stopping OpenSearch container...")
        if cls.opensearch_container:
            cls.opensearch_container.stop()
        print("OpenSearch container stopped.")

    def test_org_files_ingestion(self):
        sample_files_dir = "sample_files"
        org_files = OrgFileDiscovery.discover_files(sample_files_dir)
        self.assertGreater(len(org_files), 0, "No .org files found in sample_files directory.")

        index_name = 'my-org-index-2024-05-21--1' # Hardcoded in OrgDatabase
        
        # Ensure the index is fresh for this test run
        if self.opensearch_client.indices.exists(index=index_name):
            self.opensearch_client.indices.delete(index=index_name)
            
        org_database_visitor = OrgDatabase(index_name=index_name) # OrgDatabase now gets client from env var

        for file_path in org_files:
            org_file = OrgParser.parse(file_path)
            org_file.accept(org_database_visitor)
            
        # Give OpenSearch some time to refresh its index, if needed
        self.opensearch_client.indices.refresh(index=index_name)

        # Verify ingestion
        search_body = {"query": {"match_all": {}}}
        response = self.opensearch_client.search(index=index_name, body=search_body)
        
        self.assertGreater(response['hits']['total']['value'], 0, "No documents were ingested into OpenSearch.")

if __name__ == '__main__':
    unittest.main()
