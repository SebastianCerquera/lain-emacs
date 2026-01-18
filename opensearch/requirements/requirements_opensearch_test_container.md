# TDD (Test-Driven-Development) automated feature development

## Methodology

Leverage red-green refactor and test pyramid, start by adding unit tests, update the code to pass the unit tests, then validate using an integration test.

Leverage SOLID and OOP, striving to produce highly testeable code.

## Context

### Test containers

Opensearch container might take some time to deploy, leverage the sniped below to make sure it is properly runing before runing the suite.

```python
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
        
        max_tries = 60
        for i in range(max_tries):
            try:
                temp_client = OpenSearch(
                    hosts=[{'host': host, 'port': port}],
                    http_auth=auth,
                    use_ssl=False,
                    verify_certs=False
                )
                # Check cluster health
                health = temp_client.cluster.health()
                if health['status'] in ['green', 'yellow']:
                    # Attempt to create an index, index a document, and search for it
                    test_index_name = "readiness-test-index"
                    test_doc_id = "readiness-doc-1"
                    test_document = {"message": "OpenSearch is ready for operations"}

                    temp_client.indices.create(index=test_index_name, ignore=400) # ignore 400 if index already exists
                    temp_client.index(index=test_index_name, id=test_doc_id, body=test_document, refresh=True)
                    search_result = temp_client.search(index=test_index_name, body={"query": {"match_all": {}}})
                    
                    if search_result['hits']['total']['value'] > 0:
                        print(f"OpenSearch is fully operational after {i+1} tries.")
                        temp_client.indices.delete(index=test_index_name)
                        break
            except ConnectionError as e:
                print(f"Attempt {i+1}/{max_tries}: OpenSearch not ready yet - {e}")
            except Exception as e:
                print(f"Attempt {i+1}/{max_tries}: OpenSearch operational check failed - {e}")
            time.sleep(5)
        else:
            raise Exception("OpenSearch did not become fully operational within the expected time.")

        # Initialize the OpenSearchClient with container details
        cls.opensearch_client = OpenSearchClient(
            host=host,
            port=port,
            auth=auth,
            use_ssl=False, # Testcontainers might not set up SSL by default
            verify_certs=False
        )

    @classmethod
    def tearDownClass(cls):
        # Stop the OpenSearch container
        if cls.opensearch_container:
            cls.opensearch_container.stop()
```

### Divide and coqueer

If after running the integration regression suite you still find that there are entries that weren't ingested, take a divide and conqueer approach, you can do this by breaking down the @sample_file/scrum.org file into smaller segmented version with only a subset of task, this way you'll be able to pinpoint the task entries that are falining in the ingestion process.

```bash
python -m unittest discover -v -s integration/unit -p test_*.py
```

## Task

1) Implement a new integration suite leveraging the recomendation in the context, it should look for org files from the @sample_files and ingest it into opensearch, then it should pick a random task and check that there are documents, meaning that it was properly loaded.

* I can confirm that the org files in the @sample_files are properly formated and the entries are valid, there are no issues in this file, if you pick a segment of an entry and look it up in opensearch after the ingestion process, it should provide resutls.

* I am certain that if you pick a line from any task it wil be unique.

2) Run the integration suite, fix any issue up until it works. Remember to leverage TDD and test pyramid, any modification to the code should trigger the unit regression suite.

```bash
python -m unittest discover -v -s test/unit -p test_*.py
```

3) Run the integration regression suite and confirm that the data was loaded.
