# TDD (Test-Driven-Development) automated feature development

## Methodology

* Leverage red-green refactor and test pyramid, start by adding unit tests, update the code to pass the unit tests, then validate using an integration test.

* Leverage SOLID and OOP, striving to produce highly testeable code.

* DO NOT use print statements, always leverage the logging module.

* DO NOT use pdb or any form of interactive debugger.

## Context 

@src/lain/lain_org_utils.py:

* `TASKID`: The TASKID hash itself is stored in the task_id field within OpenSearch documents (which represent OrgThread objects). However, the original
  task title that generated this TASKID is not currently stored as a separate field in the indexed documents. While the OpenSearch mapping includes a
  task_title field, the OrgThread.to_json() method, which dictates what is indexed, only includes the task_id (the hash) and thread_body, but not the
  original task_title.
* `HTTPID`: The HTTPID hash replaces the original URL within the thread_body content before it's indexed. Therefore, the original URL is not stored as a
  separate field, and is effectively lost from the indexed document's thread_body.

In summary, only the hashes (TASKID and HTTPID embedded in thread_body) are stored. The original texts they represent (task titles and URLs) are not
directly indexed as separate, retrievable fields.

## Task

1) Create a new index to store the mappings, the actual task and http link.

* It should work for both the TASKID suffixed tasks.
* It should work for the HTTPID hashes as well.

2) Update the unit regression suite to include the new test cases.

3) Run the unit regression suite and fix any introduced issue:

```bash
python -m unittest discover -s test/unit -p "test_*.py"
```

4) Update the integration regression suite to include the new test cases.

5) Run the integration regression suite and fix any introduced issue:

```bash
python -m unittest discover -s test/integration -p "test_*.py"
```