# Instructions for Data Integrity Validation

This document outlines the technical process for validating data integrity across the `lain-emacs` API and OpenSearch indexes.

## 1. Source File Validation
Ensure the Org-mode source files are syntactically valid for the current parser.

```bash
python3 opensearch/scripts/validate_org.py
```

## 2. OpenSearch Index Inspection
Verify the state, schema, and document counts within the OpenSearch indexes.

### List Indices and Document Counts
```bash
curl -X GET "localhost:9200/_cat/indices?v"
```

### Inspect Index Mappings (Schema)
```bash
curl -X GET "localhost:9200/scrum-agendas/_mapping?pretty"
```

## 3. Task Traceability Validation
Confirm that specific tasks are correctly mapped to IDs and that threads are accurately associated with those IDs.

### Find a TASKID for a Specific Task Title
```bash
curl -X GET "localhost:9200/org-id-mappings/_search?pretty" -H 'Content-Type: application/json' -d'
{
  "query": {
    "match": { "original_value": "INSERT_TASK_TITLE_HERE" }
  }
}
'
```

### Query All Threads for a Specific task_id
```bash
curl -X GET "localhost:9200/scrum-agendas/_search?pretty" -H 'Content-Type: application/json' -d'
{
  "query": {
    "term": { "task_id.keyword": "INSERT_TASKID_HERE" }
  }
}
'
```

## 4. Temporal (Date Range) Validation
Validate the temporal completeness and accuracy of indexed data.

### Identify the Date Range of All Indexed Data
```bash
curl -X GET "localhost:9200/scrum-agendas/_search?pretty" -H 'Content-Type: application/json' -d'
{
  "aggs": {
    "min_date": { "min": { "field": "thread_date" } },
    "max_date": { "max": { "field": "thread_date" } }
  },
  "size": 0
}
'
```

### Search for Tasks Within a Specific 24-Hour Window
```bash
curl -X GET "localhost:9200/scrum-agendas/_search?pretty" -H 'Content-Type: application/json' -d'
{
  "query": {
    "range": {
      "thread_date": {
        "gte": "2026-02-23T00:00:00-05:00",
        "lte": "2026-02-23T23:59:59-05:00"
      }
    }
  }
}
'
```

## 5. Web API Interface Validation
Verify the `lain-emacs` API state and UI synchronization. This process MUST be performed leveraging Chrome MCP tools.

### Automated Validation Steps

1. **Navigate to the Scrum View:**
   Use `new_page` to navigate to the Scrum endpoint.
   ```json
   { "url": "http://localhost:8081/scrum/" }
   ```

2. **Access Task Details:**
   Task details can ONLY be accessed by clicking on the task state (e.g., `TODO`, `IN_PROGRESS`, `DONE`).
   - Use `take_snapshot` to identify the `uid` of the task state span.
   - Use `click` on the identified `uid`.

3. **Verify Dynamic Content Update:**
   - Confirm that the URL changes to include `ORG-TASK.html`.
   - Use `take_snapshot` or `evaluate_script` to confirm that the view reflects the specific content of the selected task rather than stale data.

