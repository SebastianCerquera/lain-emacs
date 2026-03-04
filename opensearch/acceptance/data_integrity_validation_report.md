# Data Integrity Validation Report

## Executive Summary
This report details the findings of a data integrity validation process leveraging the `lain-emacs` API (`localhost:8081/scrum/`) and the OpenSearch indexes (`scrum-agendas`, `org-id-mappings`). The validation revealed significant gaps between the live system state and the ingested data.

## Findings

### 1. Ingestion & Search Index (OpenSearch)
*   **Data Completeness Gap:** The `scrum-agendas` index is incomplete. While the live system manages agendas from 2022 to 2026, the index only contains data from 2025.
*   **Structural Data Loss:** `LOGBOOK` and `CLOCK` entries, which contain critical time-tracking data, are not being ingested into OpenSearch.
*   **Thread Fragmentation:** The parser incorrectly handles newlines followed by dashes within threads, leading to fragmented `thread_body` entries.
*   **Unresolved Link Identifiers:** Internal link placeholders (e.g., `HTTPID...`) are present in `scrum-agendas` but often missing from `org-id-mappings`, breaking traceability.
*   **Missing Metadata:** The `file_path` field in the OpenSearch mapping is empty across all records.
*   **Temporal Inaccuracy:** Documents without explicit timestamps are being assigned the current system time during ingestion, distorting historical data.

### 2. Web Interface (Lain API)
*   **Content State Failure:** The `/scrum/` interface frequently fails to update the `ORG-TASK.html` view. Selecting different tasks often results in the same stale content (e.g., `otorrinolaringologist, health, 2026-02`) being displayed.
*   **Interaction Fragility:** The client-side JavaScript responsible for extracting task text for AJAX calls is fragile, causing intermittent failures in task selection.

### 3. Source Validation
*   **Validation Script Performance:** The `validate_org.py` script passed for all 52 files in `opensearch/scrum_agendas/`. This indicates that while the files are valid Org-mode syntax for the current parser, the parser itself is failing to capture the full structural richness required for high-fidelity data integrity.

## OpenSearch Inspection Commands

The following commands were used to verify the state of the ingested data in OpenSearch:

### 1. Index and Mapping Overview
```bash
# List all indices and document counts
curl -X GET "localhost:9200/_cat/indices?v"

# Inspect index mappings (schema)
curl -X GET "localhost:9200/scrum-agendas/_mapping?pretty"
```

### 2. Task Identification and Traceability
```bash
# Find TASKID for a specific task title
curl -X GET "localhost:9200/org-id-mappings/_search?pretty" -H 'Content-Type: application/json' -d'
{
  "query": {
    "match": { "original_value": "otorrinolaringologist, health, 2026-02" }
  }
}
'

# Query all threads belonging to a specific task_id
curl -X GET "localhost:9200/scrum-agendas/_search?pretty" -H 'Content-Type: application/json' -d'
{
  "query": {
    "term": { "task_id.keyword": "TASKID2ff32980d720" }
  }
}
'
```

### 3. Temporal Validation
```bash
# Identify the date range of indexed data
curl -X GET "localhost:9200/scrum-agendas/_search?pretty" -H 'Content-Type: application/json' -d'
{
  "aggs": {
    "min_date": { "min": { "field": "thread_date" } },
    "max_date": { "max": { "field": "thread_date" } }
  },
  "size": 0
}
'

# Search for tasks within a specific date range
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

## Recommendations
1.  **Enhance Ingestion Logic:** Update the parser to support `LOGBOOK` blocks and multi-line thread bodies.
2.  **Broaden Ingestion Scope:** Ensure all agenda files from 2022-2026 are included in the ingestion pipeline.
3.  **Fix Web State Management:** Debug the `/lain/` endpoint and associated AJAX logic to ensure `ORG-TASK.html` accurately reflects the selected task.
4.  **Populate Metadata:** Modify the indexer to include the `file_path` in each OpenSearch document.
