# Methodology for Deep Research and Evidence Retrieval (Lain Ecosystem)

This document abstracts the methodology used to conduct deep forensic research within the `lain-emacs` ecosystem, specifically targeting the retrieval of evidence from distributed and heterogeneous data sources.

## 1. Data Source Architecture

The environment consists of three primary layers of information:

1.  **Source Layer (Org Agenda Files):** Plain-text `.org` files containing raw logs, clock entries, and nested task hierarchies. These represent the most granular and temporally accurate records.
2.  **Indexing Layer (OpenSearch):** A search engine at `localhost:9200` that indexes ingested Org files. 
    -   `scrum-agendas`: Contains the body of threads and individual notes.
    -   `org-id-mappings`: Maps internal UUIDs to the original task structures.
3.  **Application Layer (Lain API):** A dynamic service at `localhost:8081` that provides a rendered view of the tasks, often reconstructing context that ingestion might flatten.

## 2. Information Retrieval Strategy

### A. Pattern-Based Discovery (OpenSearch)
To locate specific events without exact dates, use broad keyword searches in the `thread_body` and `task_title` fields.
*   **Search Pattern:** Use `multi_match` queries to identify keywords across multiple fields.
*   **Contextual Filtering:** When a task title is identified (e.g., `"apto brisas, investing"`), filter by that title to reconstruct the chronological sequence of notes.
*   **Example Query:**
    ```json
    {
      "query": { "match": { "task_title": "derechos fundamentales" } },
      "sort": [{ "thread_date": "desc" }]
    }
    ```

### B. Dynamic View Inspection (Lain API via Chrome DevTools)
Since the Lain API uses JavaScript to render content, static `curl` requests may fail. Use browser automation (Chrome MCP) to:
1.  Navigate to `http://localhost:8081/SCRUM.html`.
2.  Use `evaluate_script` to find specific task strings within the DOM.
3.  Identify the unique identifier (`uid`) of the task and simulate a click to open the full task detail (`ORG-TASK.html`).

### C. Forensic File Search (Grep/Ripgrep)
When API or Index layers are inconsistent, fall back to the raw source files.
*   **Temporal Targeting:** Focus on file names following the `scrum.YYYY.MM.DD.org` pattern.
*   **Cross-Referencing:** Use the `thread_date` found in OpenSearch to target specific line ranges in the `.org` files using `read_file`.
*   **Command Pattern:** `grep -rnEi "keyword" /path/to/agendas/scrum.*.org`

## 3. Heuristics for Information Gathering

To reconstruct the history of an event (e.g., a denial of legal aid), follow these logical pivots:

1.  **Identify the Intent:** Search for the high-level task title (e.g., "impugnación", "defensoría").
2.  **Trace the Interaction:** Look for "LOGBOOK" or "CLOCK" entries near the event date to confirm physical presence or duration of the activity.
3.  **Locate the Quotation:** Search for specific terms of refusal (e.g., "vulnerabilidad", "negó", "no procede").
4.  **Validate the Date:** If a date appears in a future-dated file (e.g., a 2026 entry in a 2025 file), it indicates the task was "carried forward" or updated in a long-standing log.

## 4. Key Lessons and Constraints

*   **Ingestion Lag:** OpenSearch might not reflect the most recent local changes in `.org` files. Always verify the raw file if the search index returns empty.
*   **Implicit Detail:** Key evidence is often found in the *notes* section of a task, not just the title. 
*   **API Pathing:** The Lain API uses a specific routing logic (e.g., `/scrum/` vs `/SCRUM.html`). Understanding the `lain.el` source code is required to predict valid API endpoints.
