# TDD (Test-Driven-Development) automated feature development

## Methodology

* Leverage red-green refactor and test pyramid, start by adding unit tests, update the code to pass the unit tests, then validate using an integration test.
* Leverage SOLID and OOP, striving to produce highly testeable code.
* DO NOT use print statements, always leverage the logging module.
* DO NOT use pdb or any form of interactive debugger.

## Context

The ingestion process implemented in `scripts/ingest.py` and `src/lain/lain_org_utils.py` faced several limitations and bugs when running against real-world data:

1.  **Lack of Configuration**: The OpenSearch index name was defaulted to a hardcoded value (`my-org-index-2024-05-21--1`) and could not be configured via the CLI.
2.  **Broken Initialization**: `OrgModule.run` was attempting to instantiate `OrgDatabase()` without providing the required `opensearch_client` argument, leading to an immediate `TypeError`.
3.  **Shallow and Fragile File Discovery**:
    *   Discovery was limited to the top-level directory provided.
    *   Directories named with a `.org` suffix (e.g., `SMALL/LAIN/scrum2026.02.10.org`) were incorrectly identified as files, causing `IsADirectoryError` during parsing.
4.  **Lack of Error Resilience**: Malformed Org-mode files containing invalid dates (e.g., `2023-06-52`) or invalid timestamps (e.g., `11:555`) would cause the entire ingestion process to crash and exit, preventing the processing of subsequent valid files.

## Task

1) **Enhance CLI Arguments**:
    *   Modify `scripts/ingest.py` to accept an optional `--index-name` argument.
    *   Ensure this value is passed through to the `OrgDatabase` initialization.

2) **Fix Core Library Initialization**:
    *   Correct `OrgModule.run` to properly initialize the `OpenSearch` client using the `OPENSEARCH_ENDPOINT` environment variable.
    *   Pass the initialized client to the `OrgDatabase` constructor.

3) **Improve File Discovery**:
    *   Implement recursive file discovery using `os.walk` to pick up `.org` files in nested subdirectories (e.g., `journals/`, `youtube/`).
    *   Add validation to ensure only actual files (not directories) are processed.

4) **Implement Graceful Error Handling**:
    *   Wrap the file parsing and indexing loop in a `try...except` block.
    *   Log detailed errors for malformed files while allowing the ingestion process to continue with the remaining files.

5) **Verify with Regression Suite**:
    *   Add unit tests to verify that `OrgModule` correctly handles the new `index_name` argument and initializes the database properly.
    *   Ensure all existing tests pass.
