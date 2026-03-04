# TDD (Test-Driven-Development) automated feature development

## Methodology

* Leverage red-green refactor and test pyramid, start by adding unit tests, update the code to pass the unit tests, then validate using an integration test.
* Leverage SOLID and OOP, striving to produce highly testeable code.
* DO NOT use print statements, always leverage the logging module.
* DO NOT use pdb or any form of interactive debugger.

## Context

Analysis of the codebase in `src/lain/lain_org_utils.py` shows that timestamps extracted from Org-mode files are currently handled as naive `datetime.date` or `datetime.datetime` objects. Specifically:

1.  `CleaningVisitor._extract_and_set_timestamp` uses `datetime.datetime.strptime(match.group(1), '%Y-%m-%d')` which produces a naive `datetime.datetime` object (defaulting to 00:00:00).
2.  `ThreadParser.parse_thread` uses `.date()` after `strptime`, producing a naive `datetime.date` object.
3.  `CleaningVisitor._set_lowest_timestamp` falls back to `datetime.datetime.now().date()` which is also naive.

When these naive objects are indexed in OpenSearch, they are serialized to ISO format and interpreted as **UTC** (GMT+0). Since the actual notes were taken in **GMT-5**, this causes a 5-hour offset error in dashboards. For example, a note recorded on `2024-05-18` (local time) is indexed as `2024-05-18T00:00:00Z`. A dashboard configured for GMT-5 then incorrectly displays this as `2024-05-17T19:00:00-05:00`, shifting the date to the previous day.

The ingestion process should correctly associate all `thread_date` timestamps with the **GMT-5** timezone offset to ensure consistency across dashboards.

## Task

1) **Standardize Timestamp Processing**:
    *   Update `OrgThread` and related components to use `datetime.datetime` objects with explicit timezone information (GMT-5).
    *   Modify `CleaningVisitor._extract_and_set_timestamp` to apply the `-05:00` offset to the parsed date.
    *   Ensure that fallback timestamps (e.g., from `_set_lowest_timestamp`) also include the timezone offset.
    *   Ensure all timestamps used throughout the ingestion process are aware of the GMT-5 timezone.

2) **Update Unit Regression Suite**:
    *   Add new test cases to `test/unit/test_thread_parser.py` and `test/unit/test_lain_org_utils.py` that specifically verify the presence of the `-05:00` timezone offset in `thread_date`.
    *   Ensure all existing tests are updated to account for timezone-aware datetime objects.

3) **Run Unit Regression Suite**:
    ```bash
    python -m unittest discover -s test/unit -p test_*.py
    ```

4) **Validate with Integration Suite**:
    *   Run the integration tests to confirm that documents indexed in OpenSearch contain the correctly offset `thread_date` strings (e.g., `2024-05-18T00:00:00-05:00`).
    *   Run the integration regression suite:
    ```bash
    python -m unittest discover -s test/integration -p test_*.py
    ```
