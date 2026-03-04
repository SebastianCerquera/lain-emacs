# Data Integrity Report - Org-mode Ingestion

## Summary
During the ingestion of the `SMALL/LAIN/` directory into OpenSearch, several files failed to parse due to malformed timestamps or structural anomalies. The ingestion logic has been updated to handle these gracefully, but the source data remains inconsistent.

## Identified Issues

### 1. Malformed Minutes (Out of Range)
*   **File:** `SMALL/LAIN/scrum.2024.10.07.org`
*   **Error:** `ValueError: minute must be in 0..59`
*   **Cause:** A `CLOCK` entry contained a three-digit minute.
    *   *Line 343:* `CLOCK: [2024-10-13 dom 10:45]--[2024-10-13 dom 11:555] =>  1:10`

### 2. Malformed Days (Out of Range)
*   **File:** `SMALL/LAIN/scrum.org`
*   **Error:** `ValueError: day is out of range for month`
*   **Cause:** A timestamp used an impossible day of the month.
    *   *Line 35338:* `- <2023-06-52 lun> Parece que hay un extension de vscode para org.`

### 3. Empty Org Files
*   **File:** `SMALL/LAIN/emacs.org`
*   **Error:** `IndexError: list index out of range`
*   **Cause:** The file exists but contains no Org-mode headers or valid children, causing `orgparse.children[0]` to fail.

### 4. Transient/Backup Files
*   The discovery process originally picked up Emacs lock files (`.#filename.org`) and backup files (`filename.org~`). These are now filtered out automatically.

## Remediation
1.  **Robust Ingestion:** `OrgParser.parse` now catches `orgparse` exceptions and returns `None`, allowing the ingestion to skip bad files without crashing the process.
2.  **Discovery Filtering:** `OrgFileDiscovery` now excludes hidden and backup files.
3.  **Validation Tool:** A standalone validation script (`scripts/validate_org.py`) has been added to identify these issues before ingestion.
