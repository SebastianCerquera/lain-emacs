# Requirement: Fix Scrum API Usability (Sticky Tasks)

## 1. Problem Statement
The current `lain-emacs` system suffers from a usability issue in the web interface as documented in the `data_integrity_validation_report.md`:
*   **Web Interface Content State Failure:** The `/scrum/` interface frequently fails to update the `ORG-TASK.html` view. Selecting different tasks often results in the same stale content (e.g., `otorrinolaringologist, health, 2026-02`) being displayed.
*   **Root Cause:** This is caused by persistent buffer narrowing in Emacs. Once a task is selected, the buffer remains narrowed to that specific subtree. Subsequent requests to load different tasks fail because the search for the next task marker is performed within the narrowed buffer, where the other markers are invisible.

## 2. Proposed Architectural Changes

### 2.1. Emacs Backend (`lain/lain.el`)
*   **Narrowing Reset:** Ensure that every function performing narrowing (`org-narrow-to-subtree`) calls `widen` after the side-effect (writing the task to HTML) is complete. This resets the buffer state for the next request.
    *   Affected functions: `lain-create-agenda-view`, `lain-reschedule-task`, `lain-update-task`.
*   **Buffer Management:** Update `lain-kill-org-buffers` to use a more inclusive regex (`scrum.*\.org`) instead of the literal `scrum.org`. This ensures that dated agenda files (e.g., `scrum.2025.04.21.org`) are correctly closed when refreshing the agenda, preventing stale markers and state isolation issues.

## 3. Validation Strategy

### 3.1. API Validation
*   **Reproduction:**
    1.  Access the `/scrum/` endpoint to load the agenda.
    2.  Select a task (e.g., "Task A"). Verify `ORG-TASK.html` shows Task A.
    3.  Select a different task (e.g., "Task B") without manually refreshing the agenda.
*   **Verification:** Confirm that `ORG-TASK.html` accurately reflects the content of Task B. If it still shows Task A, the narrowing reset has failed.

## 4. Risks & Constraints
*   **Performance:** The updated regex for buffer killing is slightly more complex but remains efficient for the typical number of open agenda buffers. Correct state management is prioritized over the negligible overhead of the regex match.
