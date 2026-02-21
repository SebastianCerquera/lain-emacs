# Happy Path E2E Acceptance Test

This document provides instructions for validating the "Happy Path" of the Lain-Emacs task view generation using the Chrome MCP server.

## Prerequisites

1.  **Docker:** Ensure Docker is installed and running.
2.  **Chrome MCP:** The `chrome-devtools` skill must be available in your environment.
3.  **Clean State:** No other processes should be using port `8080`.

## 1. Environment Setup

Run the following command to start the container and the Emacs daemon in a clean state:

```bash
docker rm -f lain-e2e || true && 
docker run -d 
  -v $(pwd)/sample_files/.emacs:/root/.emacs 
  -v $(pwd)/sample_files/scrum.org:/home/agentworkstation/sources/lain-emacs/sample_files/scrum.org 
  -v $(pwd)/lain:/root/.emacs.d/lain 
  --net host 
  --name lain-e2e 
  lain-emacs:latest 
  tail -f /dev/null && 
docker exec lain-e2e emacs --daemon -l /root/.emacs
```

Wait approximately 10 seconds for the daemon to initialize. You can verify it is ready by running:
`curl -I http://localhost:8080/base.html` (should return HTTP 200).

## 2. E2E Validation Steps (via Chrome MCP)

### Step 2.1: Navigation and Authentication
1.  **Navigate** to `http://localhost:8080/base.html`.
2.  **Submit Cookie:** Find the element with text "Submit" (usually a button) and click it. This initializes the session and redirects to the Scrum View.

### Step 2.2: Access the Agenda
1.  **Wait** for the text "Scrum View" to appear.
2.  **Click** the "Scrum View" link. This will open `SCRUM.html` which contains the generated Org Agenda.

### Step 2.3: Validate Task 1 (Sequential Test Part 1)
1.  **Wait** for the text "TODO" to appear on the agenda page.
2.  **Identify a Task:** Look for a task line, for example: `eiusmod, 2024-01, 4, dolore, RESPONSABILITIES`.
3.  **Click** the "TODO" span associated with that task.
4.  **Verify:** Wait for `ORG-TASK.html` to load. It should contain the content of the "eiusmod" task and the action buttons (Check task, I tried, etc.).

### Step 2.4: Validate Task 2 (Sequential Test Part 2)
1.  **Navigate Back:** Use `navigate_page` to go back to `http://localhost:8080/SCRUM.html`.
2.  **Identify a Different Task:** Look for another task, for example: `labore, sit, 2023-06, 5, adisciping, RESPONSABILITIES`.
3.  **Click** the "TODO" span for this new task.
4.  **Verify Update:** Wait for `ORG-TASK.html` to load. **Crucially**, it must now show the content for the "labore" task. If it still shows "eiusmod", the fix for buffer narrowing has failed.

## 3. Expected Results

*   **Task Isolation:** Clicking any part of a task line should extract only that specific task name, not the entire agenda.
*   **Narrowing Reset:** Sequential clicks on different tasks must work. The backend must `widen` the buffer before searching for the next task marker.
*   **Special Characters:** Tasks containing commas, brackets, or question marks (e.g., `Task [with] special? characters`) should be correctly identified and displayed.
*   **Persistence:** The click handlers in `SCRUM.html` must remain active after multiple navigations (ensured via event delegation).

## 4. Cleanup

Stop the test environment:
```bash
docker rm -f lain-e2e
```
