# Requirement: Thread Parsing Integrity (Hyphen Splitting Fix)

## Overview
The `ThreadParser` currently incorrectly splits task content into multiple threads when it encounters a hyphen (`-`) within the text, such as inside Org-mode links (e.g., `[[...2020-01...]]`). This behavior breaks data integrity by fragmenting single logical messages.

## Correct Behavior
A hyphen (`-`) should ONLY be interpreted as the start of a new subthread if it meets the following criteria:
1.  **Leading Bullet:** It appears at the very beginning of a newline (optionally preceded by whitespace).
2.  **Indentation Consistency:** It aligns with the expected list/thread indentation structure for the current context.

Hyphens that appear **within a line of text** (e.g., in dates, links, or compound words) MUST be preserved as part of the current thread's `thread_body`.

## Technical Constraints
- The `ThreadParser.parse_thread` and `ThreadParser.parse_raw` methods must be updated to use a more context-aware splitting logic.
- The fix must NOT interfere with existing metadata exclusion rules (e.g., `:LOGBOOK:`, `CLOCK:`, `CLOSED:`).
- The solution must be verified against the existing unit test suite and a new regression test case.

## Acceptance Criteria
1.  **Org Link Integrity:** A thread containing `[[file.org::title-with-hyphen]]` is ingested as a single `thread_body`.
2.  **Subthread Correctness:** Actual bulleted lists (e.g., `\n  - Subtask`) continue to be correctly identified as child threads.
3.  **No Regression:** All existing tests in `opensearch/test/unit/test_thread_parser.py` must pass.
