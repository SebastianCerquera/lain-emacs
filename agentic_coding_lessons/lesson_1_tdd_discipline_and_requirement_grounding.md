# Lesson 1: TDD Discipline and Requirement Grounding

## Context
During the implementation of the "Thread Inheritance and Node Identifiers" requirement, the agent initially jumped to exploration and implementation without fully confirming the requirement or saving it to disk. This led to a violation of the "self-refining, dialectic software development process" outlined in the project's `GEMINI.md`.

## Key Methodology Principles

### 1. Requirement Immutability (The "Save to Disk" Rule)
**Lesson:** Never begin coding, exploring, or assuming structural patterns until the requirement is confirmed by the human-in-the-loop AND explicitly saved to disk (e.g., in a `requirements/` directory).
- **Reasoning:** Verbal confirmation is insufficient. Documentation acts as the "contract" for the dialectic process.

### 2. Deep Structural Understanding
**Lesson:** Do not assume a symbol (like a dash `-`) is just formatting. In Org-mode and this specific API, a dash represents a `node_id` with specific inheritance rules.
- **Action:** Ask clarifying questions about node identity and metadata inheritance before touching the parser logic.

### 3. TDD is Not Just Reproduction
**Lesson:** Passing a temporary reproduction script is NOT the same as completing a task.
- **Action:** Every new requirement must be integrated into the **permanent regression suite** (e.g., `opensearch/test/unit/test_thread_parser.py`).
- **Standard:** A task is only "Green" when the existing unit tests are updated to reflect the new contract and all tests pass within the official test runner.

### 4. Avoiding Context Poisoning
**Lesson:** Do not blindly follow previous autonomous reports (like validation reports) if they contain stale or incorrect information.
- **Action:** Verify each item of a previous report with the user. Only act on "Material Completeness" confirmed in the current session.

## Summary of the Dialectic Failure
The agent attempted to reach "Green" via a side-channel (repro script) rather than evolving the core test suite. This "shortcut" bypasses the **Refactor** phase of TDD where the codebase's structural integrity is maintained.
