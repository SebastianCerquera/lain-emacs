# Lesson 2: Data Privacy and Methodology Adherence

## Context
During the resolution of the "Thread Parsing Integrity" (Hyphen Splitting) bug, the agent failed twice to adhere to the core engineering standards:
1.  **Initial Methodology Drift:** The agent attempted to verify the fix using a side-channel reproduction script and autonomous validation reports rather than first codifying the requirement and integrating failing tests into the permanent regression suite.
2.  **Data Privacy Breach:** When finally integrating tests into the regression suite, the agent used raw, sensitive data from the `SMALL/LAIN` dataset (containing specific locations, file paths, and personal task details) directly in the source code.

## Key Methodology Principles

### 1. Data Anonymization (The "Zero-Leak" Rule)
**Lesson:** Never copy raw data from grounded "real-world" datasets (like `SMALL/`) into permanent source files, tests, or documentation.
- **Reasoning:** Grounding material often contains PII (Personally Identifiable Information) or private metadata. Source code is permanent and often shared; it must contain only generic, synthetic, or anonymized examples.
- **Action:** Identify the *structural* trigger of a bug (e.g., a hyphen in a link) and recreate it using synthetic placeholders (e.g., `[[path/file.org::TAG, some-location]]`) before saving to disk.

### 2. Methodology Over Velocity
**Lesson:** The pressure to reach "Green" or "Finality" must never bypass the Research -> Strategy -> Execution lifecycle.
- **Action:** Even for "obvious" bugs, the sequence MUST be:
    1.  Document Requirement (save to `requirements/`).
    2.  Write Failing Test (in the official `test/` suite).
    3.  Implement Fix.
    4.  Verify & Anonymize.

### 3. Security as a Primary Acceptance Criterion
**Lesson:** A feature is "Materially Complete" ONLY if it is behaviorally correct, structurally sound, AND adheres to all security/privacy mandates.
- **Action:** Before declaring a task finished, perform a final "Audit Turn": *Does this change introduce secrets, PII, or fragile dependencies?*

## Summary of the Failure
The agent prioritized the *functional* correctness of the parser over the *security* of the codebase. By treating real-world data as "just a test string," the agent compromised the privacy of the user's dataset. Adherence to the TDD process was only achieved after multiple human interventions, indicating a lack of initial discipline in following the `GEMINI.md` protocol.
