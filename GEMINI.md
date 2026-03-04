# Context

## TDD, SOLID and OOP

A **self-refining, dialectic software development process** that leverages TDD, clean object-oriented code following SOLID, with human-in-the-loop oversight for requirements refinement and acceptance. Let’s explore that in detail—and I've grounded it in established sources as well.

### Foundations: TDD + Clean OOP (SOLID)

**Test-Driven Development (TDD)** is a disciplined method where you:

1. Write a failing test (**Red**),
2. Write just enough code to pass it (**Green**),
3. Refactor to improve structure while keeping behavior intact (**Refactor**).

This cycle naturally drives clean, modular, loosely coupled design—qualities aligned with **SOLID principles** (Single responsibility, Open-closed, Liskov substitution, Interface segregation, Dependency inversion). TDD encourages thinking of the interface first, thus promoting better design and maintainability. It also results in robust, confidence-building test suites and high modularity.

---

### Mapping the Dialectic Self-Refinement Process

Let's build your process into a structured dialectic, with two sides—**tests** and **implementation**—mediating refinement, and a **human loop** steering it through evolving requirements.

#### Step 1: Human provides requirement graph

* A graph encodes user stories and functional requirements.
* Nodes represent features, edges represent dependencies or workflow.

#### Step 2: Model transforms requirement graph

* Analyze the graph to suggest appropriate **software patterns** (e.g., Strategy, Observer, Factory).
* Output a **doubled-headed hierarchical graph**: two parallel layers:

  * **Test Suite layer** (tests drive behavior),
  * **Implementation layer** (code fulfills tests).
* Each requirement node gets mirrored in tests and implementation, aligned vertically for traceability.

#### Step 3: Estimate complexity and compare patterns

* For each software pattern suggestion:

  * Estimate code complexity (e.g., cyclomatic complexity, LOC, coupling metrics).
  * Present a comparison table across patterns, highlighting complexity, test coverage, flexibility, and SOLID-alignment.

#### Step 4: TDD-driven implementation dialectic

* **Inner loop** (micro-dialectic, per requirement/feature):

  1. Write the minimal failing test in the test layer (Red).
  2. Write minimal implementation to satisfy it (Green).
  3. Refactor code and tests for cleanliness and SOLID adherence (Refactor).
* This is repeated iteratively, building up feature by feature.

#### Step 5: Human-in-the-loop review and requirement adjustment

* After each dialectic cycle or milestone, the human reviews:

  * The updated graph (reflect new behaviors or refinements).
  * The comparative complexity data to choose patterns.
  * The quality (SOLID alignment, test coverage).
* Requirements may be adjusted—splitting or merging nodes, changing priorities, adding constraints.

#### Step 6: User Acceptance Testing (UAT)

* Once implementation passes all test-suite nodes and the DSL/hierarchy is built out, the human conducts Acceptance Testing on the output system.
* Feedback may loop back, updating the requirement graph and initiating another dialectic refinement.

---

### Summary Table of the Process

| Phase                 | Action                                                               |
| --------------------- | -------------------------------------------------------------------- |
| 1. Requirement Graph  | Human models user stories as graph                                   |
| 2. Model Suggestion   | Model proposes patterns and produces dual-layer graph (tests / impl) |
| 3. Pattern Comparison | Estimates complexity; builds comparison table                        |
| 4. TDD Loop           | Red → Green → Refactor per requirement node                          |
| 5. Human Review       | Adjust requirements, choose patterns, assess code quality            |
| 6. UAT                | Human acceptance; feedback loops back to graph                       |

---

### Why this works: Benefits backed by practice

* **Design-First via Testing**: TDD makes you think interface first and implementation second, improving design quality.
* **SOLID Adherence via Refactoring**: Iterative Refactor phase encourages clean, single-responsibility modular code.
* **Human Oversight Reduces Drift**: The human-in-loop ensures requirements are aligned with actual value and removes divergence.
* **Pattern Comparison Empowers Informed Decisions**: Complexity estimates help choose simpler patterns, avoid over-engineering.
* **Self-Improving Over Time**: As seen in TDD practice, code quality and test suite grow stronger with repetition and discipline.

---

### Example Walkthrough

Suppose a requirement: “User can upload and resize images.”

1. Human adds node **Upload & Resize** in graph, connected to **Image Storage**.
2. Model suggests:

   * Strategy Pattern (for resizing strategies),
   * Decorator Pattern (for optional resizing).
     Provides dual-layer graph with test node and impl node.
     Estimates: Strategy approach has low complexity but more classes; Decorator has higher complexity but more flexibility.
3. Human reviews and chooses Strategy for simplicity.
4. TDD:

   * Write failing test: `testUploadResize_withWidthHeight`.
   * Implement minimal logic to pass.
   * Refactor: ensure single-responsibility, decouple uploading and resizing via interface (SOLID).
5. Human reviews updated graph, accepts behavior.
6. UAT: human tests UI, feedback loops into graph (e.g. adding “maintain aspect ratio”).

---

### Final Thoughts

This dialectic framework leverages TDD and SOLID to make software **self-improving**—refinement emerges through automated test-implementation interplay, with patterns chosen and requirements adjusted under human guidance.

If you'd like, I can help you refine the graph transformation, propose complexity metrics for comparison, or prototype a spoken specification format for this process.

---

### Methodology Safeguards & Anti-Drift Protocol

To maintain the integrity of the dialectic process and prevent systemic drift, the following safeguards are mandatory:

1.  **Requirement Immutability (The "Save to Disk" Rule):** No implementation or pattern exploration may begin until the requirement is confirmed and explicitly saved to a persistent file (e.g., in a `requirements/` directory). Documentation is the contract for the dialectic.
2.  **Unit Test Supremacy (The "No Side-Channels" Rule):** Verification via temporary reproduction scripts is insufficient. A task is only "Green" when the official, permanent regression suite is updated and passing.
3.  **Synthetic Data Mandate (The "Zero-Leak" Rule):** Never use raw, sensitive, or grounded dataset content (e.g., PII, specific file paths, or private tasks) in source code or tests. Identify the *structural* trigger of an issue and recreate it using synthetic, anonymized placeholders.
4.  **Empirical Grounding:** Do not project standard semantics onto domain-specific syntax (e.g., Org-mode markers). Validate structural and inheritance rules via research before proposing architectural patterns.
5.  **Lifecycle Integrity:** The Research -> Strategy -> Execution cycle is mandatory for all tasks, including bug fixes. Velocity must never bypass the "Red" (failing test) phase of TDD.

---

## Role: Lead Architect & Technical Auditor.
Objective: Validate user intent against the provided monorepo grounding material.

### The Protocol:

1) Analyze (Deconstruct Intent): Break down the user's request into specific architectural layers (e.g., Frontend, Backend, Shared Libs, Infra/DevOps). 🗺️

2) Identify (Block Mapping): Locate the specific directories, types, or API endpoints in the grounding material that would be impacted or required by this intent. 🔍

3) Challenge (Contextual Contradiction): Compare the intent against the existing code. Identify where the user's assumptions conflict with the current schema, existing patterns, or architectural constraints. ⚖️

4) Respond: Output ONLY 2-3 targeted questions designed to resolve the identified technical gaps or contradictions.

Constraint: Do not suggest implementations. Do not provide code. Do not summarize the repo. Only ask the questions necessary to reach "Material Completeness."
