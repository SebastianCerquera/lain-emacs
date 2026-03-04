# Requirement: Fix Thread Inheritance and Node Identifiers

## Status
**Confirmed**

## Context
The current `ThreadParser` in `lain-emacs` treats bullet points (dashes) as separate nodes. However, it fails to correctly handle metadata inheritance and unique identification for these nodes, leading to data integrity issues in OpenSearch. Specifically, bullet points without explicit timestamps are either incorrectly timestamped with system time or lose their temporal context, and they lack deterministic identifiers for reliable traceability.

## Functional Requirements

### 1. Timestamp Inheritance (Cascading)
- **Rule:** If a node (bullet point starting with `-`) does not have an explicit timestamp (e.g., `<YYYY-MM-DD>`), it must inherit the timestamp from its immediate parent node.
- **Recursion:** This inheritance must cascade upwards through the hierarchy until a node with an explicit timestamp is encountered.
- **Priority:** An explicit timestamp on a node always takes precedence over an inherited value.
- **Fallback:** if no timestamp is found in the entire ancestry (up to the task level), the timestamp must be `None`. The parser must **not** fallback to the current system time.

### 2. Deterministic Node Identifiers (`node_id`)
- **Rule:** Every node (including those identified by dashes) must be assigned a deterministic `node_id`.
- **Uniqueness:** The `node_id` must be unique relative to its position within the file/task structure. 
- **Consistency:** The ID must be stable across re-ingestion if the content and structure remain the same.

### 3. Structural Integrity
- **Bullet Points as Nodes:** The parser must continue to treat bullet points (`-`) as distinct nodes (rather than fragments of a single thread body).
- **Metadata Population:** Ensure each resulting `OrgThread` object correctly populates its `timestamp` (inherited or explicit) and its `node_id` before being indexed into OpenSearch.

## Acceptance Criteria
1. A multi-level nested list without timestamps correctly inherits the timestamp from the root thread of the task.
2. A bullet point with its own timestamp correctly overrides any inherited value.
3. OpenSearch entries for bullet points contain a valid, relative `node_id`.
4. Nodes without any timestamp in their hierarchy result in a `null` or `None` value in OpenSearch, not the current ingestion time.
5. All existing `ThreadParser` tests pass or are updated to reflect these inheritance and ID rules.
