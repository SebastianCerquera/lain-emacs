# TDD (Test-Driven-Development) automated feature development

## Methodology

* Leverage red-green refactor and test pyramid, start by adding unit tests, update the code to pass the unit tests, then validate using an integration test.

* Leverage SOLID and OOP, striving to produce highly testeable code.

* DO NOT use print statements, always leverage the logging module.

* DO NOT use pdb or any form of interactive debugger.

## Context 

@src/lain/lain_org_utils.py:

The hierarchical nature of the agenda is getting lost. Currently, individual bullet points (threads) are ingested as independent documents. While they share a `task_id`, the relationship between parent and child bullets, as well as the relative order of siblings, is not preserved in OpenSearch.

To reconstruct the agenda entries as they appear in the original `.org` files, the following fields must be added to each indexed thread document:

### 1. **`node_id`** (The Unique Identifier)
*   **Definition**: A unique, stable "primary key" for every individual bullet point.
*   **Calculation**: A hash (e.g., SHA-256, first 12 chars) of the unique path: `task_id + parent_node_id + sibling_index + content_hash`.
*   **Role**: It provides the granular identity needed to build a tree structure. It serves as the target for the `parent_id` field of its children.
*   **Stability**: Because it is a hash of content and position, it remains stable across re-ingests, allowing OpenSearch to overwrite documents instead of creating duplicates.

### 2. **`parent_id`** (The Hierarchy Link)
*   **Definition**: The `node_id` of the direct parent thread.
*   **Calculation**: If the thread is at the root of the task's body, `parent_id` is the `task_id`. Otherwise, it is the `node_id` of the bullet one level up.
*   **Role**: Enables recursive reconstruction of the tree by asking: "Who are the children of this `node_id`?"

### 3. **`thread_id`** (The Grouping Identifier)
*   **Definition**: The `node_id` of the top-most thread in the current list hierarchy (the "root" of the conversation).
*   **Role**: Unlike `node_id`, which is unique to every bullet, `thread_id` is **shared** by every bullet in a specific tree. It allows fetching an **entire nested conversation** (all levels deep) in a single OpenSearch query (`thread_id: ID_A`) rather than performing multiple recursive queries.

### 4. **`message_priority`** (The Sibling Order)
*   **Definition**: An integer representing the order of the thread among its siblings (0-indexed).
*   **Role**: Ensures that when siblings are retrieved, they can be sorted to match the original document order.

---

### Comparison: `node_id` vs. `thread_id`

| Feature | `node_id` | `thread_id` |
| :--- | :--- | :--- |
| **Uniqueness** | Unique to every single bullet. | Shared by all bullets in the same tree. |
| **Primary Job** | Identification (Who am I?). | Grouping (Which tree do I belong to?). |
| **Reconstruction** | Used to link Parent -> Child. | Used to fetch the entire tree in 1 query. |

### Relationship Example

For a nested list:
*   Bullet A
    *   Bullet A1
        *   Bullet A1.a

| Bullet | `node_id` | `thread_id` | `parent_id` | `message_priority` |
| :--- | :--- | :--- | :--- | :--- |
| **A** | `ID_A` | **`ID_A`** | `TASK_ID` | 0 |
| **A1** | `ID_A1` | **`ID_A`** | `ID_A` | 0 |
| **A1.a** | `ID_A1a` | **`ID_A`** | `ID_A1` | 0 |

---

## Task

1) **Enhance `OrgThread`**: Update the `OrgThread` class to support `node_id`, `parent_id`, `thread_id`, and `message_priority`.
2) **Update `to_json`**: Ensure `OrgThread.to_json()` includes these new fields.
3) **Implement Hierarchy Calculation**: Update the parsing or visiting logic (e.g., in `ThreadParser` or a new `HierarchyVisitor`) to calculate these fields during the parsing of the org body.
4) **Update OpenSearch Mappings**: Update `OrgDatabase.index_settings` to include the new fields as `keyword` types for precise searching.
5) **Update Unit Tests**: Add test cases to `test/unit/test_thread_parser.py` that verify:
    *   Unique `node_id` generation.
    *   Shared `thread_id` across nested levels.
    *   Correct `parent_id` linking.
    *   Stable ID generation on identical content.
6) **Run Unit Suite**:
```bash
python -m unittest discover -s test/unit -p "test_*.py"
```
7) **Update Integration Tests**: Update `test/integration/test_opensearch_client.py` to verify that an entire tree can be retrieved using a single `thread_id` query and reconstructed in memory.
8) **Run Integration Suite**:
```bash
python -m unittest discover -s test/integration -p "test_*.py"
```
