import unittest
import datetime

from orgparse.node import OrgNode
from unittest.mock import MagicMock

from lain.lain_org_utils import OrgTask, OrgThread, ThreadParser, CleaningVisitor, HierarchyVisitor

GMT_MINUS_5 = datetime.timezone(datetime.timedelta(hours=-5))

class ThreadParserTest(unittest.TestCase): 

    parser = ThreadParser()
    cleaning_visitor = CleaningVisitor()
    hierarchy_visitor = HierarchyVisitor()
        
    def test_parser_thread_body_is_multibullet_list(self):
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        #when: 
        threads = ThreadParser.parse_raw("""- <2024-05-18> My test title 1
- <2024-05-19> My test title 2
- <2024-05-20> My test title 3""", task=task, is_root=True)

        #then: 
        self.assertEqual(len(threads), 3)
        self.assertEqual(len(task.threads), 3)

        for i, thread in enumerate(threads):
            self.assertEqual(thread.content, None)
            self.assertEqual(thread.timestamp, None)
            self.assertEqual(thread.raw, f"- <2024-05-{i + 18}> My test title {i + 1}")

    def test_parser_thread_body_is_indented_multibullet_list(self):
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        #when: 
        threads = ThreadParser.parse_raw("""   - <2024-05-18> My test title 1
   - <2024-05-19> My test title 2
   - <2024-05-20> My test title 3""", task=task, is_root=True)
        
        #then: 
        self.assertEqual(len(threads), 3)
        self.assertEqual(len(task.threads), 3)

        for i, thread in enumerate(threads):
            self.assertEqual(thread.content, None)
            self.assertEqual(thread.timestamp, None)
            self.assertEqual(thread.raw, f"- <2024-05-{i + 18}> My test title {i + 1}")

    def test_parser_thread_body_list_is_indented(self):
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        threads = [OrgThread(f"""{"".join([" " for _ in range(i)])}<2024-05-18> My test title""", task=task) for i in range(10)]

        #when:
        for thread in threads:
            self.parser.parse_thread(thread)

        #then: 
        for thread in threads:
            self.assertEqual(thread.content, "My test title")
            self.assertEqual(thread.timestamp, datetime.datetime(2024, 5, 18, tzinfo=GMT_MINUS_5))

    def test_parse_thread_list_with_no_timestamp(self):        
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        threads = [OrgThread(f"""{"".join([" " for _ in range(i)])}No timestamp""", task=task) for i in range(10)]

        #when:
        for thread in threads:
            self.parser.parse_thread(thread)
            self.cleaning_visitor.visit_org_thread(thread)

        #then: 
        for thread in threads:
            self.assertEqual(thread.content, "No timestamp")
            self.assertIsNone(thread.timestamp)

    def test_parse_org_thread_with_spanish_timestamp(self):        
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        thread = OrgThread("""<2024-05-11 sáb> Spanish timestamp format""", task=task)

        #when: 
        self.parser.parse_thread(thread)

        #then: 
        self.assertEqual(thread.content, "Spanish timestamp format")
        self.assertEqual(thread.timestamp, datetime.datetime(2024, 5, 11, tzinfo=GMT_MINUS_5))

    def test_parse_org_thread_with_code_block(self):        
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        thread = OrgThread("""- <2024-05-11 sáb> 
  \\begin{verbatim}
    def test():
        pass
  \end{verbatim}""", task=task)

        #when: 
        self.parser.parse_thread(thread)

        #then: 
        self.assertEqual(len(thread.children), 0)
        self.assertEqual(thread.content, """\\begin{verbatim}
    def test():
        pass
  \end{verbatim}""")

    def test_parse_org_thread_with_code_block_no_timestamp(self):
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        thread = OrgThread("""- 
  \\begin{verbatim}
  \end{verbatim}""", task=task)

        #when: 
        self.parser.parse_thread(thread)

        #then: 
        self.assertEqual(thread.content, """\\begin{verbatim}
  \end{verbatim}""")
        
        self.assertEqual(len(thread.children), 0)

    def test_parse_org_thread_with_code_block_creates_subthread(self):
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        thread = OrgThread("""- code block
  \\begin{verbatim}
  \end{verbatim}""", task=task)

        #when: 
        self.parser.parse_thread(thread)

        #then: 
        self.assertEqual(thread.content, "code block")  
        self.assertEqual(len(thread.children), 1)

    def test_parse_org_thread_with_nested_thread(self):        
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        thread = OrgThread("""- <2024-05-11> Parent line
  - <2024-05-12> Child line""", task=task)

        #when: 
        self.parser.parse_thread(thread)        
        self.parser.parse_thread(thread.children[0])

        #then: 
        self.assertEqual(thread.content, """Parent line""")
        self.assertEqual(thread.timestamp, datetime.datetime(2024, 5, 11, tzinfo=GMT_MINUS_5))

        self.assertEqual(thread.children[0].content, """Child line""")
        self.assertEqual(thread.children[0].timestamp, datetime.datetime(2024, 5, 12, tzinfo=GMT_MINUS_5))

    def test_parse_org_thread_with_multiline_entry(self):        
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        thread = OrgThread("""<2024-05-20> This is a multiline thread,
  this is still part of the thread content.""", task=task)

        #when: 
        self.parser.parse_thread(thread)
        
        #then:
        self.assertEqual(thread.timestamp, datetime.datetime(2024, 5, 20, tzinfo=GMT_MINUS_5))
        self.assertEqual(thread.content, """This is a multiline thread,
  this is still part of the thread content.""")
        
    def test_parse_org_thread_with_org_properties_logbook(self):
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        #when: 
        threads = ThreadParser.parse_raw(""":LOGBOOK:
CLOCK: [2024-05-14 mar 13:51]--[2024-05-14 mar 17:34] =>  3:43
CLOCK: [2024-05-14 mar 12:46]--[2024-05-14 mar 13:16] =>  0:30
:END:
- <2024-05-18> line 1""", task=task, is_root=True)
        thread = threads[0]
        self.parser.parse_thread(thread)
        
        #then:
        self.assertEqual(thread.timestamp, datetime.datetime(2024, 5, 18, tzinfo=GMT_MINUS_5))
        self.assertEqual(thread.content, "line 1")

    def test_parse_org_thread_with_org_properties_schedule_check(self):
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        #when: 
        threads = ThreadParser.parse_raw(
        """- State "CANCELED"       from "CHECK"      [2023-11-12 Sun 08:12]
- <2024-05-18> line 1""", task=task)
        thread = threads[0]
        self.parser.parse_thread(thread)

        #then:
        self.assertEqual(thread.timestamp, datetime.datetime(2024, 5, 18, tzinfo=GMT_MINUS_5))
        self.assertEqual(thread.content, "line 1")

    def test_parse_org_empty_thread_logbook(self):
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        #when: 
        threads = ThreadParser.parse_raw(""":LOGBOOK:
:END:""", task=task)
        
        #then: 
        self.assertEqual(threads, None)

    def test_parse_org_thread_with_nested_thread_and_org_properties(self):        
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        threads = ThreadParser.parse_raw(""":LOGBOOK:
:END:
- <2024-05-11> Parent line
  - <2024-05-12> Child line""", task=task, is_root=True)

        #when: 
        self.parser.parse_thread(threads[0])

        #then: 
        self.assertEqual(len(threads[0].children), 1)

    def test_parse_org_empty_thread(self):
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        thread = OrgThread("""      """, task=task)

        #when: 
        self.parser.parse_thread(thread)
        
        #then:
        self.assertEqual(thread.timestamp, None)
        self.assertEqual(thread.content, None)

    def test_parse_org_thread_with_nested_thread_with_no_timestamp(self):        
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        threads = ThreadParser.parse_raw("""
    - <2024-05-17 vie> Line 1
      - Line 2
        Line 3
        Line 4
        - Line 5
          Line 6""", task=task, is_root=True)

        #when: 
        parent_thread = threads[0]
        ## There is no need to call the parser for the subthreads, it will be called by the parent thread due to the visitor
        self.parser.parse_thread(parent_thread)

        child_thread = parent_thread.children[0]
        self.parser.parse_thread(child_thread)

        #then: 
        self.assertEqual(parent_thread.content, "Line 1")

        self.assertEqual(child_thread.content, """Line 2
        Line 3
        Line 4""")
        self.assertEqual(len(child_thread.children), 1)

    def test_timestamp_inheritance_cascading(self):
        # Requirement: Cascading inheritance, priority, and fallback to None
        node = MagicMock(spec=OrgNode)
        node.heading = "TASK TITLE"
        task = OrgTask(node)
        task.id = "TASK_ID"

        # Structure:
        # - <2025-01-01> Root (explicit)
        #   - Child 1 (no timestamp, should inherit 2025-01-01)
        #     - Grandchild 1 (no timestamp, should inherit 2025-01-01)
        #   - Child 2 (<2025-01-02> explicit, should override)
        #     - Grandchild 2 (no timestamp, should inherit 2025-01-02)

        root = OrgThread("- <2025-01-01> Root", task)
        c1 = OrgThread("- Child 1", task, parent=root)
        gc1 = OrgThread("- Grandchild 1", task, parent=c1)
        c2 = OrgThread("- <2025-01-02> Child 2", task, parent=root)
        gc2 = OrgThread("- Grandchild 2", task, parent=c2)

        root.add_child(c1)
        c1.add_child(gc1)
        root.add_child(c2)
        c2.add_child(gc2)

        # Run visitors
        for t in [root, c1, gc1, c2, gc2]:
            self.cleaning_visitor.visit_org_thread(t)

        # Assertions
        expected_root = datetime.datetime(2025, 1, 1, tzinfo=GMT_MINUS_5)
        expected_c2 = datetime.datetime(2025, 1, 2, tzinfo=GMT_MINUS_5)

        self.assertEqual(root.timestamp, expected_root)
        self.assertEqual(c1.timestamp, expected_root, "Child 1 should inherit root timestamp")
        self.assertEqual(gc1.timestamp, expected_root, "Grandchild 1 should inherit from Child 1 (cascading)")
        self.assertEqual(c2.timestamp, expected_c2, "Child 2 should have its own explicit timestamp")
        self.assertEqual(gc2.timestamp, expected_c2, "Grandchild 2 should inherit from Child 2 (priority)")

    def test_fallback_to_none_in_ancestry(self):
        # Requirement: Fallback to None if no timestamp in ancestry
        node = MagicMock(spec=OrgNode)
        node.heading = "TASK TITLE"
        task = OrgTask(node)
        task.id = "TASK_ID"

        root = OrgThread("- Root no timestamp", task)
        child = OrgThread("- Child no timestamp", task, parent=root)
        root.add_child(child)

        # Run visitors
        self.cleaning_visitor.visit_org_thread(root)
        self.cleaning_visitor.visit_org_thread(child)

        self.assertIsNone(root.timestamp, "Root with no timestamp in ancestry should have None timestamp")
        self.assertIsNone(child.timestamp, "Child with no timestamp in ancestry should have None timestamp")

    def test_parse_org_thread_with_links_and_nested_thread(self):        
        #given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)

        #when: 
        threads = ThreadParser.parse_raw("""    - <2024-05-15 mié> Line 1
      Line 2
      Line 3
      Line 3.
    - <2024-05-14 mar> [[some.org::title a]]
      - <2024-05-14 mar> https://test1.com
    - <2024-05-14 mar> [[some.org::title b]]
      - <2024-05-13 mar> https://test2.com
      - <2024-05-14 mar> https://test3.com
        - Line 4
          Line 5
      - <2024-05-14 mar>
        - https://test4.com""", task=task, is_root=True)

        self.parser.parse_thread(threads[0])
        self.parser.parse_thread(threads[1])
        self.parser.parse_thread(threads[2])

        self.parser.parse_thread(threads[1].children[0])
        self.parser.parse_thread(threads[2].children[0])
        self.parser.parse_thread(threads[2].children[1])
        self.parser.parse_thread(threads[2].children[2])

        self.parser.parse_thread(threads[2].children[1].children[0])
        self.parser.parse_thread(threads[2].children[2].children[0])

        #then:

        # First thread
        self.assertEqual(threads[0].timestamp, datetime.datetime(2024, 5, 15, tzinfo=GMT_MINUS_5))
        self.assertEqual(threads[0].content, """Line 1
      Line 2
      Line 3
      Line 3.""")
        
        # Second thread
        self.assertEqual(threads[1].timestamp, datetime.datetime(2024, 5, 14, tzinfo=GMT_MINUS_5))
        self.assertEqual(threads[1].content, "[[some.org::title a]]")

        self.assertEqual(len(threads[1].children), 1)
        self.assertEqual(threads[1].children[0].timestamp, datetime.datetime(2024, 5, 14, tzinfo=GMT_MINUS_5))
        self.assertEqual(threads[1].children[0].content, "https://test1.com")

        # Third thread
        self.assertEqual(threads[2].timestamp, datetime.datetime(2024, 5, 14, tzinfo=GMT_MINUS_5))
        self.assertEqual(threads[2].content, "[[some.org::title b]]")

        self.assertEqual(len(threads[2].children), 3)
        self.assertEqual(threads[2].children[0].timestamp, datetime.datetime(2024, 5, 13, tzinfo=GMT_MINUS_5))
        self.assertEqual(threads[2].children[0].content, "https://test2.com")

        self.assertEqual(threads[2].children[1].timestamp, datetime.datetime(2024, 5, 14, tzinfo=GMT_MINUS_5))
        self.assertEqual(threads[2].children[1].content, "https://test3.com")

        self.assertEqual(len(threads[2].children[1].children), 1)
        self.assertEqual(threads[2].children[1].children[0].content, "Line 4\n          Line 5")

        self.assertEqual(threads[2].children[2].timestamp, datetime.datetime(2024, 5, 14, tzinfo=GMT_MINUS_5))
        self.assertEqual(threads[2].children[2].content, "")

        self.assertEqual(threads[2].children[2].children[0].content, "https://test4.com")

    def test_hierarchy_calculation(self):
        from lain.lain_org_utils import HierarchyVisitor
        # given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"
        task = OrgTask(node)
        task.id = "TASK_ID"

        # A nested structure:
        # - Parent
        #   - Child
        parent_thread = OrgThread("- Parent", task)
        child_thread = OrgThread("- Child", task, parent=parent_thread)
        parent_thread.add_child(child_thread)
        
        # We need to simulate the cleaning visitor setting content
        parent_thread.content = "Parent"
        child_thread.content = "Child"

        visitor = HierarchyVisitor()
        visitor.visit_org_task(task)
        parent_thread.accept(visitor)

        # then:
        self.assertIsNotNone(parent_thread.node_id)
        self.assertEqual(parent_thread.parent_id, "TASK_ID")
        self.assertEqual(parent_thread.thread_id, parent_thread.node_id)
        self.assertEqual(parent_thread.message_priority, 0)

        self.assertIsNotNone(child_thread.node_id)
        self.assertEqual(child_thread.parent_id, parent_thread.node_id)
        self.assertEqual(child_thread.thread_id, parent_thread.node_id)
        self.assertEqual(child_thread.message_priority, 0)

    def test_stable_id_generation(self):
        from lain.lain_org_utils import HierarchyVisitor
        # given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"
        task = OrgTask(node)
        task.id = "TASK_ID"

        def create_and_visit():
            parent = OrgThread("- Parent", task)
            parent.content = "Parent"
            visitor = HierarchyVisitor()
            visitor.visit_org_task(task)
            parent.accept(visitor)
            return parent.node_id

        # when:
        id1 = create_and_visit()
        id2 = create_and_visit()

        # then:
        self.assertEqual(id1, id2)

    def test_shared_thread_id_deep_nesting(self):
        from lain.lain_org_utils import HierarchyVisitor
        # given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"
        task = OrgTask(node)
        task.id = "TASK_ID"

        # - A
        #   - A1
        #     - A1a
        thread_a = OrgThread("- A", task)
        thread_a.content = "A"
        thread_a1 = OrgThread("- A1", task, parent=thread_a)
        thread_a1.content = "A1"
        thread_a1a = OrgThread("- A1a", task, parent=thread_a1)
        thread_a1a.content = "A1a"
        
        thread_a.add_child(thread_a1)
        thread_a1.add_child(thread_a1a)

        visitor = HierarchyVisitor()
        visitor.visit_org_task(task)
        thread_a.accept(visitor)

        # then:
        self.assertEqual(thread_a.thread_id, thread_a.node_id)
        self.assertEqual(thread_a1.thread_id, thread_a.node_id)
        self.assertEqual(thread_a1a.thread_id, thread_a.node_id)

    def test_sibling_priority(self):
        from lain.lain_org_utils import HierarchyVisitor
        # given:
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"
        task = OrgTask(node)
        task.id = "TASK_ID"

        # - Sibling 0
        # - Sibling 1
        s0 = OrgThread("- S0", task)
        s0.content = "S0"
        s1 = OrgThread("- S1", task)
        s1.content = "S1"
        
        task.threads = [s0, s1]

        visitor = HierarchyVisitor()
        visitor.visit_org_task(task)
        s0.accept(visitor)
        s1.accept(visitor)

        # then:
        self.assertEqual(s0.message_priority, 0)
        self.assertEqual(s1.message_priority, 1)
        self.assertNotEqual(s0.node_id, s1.node_id)


