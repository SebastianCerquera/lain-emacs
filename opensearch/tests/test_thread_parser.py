import unittest
import datetime
from lain.lain_org_utils import OrgTask, OrgThread, ThreadParser


class ThreadParserTest(unittest.TestCase): 

    parser = ThreadParser()
        
    def test_parser_thread_body_is_multibullet_list(self):
        #given:
        task = OrgTask(None)

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
            self.assertEqual(thread.raw, f" <2024-05-{i + 18}> My test title {i + 1}")

    def test_parser_thread_body_is_indented_multibullet_list(self):
        #given:
        task = OrgTask(None)

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
            self.assertEqual(thread.raw, f" <2024-05-{i + 18}> My test title {i + 1}")

    def test_parser_thread_body_list_is_indented(self):
        #given:
        task = OrgTask(None)
        threads = [OrgThread(f"""{"".join([" " for _ in range(i)])}<2024-05-18> My test title""", task=task) for i in range(10)]

        #when:
        for thread in threads:
            self.parser.parse_thread(thread)

        #then: 
        for thread in threads:
            self.assertEqual(thread.content, "My test title")
            self.assertEqual(thread.timestamp, datetime.datetime(2024, 5, 18).date())

    def test_parse_thread_list_with_no_timestamp(self):        
        #given:
        task = OrgTask(None)
        threads = [OrgThread(f"""{"".join([" " for _ in range(i)])}No timestamp""", task=task) for i in range(10)]

        #when:
        for thread in threads:
            self.parser.parse_thread(thread)

        #then: 
        for thread in threads:
            self.assertEqual(thread.content, "No timestamp")
            self.assertEqual(thread.timestamp, datetime.datetime.now().date())

    def test_parse_org_thread_with_spanish_timestamp(self):        
        #given:
        task = OrgTask(None)
        thread = OrgThread("""<2024-05-11 sáb> Spanish timestamp format""", task=task)

        #when: 
        self.parser.parse_thread(thread)

        #then: 
        self.assertEqual(thread.content, "Spanish timestamp format")
        self.assertEqual(thread.timestamp, datetime.datetime(2024, 5, 11).date())

    def test_parse_org_thread_with_nested_thread(self):        
        #given:
        task = OrgTask(None)
        thread = OrgThread("""<2024-05-11> Parent line
  - <2024-05-12> Child line""", task=task)

        #when: 
        self.parser.parse_thread(thread)

        #then: 
        self.assertEqual(len(thread.children), 1)

        self.assertEqual(thread.content, """Parent line""")
        self.assertEqual(thread.timestamp, datetime.datetime(2024, 5, 11).date())

        self.assertEqual(thread.children[0].content, """Child line""")
        self.assertEqual(thread.children[0].timestamp, datetime.datetime(2024, 5, 12).date())

    def test_parse_org_thread_with_multiline_entry(self):        
        #given:
        task = OrgTask(None)
        thread = OrgThread("""<2024-05-20> This is a multiline thread,
  this is still part of the thread content.""", task=task)

        #when: 
        self.parser.parse_thread(thread)
        
        #then:
        self.assertEqual(thread.timestamp, datetime.datetime(2024, 5, 20).date())
        self.assertEqual(thread.content, """This is a multiline thread,
  this is still part of the thread content.""")
        
    def test_parse_org_thread_with_org_properties_logbook(self):
        #given:
        task = OrgTask(None)

        #when: 
        threads = ThreadParser.parse_raw(""":LOGBOOK:
:END:
- <2024-05-18> line 1""", task=task, is_root=True)
        thread = threads[0]
        self.parser.parse_thread(thread)
        
        #then:
        self.assertEqual(thread.timestamp, datetime.datetime(2024, 5, 18).date())
        self.assertEqual(thread.content, "line 1")

    def test_parse_org_thread_with_org_properties_schedule_check(self):
        #given:
        task = OrgTask(None)

        #when: 
        threads = ThreadParser.parse_raw(
        """- State "CANCELED"       from "CHECK"      [2023-11-12 Sun 08:12]
- <2024-05-18> line 1""", task=task)
        thread = threads[0]
        self.parser.parse_thread(thread)

        #then:
        self.assertEqual(thread.timestamp, datetime.datetime(2024, 5, 18).date())
        self.assertEqual(thread.content, "line 1")

    def test_parse_org_empty_thread_logbook(self):
        #given:
        task = OrgTask(None)

        #when: 
        threads = ThreadParser.parse_raw(""":LOGBOOK:
:END:""", task=task)
        
        #then: 
        self.assertEqual(threads, None)

    def test_parse_org_thread_with_nested_thread_and_org_properties(self):        
        #given:
        task = OrgTask(None)
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
        task = OrgTask(None)
        thread = OrgThread("""      """, task=task)

        #when: 
        self.parser.parse_thread(thread)
        
        #then:
        self.assertEqual(thread.timestamp, None)
        self.assertEqual(thread.content, None)

    def test_parse_org_thread_with_nested_thread_with_no_timestamp(self):        
        #given:
        task = OrgTask(None)
        threads = ThreadParser.parse_raw("""    - <2024-05-17 vie> Line 1
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

        #then: 
        parent_thread.content = "Line 1"        
        self.assertEqual(len(parent_thread.children), 1)

        self.assertEqual(child_thread.content, """Line 2
        Line 3
        Line 4""")
        self.assertEqual(len(child_thread.children), 1)

    def test_parse_org_thread_with_links_and_nested_thread(self):        
        #given:
        task = OrgTask(None)

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

        #then:
        self.assertEqual(len(threads), 3)

        # First thread
        self.assertEqual(threads[0].timestamp, datetime.datetime(2024, 5, 15).date())
        self.assertEqual(threads[0].content, """Line 1
      Line 2
      Line 3
      Line 3.""")
        
        # Second thread
        self.assertEqual(threads[1].timestamp, datetime.datetime(2024, 5, 14).date())
        self.assertEqual(threads[1].content, "[[some.org::title a]]")

        self.assertEqual(len(threads[1].children), 1)
        self.assertEqual(threads[1].children[0].timestamp, datetime.datetime(2024, 5, 14).date())
        self.assertEqual(threads[1].children[0].content, "https://test1.com")

        # Third thread
        self.assertEqual(threads[2].timestamp, datetime.datetime(2024, 5, 14).date())
        self.assertEqual(threads[2].content, "[[some.org::title b]]")

        self.assertEqual(len(threads[2].children), 3)
        self.assertEqual(threads[2].children[0].timestamp, datetime.datetime(2024, 5, 13).date())
        self.assertEqual(threads[2].children[0].content, "https://test2.com")

        self.assertEqual(threads[2].children[1].timestamp, datetime.datetime(2024, 5, 14).date())
        self.assertEqual(threads[2].children[1].content, "https://test3.com")

        self.assertEqual(len(threads[2].children[1].children), 1)
        self.assertEqual(threads[2].children[1].children[0].content, "Line 4\n          Line 5")

        self.assertEqual(threads[2].children[2].timestamp, datetime.datetime(2024, 5, 14).date())
        self.assertEqual(threads[2].children[2].content, "")

        self.assertEqual(len(threads[2].children[2].children), 1)
        self.assertEqual(threads[2].children[2].children[0].content, "https://test4.com")


