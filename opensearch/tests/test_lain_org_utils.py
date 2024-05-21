import unittest
import datetime
from lain.lain_org_utils import OrgParser, OrgTask, OrgFile, OrgThread

class TestLainOrgUtilsParse(unittest.TestCase):
    def setUp(self):
        self.utils = OrgParser()
        self.file_path = "tests/sample.org"

    def test_parse_creates_org_file(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertIsInstance(org_file, OrgFile)

    def test_parse_creates_org_tasks(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.tasks), 10)
        self.assertIsInstance(org_file.root, OrgTask)
        self.assertEqual(org_file.root.title, "TITLE")

        titles = list(map(lambda e: e.title, org_file.tasks))
        self.assertTrue("TITLE 2" in titles)
        self.assertTrue("TITLE 10" in titles)
        
    def test_parse_creates_org_task_that_are_setted(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        for task in org_file.tasks:
            self.assertIsInstance(task, OrgTask)
            self.assertTrue(task.title is not None)
            self.assertTrue(task.org_node is not None)

    def test_parse_creates_child_task(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when:
        parent_task = org_file.root
        child_task = parent_task.children[0]
        
        # then:
        self.assertEqual(parent_task, child_task.parent)

    def test_parse_creates_org_thread(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.root.threads), 2)
        self.assertIsInstance(org_file.root.threads[0], OrgThread)
        self.assertEqual(org_file.root.threads[0].raw, """- <2024-05-18> My test title
    - <2024-05-19> My test title 3""")
        self.assertEqual(org_file.root.threads[0].timestamp, datetime.datetime(2024, 5, 18))
        self.assertEqual(org_file.root.threads[0].content, "My test title\n     My test title 3") 
        self.assertEqual(org_file.root.threads[1].content, "My test title 3") 

    def test_parse_creates_second_level_tasks_org_thread(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.root.children[0].threads), 1)
        self.assertIsInstance(org_file.root.children[0].threads[0], OrgThread)
        self.assertEqual(org_file.root.children[0].threads[0].raw, "- <2024-05-18> My test title 2")
        self.assertEqual(org_file.root.children[0].threads[0].content, "My test title 2") 

    def test_parse_creates_empty_second_level_tasks_org_thread(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        org_task = org_file.root.children[2]

        # when, then:
        self.assertEqual(len(org_task.threads), 2)
        self.assertEqual(org_task.threads[0].timestamp, datetime.datetime(2024, 5, 19))
        self.assertEqual(org_task.threads[1].timestamp, datetime.datetime(2024, 5, 19))

    def test_parse_creates_empty_task(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when:
        task = org_file.root.children[7]
     
        # then:
        self.assertEqual(len(task.threads), 0)
        
    def test_parse_org_thread_non_lain_entry(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.root.children[3].threads), 0)

    def test_parse_org_thread_with_multiline_entry(self):        
        # given:
        org_file = self.utils.parse(self.file_path)

        # when
        org_task = org_file.root.children[4]
        
        #then:
        self.assertEqual(len(org_task.threads), 1)
        self.assertEqual(org_task.threads[0].timestamp, datetime.datetime(2024, 5, 20))
        self.assertEqual(org_task.threads[0].raw, """- <2024-05-20> This is a multiline thread,
      this is still part of the thread content.""")
        self.assertEqual(org_task.threads[0].content, """This is a multiline thread,
      this is still part of the thread content.""")
        
    def test_parse_org_thread_with_no_time_stamp(self):        
        # given:
        org_file = self.utils.parse(self.file_path)

        # when
        org_task = org_file.root.children[5]
        
        #then:
        self.assertEqual(len(org_task.threads), 1)
        self.assertEqual(org_task.threads[0].raw, "- No timestamp")
        self.assertEqual(org_task.threads[0].content, "No timestamp")

        self.assertEqual(org_task.threads[0].timestamp, datetime.datetime.now().date())