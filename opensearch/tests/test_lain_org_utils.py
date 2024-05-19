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
        self.assertEqual(org_file.root.threads[0].raw, """  - <2024-05-18> My test title
    - <2024-05-19> My test title 3""")
        self.assertEqual(org_file.root.threads[0].timestamp, datetime.datetime(2024, 5, 18))
        self.assertEqual(org_file.root.threads[0].content, "My test title") 
        self.assertEqual(org_file.root.threads[1].content, "My test title 3") 

    def test_parse_creates_second_level_tasks_org_thread(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.root.children[0].threads), 1)
        self.assertIsInstance(org_file.root.children[0].threads[0], OrgThread)
        self.assertEqual(org_file.root.children[0].threads[0].raw, "   - <2024-05-18> My test title 2")
        self.assertEqual(org_file.root.children[0].threads[0].content, "My test title 2") 

    def test_parse_creates_empty_second_level_tasks_org_thread(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.root.children[1].threads), 0)