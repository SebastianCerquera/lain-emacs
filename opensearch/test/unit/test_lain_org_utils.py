import unittest
import string
from src.lain.lain_org_utils import OrgParser, OrgTask, OrgFile, OrgThread

class TestLainOrgUtilsParse(unittest.TestCase):
    def setUp(self):
        self.utils = OrgParser()
        self.file_path = "test/unit/sample.org"

    def test_parse_creates_org_file(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertIsInstance(org_file, OrgFile)

    def test_parse_creates_org_tasks(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.tasks), 13)
        self.assertIsInstance(org_file.root, OrgTask)
        self.assertEqual(org_file.root.title, "TITLE")

        titles = list(map(lambda e: e.title, org_file.tasks))
        self.assertTrue("TITLE 2" in titles)
        self.assertTrue("TITLE 10" in titles)
        self.assertTrue("TITLE 11" in titles)

    def test_parse_creates_org_task_titles_are_hashed(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        for task in org_file.tasks:
            self.assertIsInstance(task, OrgTask)

            for char in ["<", ">", ",", ".", "-", ":", "(", ")", "*"]:
                self.assertTrue(char not in task.title)
        
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

    def test_parse_parent_thread_dont_duplicate_content(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when:
        parent_task = org_file.root
        
        # then:
        self.assertEqual(parent_task.threads[0].content, "My test title")
        self.assertEqual(parent_task.threads[0].children[0].content, "My test title 3")

    def test_parse_creates_org_thread(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.root.threads), 1)

    def test_parse_creates_second_level_tasks_org_thread(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.root.children[0].threads), 1)
        self.assertIsInstance(org_file.root.children[0].threads[0], OrgThread)

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

    def test_parse_creates_org_task_with_more_than_one_thread(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when
        org_task = org_file.root.children[9]

        # then:
        self.assertEqual(len(org_task.threads), 2)

    def test_parse_creates_org_task_with_complex_thread(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when
        org_task = org_file.root.children[10]

        # then:
        self.assertEqual(len(org_task.threads), 1)

    def test_parse_creates_org_task_with_complex_thread(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when
        org_task = org_file.root.children[11]

        # then:
        self.assertEqual(len(org_task.threads), 2)