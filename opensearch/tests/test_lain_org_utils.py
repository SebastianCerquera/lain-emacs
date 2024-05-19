import unittest
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
        self.assertEqual(org_file.path, self.file_path)

    def test_parse_creates_org_tasks(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.tasks), 2)
        self.assertIsInstance(org_file.tasks[0], OrgTask)
        self.assertEqual(org_file.tasks[0].title, "TITLE")

    def test_parse_creates_child_task(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when:
        parent_task = org_file.tasks[0]
        child_task = org_file.tasks[1]
        
        # then:
        self.assertEqual(parent_task, child_task.parent)

    def test_parse_creates_org_thread(self):
        # given:
        org_file = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.tasks[1].threads), 1)
        self.assertIsInstance(org_file.tasks[1].threads[0], OrgThread)
        self.assertEqual(org_file.tasks[1].threads[0].content, "   - <2024-05-18> My test title 2")