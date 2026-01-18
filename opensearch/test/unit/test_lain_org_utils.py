import unittest
import string
import os
import tempfile
from src.lain.lain_org_utils import OrgParser, OrgTask, OrgFile, OrgThread, OrgFileDiscovery

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


class TestOrgFileDiscovery(unittest.TestCase):
    def setUp(self):
        self.test_dir = tempfile.mkdtemp()
        self.org_file1 = os.path.join(self.test_dir, "file1.org")
        self.org_file2 = os.path.join(self.test_dir, "file2.org")
        self.txt_file = os.path.join(self.test_dir, "file3.txt")

        with open(self.org_file1, "w") as f:
            f.write("* Task 1")
        with open(self.org_file2, "w") as f:
            f.write("* Task 2")
        with open(self.txt_file, "w") as f:
            f.write("Just some text")

    def tearDown(self):
        os.remove(self.org_file1)
        os.remove(self.org_file2)
        os.remove(self.txt_file)
        os.rmdir(self.test_dir)

    def test_discover_files_finds_org_files(self):
        # given:
        expected_files = sorted([self.org_file1, self.org_file2])

        # when:
        discovered_files = sorted(OrgFileDiscovery.discover_files(self.test_dir))

        # then:
        self.assertEqual(discovered_files, expected_files)

    def test_discover_files_ignores_non_org_files(self):
        # when:
        discovered_files = OrgFileDiscovery.discover_files(self.test_dir)

        # then:
        self.assertNotIn(self.txt_file, discovered_files)

    def test_discover_files_empty_directory(self):
        # given:
        empty_dir = tempfile.mkdtemp()

        # when:
        discovered_files = OrgFileDiscovery.discover_files(empty_dir)

        # then:
        self.assertEqual(discovered_files, [])

        # cleanup:
        os.rmdir(empty_dir)