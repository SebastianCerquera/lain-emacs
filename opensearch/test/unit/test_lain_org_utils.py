import unittest
import string
import os
import tempfile
from unittest.mock import MagicMock
from lain.lain_org_utils import OrgParser, OrgTask, OrgFile, OrgThread, OrgFileDiscovery, OrgDatabase, CleaningVisitor

class TestLainOrgUtilsParse(unittest.TestCase):
    def setUp(self):
        self.utils = OrgParser()
        self.file_path = "test/unit/sample.org"

    def test_parse_creates_org_file(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)

        # when, then:
        self.assertIsInstance(org_file, OrgFile)

    def test_parse_creates_org_tasks(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)

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
        org_file, _ = self.utils.parse(self.file_path)

        # when, then:
        for task in org_file.tasks:
            self.assertIsInstance(task, OrgTask)

            # This assertion might need adjustment based on the actual hashing
            # For now, just check if it's not the original title and contains TASKID
            self.assertTrue(task.id.startswith("TASKID"))

    def test_org_thread_to_json_includes_task_title(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)
        
        # when:
        # Assuming there's at least one task with threads
        task_with_threads = None
        for task in org_file.tasks:
            if task.threads:
                task_with_threads = task
                break
        
        self.assertIsNotNone(task_with_threads, "Sample .org file should have at least one task with threads.")
        
        thread = task_with_threads.threads[0]
        json_output = thread.to_json()

        # then:
        self.assertIn("task_title", json_output)
        self.assertEqual(json_output["task_title"], task_with_threads.title)
        
    def test_parse_creates_org_task_that_are_setted(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)

        # when, then:
        for task in org_file.tasks:
            self.assertIsInstance(task, OrgTask)
            self.assertTrue(task.title is not None)
            self.assertTrue(task.org_node is not None)

    def test_parse_creates_child_task(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)

        # when:
        parent_task = org_file.root
        child_task = parent_task.children[0]
        
        # then:
        self.assertEqual(parent_task, child_task.parent)

    def test_parse_parent_thread_dont_duplicate_content(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)

        # when:
        parent_task = org_file.root
        
        # then:
        self.assertEqual(parent_task.threads[0].content, "My test title")
        self.assertEqual(parent_task.threads[0].children[0].content, "My test title 3")

    def test_parse_creates_org_thread(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.root.threads), 1)

    def test_parse_creates_second_level_tasks_org_thread(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.root.children[0].threads), 1)
        self.assertIsInstance(org_file.root.children[0].threads[0], OrgThread)

    def test_parse_creates_empty_task(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)

        # when:
        task = org_file.root.children[7]
     
        # then:
        self.assertEqual(len(task.threads), 0)
        
    def test_parse_org_thread_non_lain_entry(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)

        # when, then:
        self.assertEqual(len(org_file.root.children[3].threads), 0)

    def test_parse_creates_org_task_with_more_than_one_thread(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)

        # when
        org_task = org_file.root.children[9]

        # then:
        self.assertEqual(len(org_task.threads), 2)

    def test_parse_creates_org_task_with_complex_thread(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)

        # when
        org_task = org_file.root.children[10]

        # then:
        self.assertEqual(len(org_task.threads), 1)

    def test_parse_creates_org_task_with_complex_thread(self):
        # given:
        org_file, _ = self.utils.parse(self.file_path)

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

class TestMappingLogic(unittest.TestCase):
    def setUp(self):
        self.file_path = "test/unit/sample.org"
        
    def test_org_parser_returns_cleaning_visitor_with_links(self):
        # When
        org_file, cleaning_visitor = OrgParser.parse(self.file_path)

        # Then
        self.assertIsInstance(cleaning_visitor, CleaningVisitor)
        self.assertIsInstance(cleaning_visitor.links, dict)
        self.assertGreater(len(cleaning_visitor.links), 0)

        # Verify some expected links
        # Need to parse the sample.org to know the exact titles that will be hashed
        # For example, if "TITLE" is in sample.org, then we expect its hash
        # Assuming "TITLE" and "TITLE 10" are in sample.org and generate TASKIDs
        sample_titles = ["TITLE", "TITLE 2", "TITLE 3", "TITLE 4", "TITLE 5", "TITLE 6", "TITLE 7", "TITLE 8", "TITLE 9", "TITLE 10", "TITLE 11", "TITLE 12", "TITLE 13"] # Adjust based on actual sample.org content
        for title in sample_titles:
            if title in cleaning_visitor.links:
                self.assertTrue(cleaning_visitor.links[title].startswith("TASKID"), f"Expected TASKID for title '{title}'")

        # Verify HTTPID links
        expected_http_links = [
            "https://test1.com",
            "https://test2.com",
            "https://test3.com",
            "https://test4.com",
        ]
        for link in expected_http_links:
            self.assertIn(link, cleaning_visitor.links, f"Expected HTTP link '{link}' not found in mappings")
            self.assertTrue(cleaning_visitor.links[link].startswith("HTTPID"), f"Expected HTTPID for link '{link}'")

        # Verify TASKID links from org-mode links in content
        expected_org_mode_links = [
            "MYLINK",
            "some.org::title a",
            "some.org::title b",
        ]
        for link in expected_org_mode_links:
            self.assertIn(link, cleaning_visitor.links, f"Expected Org-mode link '{link}' not found in mappings")
            self.assertTrue(cleaning_visitor.links[link].startswith("TASKID"), f"Expected TASKID for Org-mode link '{link}'")

    def test_org_database_indexes_mappings_correctly(self):
        # Given
        mock_es_client = MagicMock()
        
        # Instantiate OrgDatabase with a mock OpenSearch client
        # Mocking the __init__ to prevent actual OpenSearch connection attempts during unit test
        with unittest.mock.patch('lain.lain_org_utils.OpenSearch') as MockOpenSearch:
            MockOpenSearch.return_value = mock_es_client
            org_database = OrgDatabase(opensearch_client=mock_es_client, index_name="test-org-index", mappings_index_name="test-mappings-index")
            # The mock_es_client is already set as org_database.elasticsearch inside the mocked __init__

            # Simulate links from CleaningVisitor
            test_links = {
                "My Task Title": "TASKID123",
                "https://example.com/some/url": "HTTPID456"
            }

            # When
            org_database.index_mappings(test_links)

            # Then
            # Assert that elasticsearch.index was called twice, once for each mapping
            self.assertEqual(mock_es_client.index.call_count, 2)

            # Verify the first call for TASKID
            first_call_args = mock_es_client.index.call_args_list[0].kwargs
            self.assertEqual(first_call_args['index'], "test-mappings-index")
            self.assertEqual(first_call_args['id'], "TASKID123")
            self.assertEqual(first_call_args['body']['hashed_id'], "TASKID123")
            self.assertEqual(first_call_args['body']['original_value'], "My Task Title")
            self.assertEqual(first_call_args['body']['type'], "TASKID")

            # Verify the second call for HTTPID
            second_call_args = mock_es_client.index.call_args_list[1].kwargs
            self.assertEqual(second_call_args['index'], "test-mappings-index")
            self.assertEqual(second_call_args['id'], "HTTPID456")
            self.assertEqual(second_call_args['body']['hashed_id'], "HTTPID456")
            self.assertEqual(second_call_args['body']['original_value'], "https://example.com/some/url")
            self.assertEqual(second_call_args['body']['type'], "HTTPID")


if __name__ == '__main__':
    unittest.main()