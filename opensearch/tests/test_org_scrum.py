import unittest
from orgparse import load, loads


class LainSyntaxTest(unittest.TestCase): 
        
    def test_lain_org_files_tasks_have_headings(self):
        #given:
        org_tree = load('tests/sample.org')

        #when, then: 
        self.assertEqual(org_tree.children[0].heading,"TITLE")

    def test_lain_org_files_tasks_have_thread(self):
        #given:
        org_tree = load('tests/sample.org')

        #when, then: 
        self.assertTrue(org_tree.children[0].body is not None)
        self.assertTrue("<2024-05-18>" in org_tree.children[0].body)
        self.assertEqual(org_tree.children[0].body, """  :LOGBOOK:
  :END:
  - <2024-05-18> My test title
    - <2024-05-19> My test title 3""")

    def test_lain_org_files_tasks_headings_have_subheadings(self):
        #given:
        org_tree = load('tests/sample.org')

        #when, then: 
        self.assertEqual(org_tree.children[0].children[0].heading, "TITLE 2")
        self.assertTrue("<2024-05-18>" in org_tree.children[0].children[0].body)
        self.assertEqual(org_tree.children[0].children[0].body, "   - <2024-05-18> My test title 2")