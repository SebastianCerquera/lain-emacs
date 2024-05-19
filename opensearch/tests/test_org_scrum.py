import unittest
from orgparse import load, loads


class LainSyntaxTest(unittest.TestCase): 
        
    def test_lain_org_files_tasks_have_headings(self):
        #given:
        org_tree = load('tests/sample.org')

        #when, then: 
        self.assertEqual(org_tree.children[0].heading,"TITLE")
