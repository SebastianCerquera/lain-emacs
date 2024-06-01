import unittest
import datetime

from orgparse.node import OrgNode
from unittest.mock import MagicMock

from lain.lain_org_utils import OrgTask, OrgThread, CleaningVisitor


class ThreadParserTest(unittest.TestCase): 

    def test_cleanup_org_task(self):        
        #given:
        heading = "TITLE"

        node = MagicMock(spec=OrgNode)
        node.heading = heading

        task = OrgTask(node)

        visitor = CleaningVisitor()

        #when: 
        visitor.visit_org_task(task)

        #then: 
        self.assertEqual(task.title, heading)
        self.assertEqual(task.id, visitor.links[heading])

    def test_cleanup_org_thread_link_with_no_text(self):
        #given:
        heading = "TITLE"

        node = MagicMock(spec=OrgNode)
        node.heading = heading

        task = OrgTask(node)
        thread = OrgThread(f"- <2024-05-18> [[{heading}]]", task=task)

        visitor = CleaningVisitor()

        #when: 
        visitor.visit_org_task(task)
        visitor.visit_org_thread(thread)

        #then: 
        self.assertEqual(thread.content, f"{visitor.links[heading]}")

    def test_cleanup_org_thread_link_with_text(self):
        #given:
        heading = "TITLE"

        node = MagicMock(spec=OrgNode)
        node.heading = heading

        task = OrgTask(node)
        thread = OrgThread(f"- <2024-05-18> [[{heading}][LINK TO TITLE]]", task=task)

        visitor = CleaningVisitor()

        #when: 
        visitor.visit_org_task(task)
        visitor.visit_org_thread(thread)

        #then: 
        self.assertEqual(thread.content, f"[[{visitor.links[heading]}][LINK TO TITLE]]")
