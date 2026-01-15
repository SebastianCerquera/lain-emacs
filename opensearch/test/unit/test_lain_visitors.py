import unittest
import datetime

from orgparse.node import OrgNode
from unittest.mock import MagicMock

from src.lain.lain_org_utils import OrgTask, OrgThread, CleaningVisitor


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

    def test_cleanup_org_thread_http(self):
        #given:
        url = "http://test.com?a=1&b=2#test" 

        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)
        thread = OrgThread(f"- <2024-05-18> {url}", task=task)

        visitor = CleaningVisitor()

        #when: 
        visitor.visit_org_task(task)
        visitor.visit_org_thread(thread)

        #then: 
        self.assertEqual(thread.content, f"{visitor.links[url]}")

    def test_cleanup_org_thread_inline_http(self):
        #given:
        url = "http://test.com?a=1&b=2#test" 

        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)
        thread = OrgThread(f"- <2024-05-18> {url} content", task=task)

        visitor = CleaningVisitor()

        #when: 
        visitor.visit_org_task(task)
        visitor.visit_org_thread(thread)

        #then: 
        self.assertEqual(thread.content, f"{visitor.links[url]} content")

    def test_cleanup_org_thread_inline_http_whitespaces(self):
        #given:
        url = "http://www.test.com/s/dgbixixv5yvi81y/word%20word%20Word.pdf" 

        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)
        thread = OrgThread(f"- <2024-05-18> content: {url}", task=task)

        visitor = CleaningVisitor()

        #when: 
        visitor.visit_org_task(task)
        visitor.visit_org_thread(thread)

        #then: 
        self.assertEqual(thread.content, f"content: {visitor.links[url]}")

    def test_cleanup_org_thread_inline_https_whitespaces(self):
        #given:
        url = "https://www.test.com/s/dgbixixv5yvi81y/word%20word%20Word.pdf" 

        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)
        thread = OrgThread(f"- <2024-05-18> content: {url}", task=task)

        visitor = CleaningVisitor()

        #when: 
        visitor.visit_org_task(task)
        visitor.visit_org_thread(thread)

        #then: 
        self.assertEqual(thread.content, f"content: {visitor.links[url]}")

    def test_cleanup_org_thread_inline_http_broken_stress(self):
        #given:
        url = "https://www.test.com/s/dgbixixv5yvi81y/word%20word%20Word.pdf" 

        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"

        task = OrgTask(node)
        thread = OrgThread(f"- <2024-05-18> Saliva mudinorvi parmiti? ste? ug?a raa le lucitvo: {url}", task=task)

        visitor = CleaningVisitor()

        #when: 
        visitor.visit_org_task(task)
        visitor.visit_org_thread(thread)

        #then: 
        self.assertEqual(thread.content, f"Saliva mudinorvi parmiti? ste? ug?a raa le lucitvo: {visitor.links[url]}")


    def test_cleanup_org_thread_removes_indentation(self):
        #given:
        heading = "TITLE"

        node = MagicMock(spec=OrgNode)
        node.heading = heading

        task = OrgTask(node)
        thread = OrgThread(f"""- <2024-05-18> line1
    line2
    line3""", task=task)

        visitor = CleaningVisitor()

        #when: 
        visitor.visit_org_task(task)
        visitor.visit_org_thread(thread)

        #then: 
        self.assertEqual(thread.content, "line1 line2 line3")