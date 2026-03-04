import unittest
import datetime
from unittest.mock import MagicMock
from orgparse.node import OrgNode
from lain.lain_org_utils import OrgTask, OrgThread, ThreadParser, CleaningVisitor

GMT_MINUS_5 = datetime.timezone(datetime.timedelta(hours=-5))

class TestTimezoneAwareness(unittest.TestCase):
    def setUp(self):
        self.parser = ThreadParser()
        self.visitor = CleaningVisitor()

    def test_thread_parser_timestamp_has_timezone(self):
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"
        task = OrgTask(node)
        thread = OrgThread("- <2024-05-18> My test title", task=task)
        
        self.parser.parse_thread(thread)
        
        self.assertIsInstance(thread.timestamp, datetime.datetime)
        self.assertIsNotNone(thread.timestamp.tzinfo)
        self.assertEqual(thread.timestamp.utcoffset(), datetime.timedelta(hours=-5))
        self.assertEqual(thread.timestamp.year, 2024)
        self.assertEqual(thread.timestamp.month, 5)
        self.assertEqual(thread.timestamp.day, 18)
        self.assertEqual(thread.timestamp.hour, 0)
        self.assertEqual(thread.timestamp.minute, 0)
        self.assertEqual(thread.timestamp.second, 0)

    def test_cleaning_visitor_timestamp_has_timezone(self):
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"
        task = OrgTask(node)
        thread = OrgThread("- <2024-05-18> My test title", task=task)
        
        self.visitor.visit_org_thread(thread)
        
        self.assertIsInstance(thread.timestamp, datetime.datetime)
        self.assertIsNotNone(thread.timestamp.tzinfo)
        self.assertEqual(thread.timestamp.utcoffset(), datetime.timedelta(hours=-5))

    def test_fallback_timestamp_has_timezone(self):
        node = MagicMock(spec=OrgNode)
        node.heading = "TITLE"
        task = OrgTask(node)
        thread = OrgThread("- My test title without timestamp", task=task)
        
        self.visitor.visit_org_thread(thread)
        
        self.assertIsInstance(thread.timestamp, datetime.datetime)
        self.assertIsNotNone(thread.timestamp.tzinfo)
        self.assertEqual(thread.timestamp.utcoffset(), datetime.timedelta(hours=-5))

if __name__ == "__main__":
    unittest.main()
