import unittest
from unittest.mock import MagicMock, patch
import os
import sys

# Ensure src is in the path for imports
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../src')))

from lain.lain_org_utils import OrgModule

class TestIngestArgs(unittest.TestCase):

    @patch('lain.lain_org_utils.OpenSearch')
    @patch('lain.lain_org_utils.OrgDatabase')
    @patch('lain.lain_org_utils.OrgFileDiscovery.discover_files')
    @patch('lain.lain_org_utils.OrgParser.parse')
    def test_org_module_run_passes_index_name(self, mock_parse, mock_discover, MockOrgDatabase, MockOpenSearch):
        # Setup
        mock_discover.return_value = ['test.org']
        mock_org_file = MagicMock()
        mock_cleaning_visitor = MagicMock()
        mock_cleaning_visitor.links = {}
        mock_parse.return_value = (mock_org_file, mock_cleaning_visitor)
        
        org_module = OrgModule()
        
        # Test with custom index name
        custom_index = "custom-index"
        org_module.run("some/path", index_name=custom_index)
        
        # Verify OrgDatabase was instantiated with the correct index_name
        MockOrgDatabase.assert_called()
        call_kwargs = MockOrgDatabase.call_args.kwargs
        self.assertEqual(call_kwargs['index_name'], custom_index)
        self.assertIn('opensearch_client', call_kwargs)

    @patch('lain.lain_org_utils.OpenSearch')
    @patch('lain.lain_org_utils.OrgDatabase')
    @patch('lain.lain_org_utils.OrgFileDiscovery.discover_files')
    @patch('lain.lain_org_utils.OrgParser.parse')
    def test_org_module_run_default_no_index_name(self, mock_parse, mock_discover, MockOrgDatabase, MockOpenSearch):
        # Setup
        mock_discover.return_value = ['test.org']
        mock_org_file = MagicMock()
        mock_cleaning_visitor = MagicMock()
        mock_cleaning_visitor.links = {}
        mock_parse.return_value = (mock_org_file, mock_cleaning_visitor)
        
        org_module = OrgModule()
        
        # Test without custom index name
        org_module.run("some/path")
        
        # Verify OrgDatabase was instantiated WITHOUT index_name (letting it use its default)
        MockOrgDatabase.assert_called()
        call_kwargs = MockOrgDatabase.call_args.kwargs
        self.assertNotIn('index_name', call_kwargs)
        self.assertIn('opensearch_client', call_kwargs)

if __name__ == '__main__':
    unittest.main()
