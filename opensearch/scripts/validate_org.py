import sys
import os
import argparse
import logging
import re

# Add src to PYTHONPATH
sys.path.append('src')

from lain.lain_org_utils import OrgParser, OrgFileDiscovery

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)

def validate_timestamp_regex(file_path):
    """Perform a simple regex scan for obviously wrong timestamps that might crash orgparse."""
    errors = []
    # Check for days > 31 or minutes > 59 (using simple patterns as seen in our data)
    # Day > 31: [3][2-9] or [4-9][0-9]
    # Minute > 59: :[6-9][0-9] or :[0-9]{3,}
    day_pattern = re.compile(r'\d{4}-\d{2}-([3][2-9]|[4-9]\d)')
    min_pattern = re.compile(r':([6-9]\d|\d{3,})')

    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
        for i, line in enumerate(f, 1):
            if day_pattern.search(line):
                errors.append(f"Line {i}: Potential invalid day found: {line.strip()}")
            if min_pattern.search(line):
                # Filter out common false positives like port numbers if needed, 
                # but for Org files these are usually timestamps.
                errors.append(f"Line {i}: Potential invalid minute found: {line.strip()}")
    return errors

def validate_org_file(file_path):
    print(f"Validating {file_path}...")
    
    # 1. Regex scan
    regex_errors = validate_timestamp_regex(file_path)
    for err in regex_errors:
        logger.warning(f"  [Regex] {err}")

    # 2. OrgParser scan (The ultimate test)
    try:
        result = OrgParser.parse(file_path)
        if result is None:
            logger.error(f"  [Parser] Failed to parse {file_path} (returned None).")
            return False
        
        org_file, _ = result
        print(f"  [Parser] Success: {len(org_file.tasks)} tasks parsed.")
        return True
    except Exception as e:
        logger.error(f"  [Parser] Exception during parsing: {e}")
        return False

def main():
    parser = argparse.ArgumentParser(description="Validate Org-mode files for ingestion.")
    parser.add_argument("path", help="Path to a file or directory.")
    args = parser.parse_args()

    if os.path.isfile(args.path):
        files = [args.path]
    else:
        files = OrgFileDiscovery.discover_files(args.path)

    total = len(files)
    failed = 0

    print(f"Starting validation for {total} files...\n")
    for f in files:
        if not validate_org_file(f):
            failed += 1
        print("-" * 20)

    print(f"\nValidation Complete.")
    print(f"Total files: {total}")
    print(f"Passed: {total - failed}")
    print(f"Failed: {failed}")

if __name__ == "__main__":
    main()
