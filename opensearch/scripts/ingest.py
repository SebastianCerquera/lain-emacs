import argparse
from src.lain.lain_org_utils import OrgModule

def main():
    parser = argparse.ArgumentParser(description="Ingest Org-mode files into OpenSearch.")
    parser.add_argument("source_path", help="Path to the directory containing Org-mode files.")
    args = parser.parse_args()

    org_module = OrgModule()
    org_module.run(args.source_path)

if __name__ == "__main__":
    main()
