import argparse
from lain.lain_org_utils import OrgModule

def main():
    parser = argparse.ArgumentParser(description="Ingest Org-mode files into OpenSearch.")
    parser.add_argument("source_path", help="Path to the directory containing Org-mode files.")
    parser.add_argument("--index-name", help="Name of the OpenSearch index to use.", default=None)
    args = parser.parse_args()

    org_module = OrgModule()
    org_module.run(args.source_path, index_name=args.index_name)

if __name__ == "__main__":
    main()
