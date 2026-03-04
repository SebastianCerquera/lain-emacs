# SETUP

python3 -m venv .dependencies
source .dependencies/bin/activate
pip3 install -r requirements.txt

# Run regressions
PYTHONPATH=src python3 -m unittest discover test/unit

# USAGE

### 1. Validate Agendas
Before ingesting, it's recommended to validate the Org-mode files for any malformed timestamps or structural issues that might cause parsing errors:

```bash
PYTHONPATH=src python scripts/validate_org.py <path_to_org_files>
```
*Example:* `PYTHONPATH=src python scripts/validate_org.py scrum_agendas/`

### 2. Ingest Agendas
To ingest Org-mode files into OpenSearch:

```bash
PYTHONPATH=src python scripts/ingest.py --index-name <index_name> <path_to_org_files>
```
*Example:* `PYTHONPATH=src python scripts/ingest.py --index-name scrum-agendas scrum_agendas/`

### 3. Data Integrity
A detailed report of common parsing issues and their causes can be found in `requirements/data_integrity_report.md`.

# DESIGN

# ROADMAP
