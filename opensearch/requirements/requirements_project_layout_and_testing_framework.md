# TDD (Test-Driven-Development) automated feature development

## Methodology

Leverage red-green refactor and test pyramid, start by adding unit tests, update the code to pass the unit tests, then validate using an integration test.

Leverage SOLID and OOP, striving to produce highly testeable code.

## Context

```
my_project/
├── pyproject.toml       # Build system, dependencies, and tool config
├── README.md            # Project documentation
├── src/                 # Source code directory
│   └── my_package/      # Your actual package
│       ├── __init__.py
│       ├── module_a.py
│       └── module_b.py
├── test/               # Test suite
│   ├── unit/            # Isolated logic tests
│   │   ├── __init__.py
│   │   └── test_module_a.py
│   └── integration/     # API, DB, and multi-module tests
│       ├── __init__.py
│       └── test_workflow.py
└── .gitignore           # Files to exclude from Git
```


## Task

1) Update the regression suite to use python -m unittest instead of pytest.

```bash
python -m unittest discover -s tests/ -p test_*.py
```

2) Update the project layout to follow the structure described in the context.

3) Run the regression suite and make sure eveything is still working after the project
   layout refactor.

