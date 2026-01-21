# TDD (Test-Driven-Development) automated feature development

## Methodology

Leverage red-green refactor and test pyramid, start by adding unit tests, update the code to pass the unit tests, then validate using an integration test.

Leverage SOLID and OOP, striving to produce highly testeable code.

## Context

@requirements/requirement_project_layout_and_testing_framework.md introduced the latest changes related to the project layout and the testing framework. Yet, there is still an issue, the project is not properly set, I can confirm that by looking at the tests imports "import src.lain.lain_org_utils ...", the are taking src as the name of the module which is not true.

The issue could be addressed by properly installing the python module by the means of pip:

```bash
pip install -e .
```
After that it should be possible to import the module as "import lain.lain_org_utils ...", pay close attention to the last statement, it no longers contain the "src." suffix.


## Task

1) Implement the changes described in the context.

2) Run the unit regression test suite and fix any introduced issues:

```bash
python -m unittest discover -s tests/unit -p test_*.py
```
3) Run the integration regression test suite and fix any introduced issues:

```bash
python -m unittest discover -s tests/integration -p test_*.py
```


