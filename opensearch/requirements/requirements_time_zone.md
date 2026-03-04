# TDD (Test-Driven-Development) automated feature development

## Methodology

* Leverage red-green refactor and test pyramid, start by adding unit tests, update the code to pass the unit tests, then validate using an integration test.

* Leverage SOLID and OOP, striving to produce highly testeable code.

* DO NOT use print statements, always leverage the logging module.

* DO NOT use pdb or any form of interactive debugger.

## Context 

@src/lain/lain_org_utils.py:

I can find issues with the date of ingested documents, the field "thread_date" seems to be stored as UTC, the dasboards then will present the date in GMT-5 resulting in the wrong value, 6 hours ahead of the actual value. 


## Task

1) Fix the ingestion issue related to the date described in the context.

2) Update the unit regression suite to include the new test cases.

3) Run the unit regression suite and fix any introduced issue:

```bash
python -m unittest discover -s test/unit -p test_*¨.py
```

4) Update the integration regression suite to include the new test cases.

5) Run the integration regression suite and fix any introduced issue:

```bash
python -m unittest discover -s test/integration -p test_*¨.py
```