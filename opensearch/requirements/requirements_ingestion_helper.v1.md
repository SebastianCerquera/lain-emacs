# TDD (Test-Driven-Development) automated feature development

## Methodology

Leverage red-green refactor and test pyramid, start by adding unit tests, update the code to pass the unit tests, then validate using an integration test.

Leverage SOLID and OOP, striving to produce highly testeable code.

## Context

@requirements/requirement_ingestion_helper.md create the ingest.py utility.

The user manually changed the locations:

ingest.py -> scripts/ingest.py
e2e_helper.py -> test/e2e_helper.py

## Task

1) Create a new script to run the ingestion process:

* You should take the source folder as an argument instead of hardcoding the value.
