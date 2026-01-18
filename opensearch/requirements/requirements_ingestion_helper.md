# TDD (Test-Driven-Development) automated feature development

## Methodology

Leverage red-green refactor and test pyramid, start by adding unit tests, update the code to pass the unit tests, then validate using an integration test.

Leverage SOLID and OOP, striving to produce highly testeable code.

## Context

@e2e_helper.py was a helper tool created to run the ingestion from the command line.

OrgModule.run is hardcoding the sources path "sample_files"


## Task

1) Create a new script to run the ingestion process:

* You should take the source folder as an argument instead of hardcoding the value.
