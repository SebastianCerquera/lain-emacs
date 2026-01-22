# TDD (Test-Driven-Development) automated feature development

## Methodology

* Leverage red-green refactor and test pyramid, start by adding unit tests, update the code to pass the unit tests, then validate using an integration test.

* Leverage SOLID and OOP, striving to produce highly testeable code.

* DO NOT use print statements, always leverage the logging module.

* DO NOT use pdb or any form of interactive debugger.

## Context

Currently the application does not implement a comprehensive logging strategy, it is done with print statements that in turn hinder code quality.

A proper logging strategy should leverage and existing logging library like the one provided in the standard lib (logging), and ALL the methods should be included, this is to be able to tell the execution sequence by just checking the logs in order to gain visibility of internals of the application, it is not just about logging that the method was called but the arguments should be logged when the level is DEBUG.

* Make sure to cover every single function in the code base.

## Task

1) Fix the issue described in the context.

2) Run the unit regression test suite and fix any introduced issues:

```bash
python -m unittest discover -s test/unit -p test_*.py
```
3) Run the integration regression test suite and fix any introduced issues:

```bash
python -m unittest discover -s test/integration -p test_*.py
```
