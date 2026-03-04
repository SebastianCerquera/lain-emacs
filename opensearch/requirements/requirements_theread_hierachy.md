# TDD (Test-Driven-Development) automated feature development

## Methodology

* Leverage red-green refactor and test pyramid, start by adding unit tests, update the code to pass the unit tests, then validate using an integration test.

* Leverage SOLID and OOP, striving to produce highly testeable code.

* DO NOT use print statements, always leverage the logging module.

* DO NOT use pdb or any form of interactive debugger.

## Context 

@src/lain/lain_org_utils.py:

The hierachical nature of the agenda is getting lost, the individual bullet points are ingested under the same task, but the relation to each other is
getting lost.

The issue could be solved by:

1) Adding an new thread_id identifier, the sample below would result in 2 documents, "dolor magna..." and "adipiscing eiusmod..." both would have the same thread_id value.

```
***** TODO [#A] ut, dolor, magna, 2023-06, 5, adipiscing, RESPONSABILITIES
      :LOGBOOK:
      CLOCK: [2026-01-09 vie 17:17]--[2026-01-09 vie 17:22] =>  0:05
      CLOCK: [2026-01-09 vie 16:38]--[2026-01-09 vie 17:17] =>  0:39
      :END:
      - <2025-12-16 mar> dolore magna amet consectetur eiusmod et consectetur dolor consectetur ut labore dolor do labore sit
        - <2026-01-09 vie> adipiscing eiusmod incididunt ut labore ipsum lorem magna incididunt magna tempor aliqua do consectetur do aliqua adipiscing do
          sit sit ut do consectetur incididunt sed et ipsum tempor amet
          do sit ipsum tempor sed sit incididunt amet
          elit do ut sit amet aliqua dolore dolor ut elit tempor
```

2) Adding a new field "message_priority" that would allow me to order the entrien in the same thread.

In the sample provided in the previous point, that would mean that the that the value assigned to the "dolor magna..." document is lower than the one assigned to "adipiscing eiusmod...".


## Task

1) Implement mechanisms to reconstruct the thread hierachy by implementing the solution proposed in the context.

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