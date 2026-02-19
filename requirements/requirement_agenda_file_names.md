# Agenda file names

## Context

@sample_files/.emacs defines the scrum-agenda function:

* Sets the org-agenda-files.

The agenda file name are aldo hardcoded in the org-scrum-view definition.

## Task

1) Improve the implementation by taking the agenda names as an environment variable in the container.

2) The entry point should update the list in both the scrum-agenda and the org-scrum-view.

3) Update the regression suites

4) Run the regression suite and fix any introduced issue.
