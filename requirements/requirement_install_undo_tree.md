# Install and Validate `undo-tree` mode

## Context
The `lain-emacs` environment is intended to be a productive and modern Emacs distribution. Advanced undo history management is a key feature for such an environment. The `undo-tree` package provides a branching undo/redo system which is more intuitive and powerful than the standard Emacs linear undo.

## Task
1) **TDD - Red Phase**: Update the regression suite in `test/regression_suite.sh` to include a check for the `undo-tree` package and its activation. Specifically, add `(check-package 'undo-tree)` and a check that `(require 'undo-tree)` succeeds and `global-undo-tree-mode` can be enabled. Verify that the regression suite fails as expected.

2) **Implementation - Green Phase**: 
    - Modify the `Dockerfile` to install the `undo-tree` package using the Emacs package manager during the image build process.
    - Update the `emacs` configuration (or the relevant initialization file) to `(require 'undo-tree)` and call `(global-undo-tree-mode 1)`.

3) **Refactor Phase**: Ensure the installation and configuration follow the established patterns in the `Dockerfile` and `emacs` configuration.

4) **Final Verification**: Run the updated `test/regression_suite.sh` and ensure all tests, including the new `undo-tree` checks, pass successfully.
