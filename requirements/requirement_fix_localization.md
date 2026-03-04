# Fix Localization and Encoding Issues

## Context
The `lain-emacs` environment currently has issues handling Spanish characters (accents and special characters like 'ñ').
Existing characters with accents are displayed as `?`.
Additionally, copying text with accents into the terminal (which triggers `xterm-paste`) fails with `xterm-paste: Wrong type argument: char-or-string-p`.
This suggests that the locale is not correctly configured in the Docker image and Emacs is not configured to handle UTF-8 by default.

## Task
1) **TDD - Red Phase**: Update the regression suite in `test/regression_suite.sh` to include a check for UTF-8 support and accented characters.
   - Specifically, add a test case that inserts a string with Spanish accents (e.g., "áéíóúñÁÉÍÓÚÑ") into a buffer and verifies that it is stored correctly (not as `?`).
   - Mock a call that triggers `xterm-paste` with accented characters to reproduce the `char-or-string-p` error if possible, or at least verify the environment's locale settings.
   - Verify that the regression suite fails as expected.

2) **Implementation - Green Phase**:
   - Modify the `Dockerfile` to install the `locales` package.
   - Configure the locale to `en_US.UTF-8` and set the `LANG`, `LANGUAGE`, and `LC_ALL` environment variables.
   - Update the `emacs` configuration to explicitly set the language environment and coding systems to UTF-8:
     ```elisp
     (set-language-environment "UTF-8")
     (set-default-coding-systems 'utf-8)
     (set-terminal-coding-system 'utf-8)
     (set-keyboard-coding-system 'utf-8)
     (set-selection-coding-system 'utf-8)
     (prefer-coding-system 'utf-8)
     ```

3) **Refactor Phase**: Ensure the locale and encoding configuration follow the established patterns in the `Dockerfile` and `emacs` configuration. Verify that no redundant or conflicting encoding settings remain.

4) **Final Verification**: Run the updated `test/regression_suite.sh` and ensure all tests, including the new localization checks, pass successfully. Verify manually if possible that `xterm-paste` no longer fails with accented characters.
