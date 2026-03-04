# Fix Timezone Configuration

## Context
After fixing the localization and encoding issues, it was identified that the container is using UTC instead of the required GMT-5 timezone. This affects time-sensitive operations and Org-mode timestamps.

## Task
1) **TDD - Red Phase**: Update `test/regression_suite.sh` to include a check for the correct timezone.
   - Add a test case that verifies the output of `(current-time-string)` or the shell command `date +%Z` matches the expected GMT-5 offset or timezone name (e.g., COT or EST).
   - Verify that the regression suite fails.

2) **Implementation - Green Phase**:
   - Modify the `Dockerfile` to install the `tzdata` package.
   - Set the `TZ` environment variable to `America/Bogota` (or another appropriate GMT-5 timezone).
   - Configure the system timezone by creating a symbolic link: `ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone`.

3) **Refactor Phase**: Ensure the timezone configuration is clean and follows the same pattern as the locale configuration in the `Dockerfile`.

4) **Final Verification**: Run `test/regression_suite.sh` to ensure all tests, including the new timezone check, pass successfully.
