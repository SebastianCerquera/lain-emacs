# Comparison of `test/regression_suite.sh` and `test_emacs_image.sh`

These two shell scripts both serve to validate aspects of an Emacs Docker image, but they differ significantly in their purpose, scope, and implementation details.

## `test/regression_suite.sh` (Created by Agent)

*   **Primary Purpose:** Acts as a continuous integration (CI) or pre-commit hook to quickly validate the Docker image and its core Emacs extensions immediately after a build or modification. It ensures that essential components are correctly installed and functional.
*   **Image Management:**
    *   **Builds the Docker image:** It explicitly calls `docker build` to create the `lain-emacs-test` image.
    *   **Removes the Docker image:** It cleans up by calling `docker rmi` after the tests are complete.
*   **Test Scope & Methodology:**
    *   **Focused Validation:** Verifies the installation and basic functionality of specific Emacs packages (`elnode`, `evil`, `dash`, `json`, `htmlize`) and custom modules (`htmlize`, `lain`) as defined in the `Dockerfile` and `emacs` configuration.
    *   **Single Emacs Lisp Block:** Executes a single, comprehensive Emacs Lisp script inside the container using `emacs --batch -l /tmp/test.el`.
    *   **Direct Error Handling:** Relies on Emacs Lisp's `(error ...)` function to directly signal test failures, causing the `docker run` command to exit with a non-zero status.
*   **Emacs Configuration Loading:**
    *   The `Dockerfile` copies the `emacs` file to `/root/.emacs`. When `emacs --batch` is run, it implicitly loads `/root/.emacs` (the user's init file) before executing the provided Lisp script. The script's `(package-initialize)` and `(require ...)` calls then ensure packages are set up and loaded correctly.
*   **Self-Contained:** Manages the entire build-test-cleanup lifecycle of the specific image under test.

## `test_emacs_image.sh` (Existing Script)

*   **Primary Purpose:** Appears to be a more comprehensive, detailed validation or integration test suite run against an *already existing* Docker image. It covers a broader range of checks, including the Emacs version and a longer list of extensions.
*   **Image Management:**
    *   **Assumes Image Exists:** It expects the `lain-emacs:latest` Docker image to be pre-built and available; it does not build or remove the image itself.
    *   **No Image Cleanup:** Does not perform any Docker image cleanup.
*   **Test Scope & Methodology:**
    *   **Broader Validation:** Checks the Emacs version, verifies the functionality of existing key extensions (`dash`, `htmlize`, `lain`), and then iteratively tests a list of other extensions (`web`, `s`, `noflet`, `kv`, `fakir`, `elnode`, `db`, `creole`).
    *   **Modular Emacs Lisp Snippets:** Uses a helper shell function `run_elisp_code` to execute smaller, targeted Emacs Lisp snippets for each individual test.
    *   **Output Parsing:** Determines test success or failure by `grep`ing specific strings in the Emacs process's output.
*   **Emacs Configuration Loading:**
    *   Explicitly mounts the local `emacs` configuration file into the container as `/root/.emacs` (`-v "$(pwd)/$EMACS_CONFIG_FILE":/root/.emacs`) and runs Emacs with `--load /root/.emacs` to ensure the specific configuration is used for testing.
*   **Temporary File Handling:** Manages the creation and cleanup of temporary `.el` files for each Emacs Lisp snippet executed.

## Summary of Key Differences:

| Feature                   | `test/regression_suite.sh`                                   | `test_emacs_image.sh`                                        |
| :------------------------ | :----------------------------------------------------------- | :----------------------------------------------------------- |
| **Primary Goal**          | Quick post-build validation, CI/CD                         | Comprehensive integration/validation of existing image       |
| **Docker Image Lifecycle**| Builds image, runs container, removes image                   | Assumes image exists, only runs container, no image cleanup  |
| **Emacs Lisp Execution**  | Single large block with `(error ...)` for failure           | Modular snippets via helper function, output `grep` for validation |
| **Configuration Loading** | Relies on Dockerfile `COPY` and implicit loading of `/root/.emacs` + explicit `package-initialize` and `require` in test code | Explicitly mounts `emacs` config and uses `--load`          |
| **Error Reporting**       | Emacs Lisp `(error ...)` leading to shell exit code          | `grep` on Emacs output to determine pass/fail                |
| **Scope of Checks**       | Core packages and custom modules (installed via Dockerfile) | Emacs version, specific known extensions, and a broad list of other extensions |
