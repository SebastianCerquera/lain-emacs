# Lain-Emacs E2E Regression Suite

This directory contains the End-to-End (E2E) regression suite for Lain-Emacs. These tests verify the web interface by interacting with a live Emacs instance running inside a Docker container.

## Overview

The suite is built with [Playwright](https://playwright.dev/) and uses [Testcontainers](https://testcontainers.com/) to manage the lifecycle of the application container.

**Key features verified:**
- **Navigation**: Ability to navigate from the base page to the SCRUM agenda view.
- **Task Interaction**: Ability to click on tasks (including those with special characters or priorities like `[#A]`) and view their contents.
- **State Isolation (Narrowing Reset)**: Verifies that when switching between tasks, the buffer narrowing is correctly reset and only the relevant task content is shown.

## Prerequisites

- **Docker**: Must be installed and running.
- **Node.js**: Version 18 or higher recommended.
- **Docker Image**: The `lain-emacs:latest` image must be built before running tests.

## Running the Suite

1. **Build the Docker Image** (from the repository root):
   ```bash
   cd ..
   docker build -t lain-emacs:latest .
   cd e2e
   ```

2. **Install Dependencies**:
   ```bash
   npm install
   npx playwright install chromium
   ```

3. **Run All Tests**:
   ```bash
   npm test
   ```

3. **Run Tests in UI Mode** (Visual debugging):
   ```bash
   npm run test:ui
   ```

4. **View HTML Report**:
   ```bash
   npm run report
   ```

## Key Components

- **`fixtures/containerFixture.ts`**: Manages the container lifecycle. It builds the Docker image and starts it with a predefined configuration and sample agenda files located in `e2e/sample_files`.
- **`pages/`**: Implements the Page Object Model (POM) for the application's UI, ensuring maintainable tests.
- **`tests/`**: Contains the actual test specifications.
  - `happy-path.spec.ts`: The core regression suite.
  - `priority-repro.spec.ts`: Specialized tests for clicking tasks with priority tags like `[#A]`.

## Configuration

The suite is configured in `playwright.config.ts`. It runs tests sequentially (1 worker) to avoid state conflicts and has an increased timeout (120s) to account for Docker builds and Emacs initialization.

## Other Regression Tests

In addition to the E2E suite, there are lower-level regression tests for the Emacs environment and Docker image:

- **Docker Image Validation**:
  ```bash
  ./test_emacs_image.sh
  ```
  This script validates the Emacs version, package installations (Dash, Htmlize, Lain, etc.), and basic mode functionality.

- **Minimal Regression Suite**:
  ```bash
  ./test/regression_suite.sh
  ```
  A fast check to ensure the Docker image can build and execute a basic Emacs Lisp snippet.

## Troubleshooting

### Timeouts
The E2E tests have a default timeout of 120 seconds to allow for Emacs cold starts and package initialization. If you have a slow machine, you might need to increase this in `playwright.config.ts`.

### Port Conflicts
The suite uses port 8080. If you see connection errors, ensure no other service is using this port.
