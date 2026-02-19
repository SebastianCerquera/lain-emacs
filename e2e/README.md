# Lain-Emacs E2E Regression Suite

This directory contains the End-to-End (E2E) regression suite for Lain-Emacs, built with [Playwright](https://playwright.dev/) and [Testcontainers](https://testcontainers.com/).

## Overview

The suite automatically:
1. **Builds/Starts a Docker Container** using the project's Dockerfile.
2. **Initializes the Emacs Daemon** with the relevant configuration and agenda files.
3. **Executes Tests** against the running container, verifying the web interface, task navigation, and state isolation (Narrowing Reset).

## Prerequisites

- **Docker**: Must be installed and running.
- **Node.js**: Version 18 or higher recommended.

## Getting Started

1. **Install Dependencies**:
   ```bash
   cd e2e
   npm install
   ```

2. **Run All Tests**:
   ```bash
   npx playwright test
   ```

3. **View Results**:
   If tests fail, Playwright will generate an HTML report.
   ```bash
   npx playwright show-report
   ```

## Key Components

- **`fixtures/containerFixture.ts`**: Handles the container lifecycle. It uses `emacs --fg-daemon` to ensure the container remains active and waits for the elnode port (8080) to be ready.
- **`pages/`**: Implements the Page Object Model (POM) for clean, maintainable selectors and actions.
- **`tests/happy-path.spec.ts`**: The main regression suite covering sequential navigation and special character handling.

## Troubleshooting

### Port Conflicts
If you encounter `ERR_CONNECTION_REFUSED`, ensure:
- No other process is using port 8080.
- Docker has permissions to bind ports.
- Orphaned containers are cleaned up: `docker rm -f $(docker ps -aq --filter name=testcontainers)`

### Timeout Issues
The tests have been configured with increased timeouts (120s) to allow for Emacs daemon cold starts. If you have a slow machine, you may need to increase the timeout in `playwright.config.ts`.
