# Lain-Emacs

A dockerized Emacs environment featuring `lain-mode` for SCRUM agenda management and `elnode` for web-based task visualization.

## Environment Preparation

Before running any tests or the application, ensure your environment is set up correctly.

### Prerequisites
- **Docker**: Required for all tests and deployment.
- **Node.js (v18+)**: Required for running the E2E regression suite.
- **Git**: Required for cloning and managing the repository.

### Initial Setup
1. **Clone the repository**:
   ```bash
   git clone <repository-url>
   cd lain-emacs
   ```

2. **Build the Docker Image**:
   All regression suites (E2E and Image Validation) depend on the `lain-emacs:latest` image.
   ```bash
   docker build -t lain-emacs:latest .
   ```

---

## Running the Regression Suites

Lain-Emacs uses a multi-layered testing strategy.

### 1. Docker Image & Emacs Environment Validation
Validates that Emacs is correctly configured, all packages (`elnode`, `evil`, `dash`, etc.) are installed, and `lain-mode` can be loaded.
```bash
./test_emacs_image.sh
```

### 2. End-to-End (E2E) Regression Suite
Verifies the web interface, sequential task navigation, and state isolation (Narrowing Reset) using Playwright.

**Note**: This suite automatically manages a container instance using `testcontainers`, but it requires the `lain-emacs:latest` image to be built first.

```bash
cd e2e
npm install
npx playwright install chromium
npm test
```
*For more details, see [e2e/README.md](e2e/README.md).*

---

## Manual Deployment for Debugging

If you want to run the application manually to inspect the web interface or Emacs state:

1. **Start the Container**:
   ```bash
   docker run -d 
     -p 8080:8080 
     --name lain-debug 
     -e SCRUM_AGENDA_FILES="/root/scrum.org" 
     -v $(pwd)/sample_files/scrum.org:/root/scrum.org 
     lain-emacs:latest 
     emacs --fg-daemon -l /root/.emacs
   ```

2. **Access the Web Interface**:
   Open your browser and navigate to `http://localhost:8080/SCRUM.html`.

3. **Check Logs**:
   ```bash
   docker logs -f lain-debug
   ```

4. **Cleanup**:
   ```bash
   docker rm -f lain-debug
   ```

## Key Files
- `Dockerfile`: Defines the Emacs 31 environment and package installations.
- `lain/lain.el`: The core logic for agenda view generation and task narrowing.
- `emacs`: The Emacs initialization file (mapped to `/root/.emacs`).
- `entrypoint.sh`: Handles environment variable injection (like `SCRUM_AGENDA_FILES`) into the Emacs configuration.
