#!/bin/bash
set -e

# Build the docker image
# We need to build it to include the modified lain/lain.el (via COPY) 
# OR use volume mount for lain.el as well.
# Since Dockerfile copies lain, simply building it will include changes 
# IF the context is the root of the repo.
# So we must run this script from the root or specify context.

cd /home/agentworkstation/sources/lain-emacs

echo "Building Docker image..."
docker build -t lain-emacs-test .

echo "Running tests..."
docker run --rm \
  -e SCRUM_AGENDA_FILES="/home/agentworkstation/sources/lain-emacs/sample_files/scrum.org" \
  -v /home/agentworkstation/sources/lain-emacs/lain/lain.el:/root/.emacs.d/lain/lain.el \
  -v /home/agentworkstation/sources/lain-emacs/sample_files/scrum.org:/home/agentworkstation/sources/lain-emacs/sample_files/scrum.org \
  -v /home/agentworkstation/sources/lain-emacs/test/test_scrum_docker.el:/tmp/test.el \
  lain-emacs-test \
  bash -c "emacs --batch -l /tmp/test.el"
