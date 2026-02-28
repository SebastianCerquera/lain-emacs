#!/bin/bash

IMAGE_NAME="lain-emacs-test"
CONTAINER_NAME="lain-emacs-test-container"
EMACS_LISP_CODE='
(require '\''package)
(package-initialize)

;; Verify packages
(defun check-package (package-name)
  (unless (package-installed-p package-name)
    (error (format "Package %s is not installed!" package-name))))

(check-package '\''elnode)
(check-package '\''evil)
(check-package '\''dash)
(check-package '\''json)


;; Verify htmlize and lain modes
(require '\''htmlize)
(let ((html-buf (with-temp-buffer 
                 (insert "test")
                 (htmlize-buffer))))
  (with-current-buffer html-buf
    (unless (string-match-p "<html" (buffer-string))
      (error "htmlize-buffer failed or did not produce expected output")))
  (kill-buffer html-buf))

(add-to-list '\''load-path "/root/.emacs.d/lain")
(require '\''lain)
(with-temp-buffer
  (insert "(lain-mode)")
  (emacs-lisp-mode)
  (forward-sexp)
  (eval-buffer)
  (unless (eq major-mode '\''lain-mode)
    (error "lain-mode did not activate correctly")))

(message "All specified Emacs packages and modes are correctly loaded and functional.")
'

echo "Building Docker image..."
docker build -t $IMAGE_NAME .

if [ $? -ne 0 ]; then
    echo "Docker image build failed!"
    exit 1
fi

echo "Running Docker container for testing..."
# Use stdin to pass the Emacs Lisp code to avoid escaping issues with bash -c
echo "$EMACS_LISP_CODE" | docker run --rm -i --name $CONTAINER_NAME $IMAGE_NAME bash -c "cat > /tmp/test.el && emacs --batch -l /tmp/test.el"

if [ $? -ne 0 ]; then
    echo "Regression tests failed!"
    exit 1
fi

echo "Regression tests passed successfully!"
echo "Cleaning up Docker image..."
docker rmi $IMAGE_NAME

exit 0
