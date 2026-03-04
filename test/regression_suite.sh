#!/bin/bash

IMAGE_NAME="lain-emacs-test"
CONTAINER_NAME="lain-emacs-test-container"
EMACS_LISP_CODE='
(load "/root/.emacs")
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

;; Verify UTF-8 and Spanish accents
(message "Checking UTF-8 and Spanish accents...")
(with-temp-buffer
  (set-buffer-multibyte t)
  (let ((test-string "áéíóúñÁÉÍÓÚÑ"))
    (insert test-string)
    (unless (string= (buffer-string) test-string)
      (error (format "UTF-8 support failed! Expected %s, got %s" test-string (buffer-string))))))

;; Verify Timezone (GMT-5)
(message "Checking Timezone (GMT-5)...")
(let ((tz-name (format-time-string "%Z"))
      (tz-offset (format-time-string "%z")))
  (unless (or (string= tz-name "COT") (string= tz-name "EST") (string= tz-name "-05") (string= tz-offset "-0500"))
    (error (format "Timezone check failed! Expected GMT-5 (COT/EST/-05/-0500), got name=%s offset=%s" tz-name tz-offset))))

;; Verify locale environment
(unless (string-match-p "UTF-8" (or (getenv "LANG") ""))
  (error (format "LANG environment variable is not UTF-8: %s" (getenv "LANG"))))

;; Try to mock xterm-paste scenario
(message "Testing xterm-paste scenario...")
(require '\''term/xterm)
(when (fboundp '\''xterm-paste)
  (let ((test-string "ñ"))
    (with-temp-buffer
      ;; We can'\''t easily simulate the terminal event, but we can check 
      ;; if the environment is set up to handle multibyte chars in terminal
      (unless (eq (terminal-coding-system) '\''utf-8)
        (error (format "Terminal coding system is not utf-8: %s" (terminal-coding-system)))))))

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
