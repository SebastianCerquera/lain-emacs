(message "Starting test_scrum_docker.el")
(require 'package)
(package-initialize)

;; Stub for missing function required by lain.el
(defun high-bright-look-and-feel ()
  (interactive)
  (message "Stubbed high-bright-look-and-feel called"))

;; Add lain to load-path (path inside container)
(add-to-list 'load-path "/root/.emacs.d/lain")

(require 'lain)

;; Keep Emacs alive for elnode to handle requests
(run-at-time "2 sec" nil (lambda ()
                           (message "Running tests...")
                           (condition-case err
                               (progn
                                 ;; Test /scrum/?format=org (Raw Org agenda)
                                 (message "Testing /scrum/?format=org (Raw Org)...")
                                 (let ((buffer (url-retrieve-synchronously "http://localhost:8080/scrum/?format=org")))
                                   (with-current-buffer buffer
                                     (goto-char (point-min))
                                     (unless (search-forward "HTTP/1.1 200 OK" nil t)
                                       (error "HTTP 200 not found for /scrum/?format=org"))
                                     (goto-char (point-min))
                                     ;; Check for characteristic Org agenda text (e.g., TODO items)
                                     (unless (search-forward "TODO" nil t)
                                       (message "Raw agenda content: %s" (buffer-string))
                                       (error "Raw Org agenda content (TODO) not found in /scrum/?format=org"))
                                     (message "Successfully validated raw Org response from /scrum/"))
                                   (kill-buffer buffer))

                                 (sleep-for 1)

                                 ;; Test /lain/?text=dolore&format=org (Raw Org task view)
                                 (message "Testing /lain/?text=dolore&format=org (Raw Org)...")
                                 (let ((buffer (url-retrieve-synchronously "http://localhost:8080/lain/?text=dolore&format=org")))
                                   (with-current-buffer buffer
                                     (goto-char (point-min))
                                     (unless (search-forward "HTTP/1.1 200 OK" nil t)
                                       (error "HTTP 200 not found for /lain/?text=dolore&format=org"))
                                     (goto-char (point-min))
                                     ;; Should contain the raw subtree for "dolore"
                                     (unless (search-forward "dolore" nil t)
                                       (message "Raw task content: %s" (buffer-string))
                                       (error "Raw task content 'dolore' not found in /lain/?format=org"))
                                     (message "Successfully validated raw Org response from /lain/"))
                                   (kill-buffer buffer))

                                 (sleep-for 1)

                                 ;; Test /scrum/ endpoint
                                 (let ((buffer (url-retrieve-synchronously "http://localhost:8080/scrum/?text=test")))
                                   (with-current-buffer buffer
                                     (goto-char (point-min))
                                     (unless (search-forward "HTTP/1.1 200 OK" nil t)
                                       (error "HTTP 200 not found for /scrum/"))
                                     (goto-char (point-min))
                                     (unless (search-forward "Scrum View" nil t)
                                       (error "Scrum View link not found"))
                                     (message "Successfully validated /scrum/ endpoint response"))
                                   (kill-buffer buffer))

                                 (sleep-for 1)

                                 ;; Test /SCRUM.html endpoint
                                 (let ((buffer (url-retrieve-synchronously "http://localhost:8080/SCRUM.html")))
                                   (with-current-buffer buffer
                                     (goto-char (point-min))
                                     (unless (search-forward "HTTP/1.1 200 OK" nil t)
                                       (error "HTTP 200 not found for /SCRUM.html"))
                                     (goto-char (point-min))
                                     (unless (search-forward "ipsum" nil t)
                                       (error "Scrum content (ipsum) not found in SCRUM.html"))
                                     (message "Successfully validated /SCRUM.html content"))
                                   (kill-buffer buffer))

                                 (sleep-for 1)

                                 ;; Test /lain/ endpoint (task view generation)
                                 ;; We need TASKS.html to be populated, which it should be from the /scrum/ hit earlier.
                                 (message "Testing /lain/ endpoint for 'dolore'...")
                                 (let ((buffer (url-retrieve-synchronously "http://localhost:8080/lain/?text=dolore")))
                                   (with-current-buffer buffer
                                     (goto-char (point-min))
                                     (unless (search-forward "HTTP/1.1 200 OK" nil t)
                                       (error "HTTP 200 not found for /lain/"))
                                     (message "Successfully hit /lain/ endpoint"))
                                   (kill-buffer buffer))

                                 (sleep-for 1)

                                 ;; Test /ORG-TASK.html endpoint
                                 (message "Verifying /ORG-TASK.html content...")
                                 (let ((buffer (url-retrieve-synchronously "http://localhost:8080/ORG-TASK.html")))
                                   (with-current-buffer buffer
                                     (goto-char (point-min))
                                     (unless (search-forward "HTTP/1.1 200 OK" nil t)
                                       (error "HTTP 200 not found for /ORG-TASK.html"))
                                     (goto-char (point-min))
                                     ;; "dolore" should be in the generated task view
                                     (unless (search-forward "dolore" nil t)
                                       (message "Task view content: %s" (buffer-string))
                                       (error "Task 'dolore' not found in ORG-TASK.html"))
                                     (message "Successfully validated /ORG-TASK.html content"))
                                   (kill-buffer buffer))

                                 (message "All Elnode endpoints and logic validated successfully!")
                                 (kill-emacs 0))
                             (error
                              (message "Test failed: %s" err)
                              (kill-emacs 1)))))

(message "Server started, waiting for tests...")
(while t (sleep-for 1))
