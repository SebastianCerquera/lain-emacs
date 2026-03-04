(require 'package)
(setq package-user-dir "/home/agentworkstation/sources/lain-emacs/elpa")
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Add lain to load-path
(add-to-list 'load-path "/home/agentworkstation/sources/lain-emacs/lain")

;; Load lain
(require 'lain)

;; Keep Emacs alive for elnode to handle requests
(run-at-time "2 sec" nil (lambda ()
                           (message "Running tests...")
                           (condition-case err
                               (progn
                                 ;; Test /scrum/ endpoint
                                 (let ((buffer (url-retrieve-synchronously "http://localhost:8080/scrum/?text=test")))
                                   (with-current-buffer buffer
                                     (goto-char (point-min))
                                     (unless (search-forward "Scrum View" nil t)
                                       (error "Scrum View link not found"))
                                     (message "Successfully accessed /scrum/ link"))
                                   (kill-buffer buffer))

                                 (sleep-for 1)

                                 ;; Test /SCRUM.html endpoint (generated file)
                                 (let ((buffer (url-retrieve-synchronously "http://localhost:8080/SCRUM.html")))
                                   (with-current-buffer buffer
                                     (goto-char (point-min))
                                     ;; Check for content from scrum.org
                                     ;; "ipsum" is a TODO item in scrum.org line 13
                                     (unless (search-forward "ipsum" nil t)
                                       (error "Scrum content (ipsum) not found in SCRUM.html"))
                                     (message "Successfully verified Scrum content"))
                                   (kill-buffer buffer))

                                 (sleep-for 1)

                                 ;; Test /lain/ endpoint for task 1 (commas)
                                 (message "Testing /lain/ for task 1 (commas)...")
                                 (let ((text "eiusmod, 2024-01, 4, dolore, RESPONSABILITIES"))
                                   (let ((buffer (url-retrieve-synchronously (concat "http://localhost:8080/lain/?text=" (url-hexify-string text)))))
                                     (with-current-buffer buffer
                                       (goto-char (point-min))
                                       (unless (search-forward "HTTP/1.1 200 OK" nil t)
                                         (error "HTTP 200 not found for task 1")))
                                     (kill-buffer buffer))
                                   ;; Verify content
                                   (let ((buffer (url-retrieve-synchronously "http://localhost:8080/ORG-TASK.html")))
                                     (with-current-buffer buffer
                                       (goto-char (point-min))
                                       (unless (search-forward "eiusmod" nil t)
                                         (error "Task 1 content not found in ORG-TASK.html"))
                                       (message "Successfully verified task 1"))
                                     (kill-buffer buffer)))

                                 (sleep-for 1)

                                 ;; Test /lain/ endpoint for task 2 (special chars)
                                 (message "Testing /lain/ for task 2 (special chars)...")
                                 (let ((text "Task [with] special? characters"))
                                   ;; Ensure task is in scrum.org (might be there from previous runs but let's be sure)
                                   (with-current-buffer (find-file-noselect "/home/agentworkstation/sources/lain-emacs/sample_files/scrum.org")
                                     (goto-char (point-max))
                                     (unless (save-excursion (goto-char (point-min)) (search-forward text nil t))
                                       (insert "\n* TODO Task [with] special? characters\n")
                                       (save-buffer)))
                                   
                                   ;; Refresh agenda
                                   (url-retrieve-synchronously "http://localhost:8080/scrum/")
                                   
                                   (let ((buffer (url-retrieve-synchronously (concat "http://localhost:8080/lain/?text=" (url-hexify-string text)))))
                                     (with-current-buffer buffer
                                       (goto-char (point-min))
                                       (unless (search-forward "HTTP/1.1 200 OK" nil t)
                                         (error "HTTP 200 not found for task 2")))
                                     (kill-buffer buffer))
                                   ;; Verify content
                                   (let ((buffer (url-retrieve-synchronously "http://localhost:8080/ORG-TASK.html")))
                                     (with-current-buffer buffer
                                       (goto-char (point-min))
                                       (unless (search-forward "special?" nil t)
                                         (error "Task 2 content not found in ORG-TASK.html"))
                                       (message "Successfully verified task 2"))
                                     (kill-buffer buffer)))
                                 
                                 (message "All tests passed!")
                                 (kill-emacs 0))
                             (error
                              (message "Test failed: %s" err)
                              (kill-emacs 1)))))

(message "Server started, waiting for tests...")
(while t (sleep-for 1))
