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
                                 
                                 (message "All tests passed!")
                                 (kill-emacs 0))
                             (error
                              (message "Test failed: %s" err)
                              (kill-emacs 1)))))

(message "Server started, waiting for tests...")
(while t (sleep-for 1))
