(defun lain-create-agenda-view (text)
  (switch-to-buffer (get-buffer-create "TASKS.html"))
  (message (buffer-name (current-buffer)))
  (beginning-of-buffer)
  (re-search-forward text)
  (org-agenda-switch-to)
  (org-narrow-to-subtree)
  (switch-to-buffer (current-buffer))
  (message (buffer-name (current-buffer)))
  (org-agenda-write-tmp "/tmp/org/ORG-TASK.html"))

(defun lain-destroy-agenda-view (text)
  (switch-to-buffer (get-buffer-create "TASKS.html"))
  (message (buffer-name (current-buffer)))
  (beginning-of-buffer)
  (re-search-forward text)
  (org-agenda-switch-to)
  (widen)
  (delete-file "/tmp/org/ORG-TASK.html"))

(defun lain-check-task (text)
  (switch-to-buffer (get-buffer-create "TASKS.html"))
  (message (buffer-name (current-buffer)))
  (beginning-of-buffer)
  (re-search-forward text)
  (org-agenda-switch-to)
  (org-todo 'right)
  ;; this is suppsed to be executed as a hook but it is not running when the command is invoked non interactively.
  (org-add-log-note)
  (save-buffer)
  (org-narrow-to-subtree)
  (switch-to-buffer (current-buffer))
  (message (buffer-name (current-buffer)))
  (org-agenda-write-tmp "/tmp/org/ORG-TASK.html"))


(defun lain-kill-org-buffers()
  (dolist (x (buffer-list))
    (if (string-match ".*PROJECT.org" (buffer-name x) 0)
        (kill-buffer x))
    (if (string-match ".*PERIODIC.org" (buffer-name x) 0)
        (kill-buffer x))))

(defun org-agenda-write-tmp (file &optional open nosettings agenda-bufname)
  (org-let (if nosettings nil org-agenda-exporter-settings)
    '(save-excursion
       (save-window-excursion
	 (let ((bs (copy-sequence (buffer-string))) beg content)
	   (with-temp-buffer
	     (rename-buffer org-agenda-write-buffer-name t)
	     (set-buffer-modified-p nil)
	     (insert bs)
	     (org-agenda-remove-marked-text 'org-filtered)
	     (run-hooks 'org-agenda-before-write-hook)
	     (cond
	      ((string-match "\\.html?\\'" file)
	       (require 'htmlize)
	       (set-buffer (htmlize-buffer (current-buffer)))
	       (when org-agenda-export-html-style
		 (goto-char (point-min))
		      (kill-region (- (search-forward "<style") 6)
			      (search-forward "</style>"))
		      (insert org-agenda-export-html-style))
	       (write-file file)
	       (kill-buffer (current-buffer))
	       (message "HTML written to %s" file))
	      (t
	       (let ((bs (buffer-string)))
		 (find-file file)
		 (erase-buffer)
		 (insert bs)
		 (save-buffer 0)
		 (kill-buffer (current-buffer))
		 (message "Plain text written to %s" file))))))))))


(setq elnode-webserver-docroot "/tmp/org/")

(defvar 
   my-app-routes 
   '(("^.+//lain/\\(.*\\)" . task-handler)
     ("^.+//check/\\(.*\\)" . periodic-handler)
     ("^.+//calendar/\\(.*\\)" . calendar-view)
     ("^.+//todo/\\(.*\\)" . todo-view)
     ("^.+//base.html" . cookie-handler)
     ("^.*//\\(.*\\)" . elnode-webserver)))
 
(setq org-agenda-export-html-style "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src * 'unsafe-inline' 'unsafe-eval'; script-src * 'unsafe-inline' 'unsafe-eval'; connect-src * 'unsafe-inline'; img-src * data: blob: 'unsafe-inline'; frame-src *; style-src * 'unsafe-inline';\">
<script src=\"http://code.jquery.com/jquery-latest.min.js\" type=\"text/javascript\"></script>
<script type=\"text/javascript\">
    $(document).ready(function(){
        $(\"span.org-todo\").click(function(el){
           var text = $(el.target)[0].nextSibling.textContent;
           text = text.replace(/\\[.+\\]/g,'').replace(/^\\s+/g,'').replace(/\\?/g,'\\\\?').replace(/\\+/g,'\\\\+').replace(\"\\n\", \"\").replace(/(.+)\\s+PROJECT:\\s+/, \"$1\").trim();
           text = encodeURIComponent(text);
           $.ajax({
               type: \"GET\", 
               url: \"/lain/?text=\" + text, 
               headers: {
                 \"apikey\": \"mykey\"
               }
             }).done(function(){
               window.location = \"ORG-TASK.html\";
           });
        });

        $(\".org-agenda-calendar-event,.org-scheduled-today,.org-scheduled,.org-agenda-done,.org-scheduled-previously,.org-warning\").click(function(el){
           var text = $(el.target).text().replace(/\\[.+\\]/g,'').replace(/^\\s+/g,'').replace(/\\?/g,'\\\\?');
           text = encodeURIComponent(text);
           $.ajax({
               type: \"GET\", 
               url: \"/lain/?text=\" + text, 
               headers: {
                 \"apikey\": \"mykey\"
               }
             }).done(function(){
               window.location = \"ORG-TASK.html\";
           });
        });

        var e = window.location.href.split(\"/\");
        if(e[e.length - 1] == \"ORG-TASK.html\"){
             var x = $(\"body\").html();
             $(\"body\").html(\"<button>Check task</button>\" + x );
        }
         
        $(\"button\").click(function(){
            var matches = $(\"pre\").text().match(/^\\*+\\s+PERIODIC\\s+(.+)\\n/);
            var text = matches[1].replace(/\\[.+\\]/g,'').replace(/^\\s+/g,'').replace(/\\?/g,'\\\\?');
            text = encodeURIComponent(text);
         
            $.ajax({
                type: \"GET\", 
                url: \"/check/?text=\" + text, 
                headers: {
                  \"apikey\": \"mykey\"
                }
            }).done(function(){
               alert(\"state updated\");
            });
        });

    });
</script>")

(setq lain-cookie-html "<html>
<head>
<meta http-equiv=\"Content-Security-Policy\" content=\"default-src * 'unsafe-inline' 'unsafe-eval'; script-src * 'unsafe-inline' 'unsafe-eval'; connect-src * 'unsafe-inline'; img-src * data: blob: 'unsafe-inline'; frame-src *; style-src * 'unsafe-inline';\">
<script src=\"http://code.jquery.com/jquery-latest.min.js\" type=\"text/javascript\"></script>
<script type=\"text/javascript\">
    $(document).ready(function(){
         $(\"input:text\").change(function(a){
             document.cookie = \"Authorization=\" + $(a.target).val() +  \"; expires=Fri, 3 Aug 2019 20:47:11 UTC; path=/;\"
         });
 
         $(\"input:submit\").click(function(a){
             window.location = \"/calendar/\";
         });
    });
</script>
</head>
<body>
  Insert the cookie:<br>
  <input type=\"text\" name=\"firstname\" value=\"Mickey\">
  <br>
  <br><br>
  <input type=\"submit\" value=\"Submit\">
</body>
</html>")


(defun cookie-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon lain-cookie-html))

;; i might needed when working on the android client
(defun periodic-view (httpcon)
  (setq lain-org-files '("/small/SMALL/PERIODIC.org"))
  (lain-kill-org-buffers)
  (dolist (file lain-org-files)
      (find-file file))
  (let ((org-agenda-files lain-org-files)
        (org-agenda-buffer-tmp-name "TASKS.html"))
    (org-agenda-list))
  (save-excursion
    (set-buffer (get-buffer-create "TASKS.html"))
    (org-agenda-write "/tmp/org/PERIODIC.html"))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/PERIODIC.html" ">Periodic View</a></html>")))

(defun calendar-view (httpcon)
  (setq lain-org-files '("/small/SMALL/PERIODIC.org" "/small/SMALL/WORK/PROJECT.org" "/small/SMALL/THINGS/PROJECT.org" "/small/SMALL/SKILLS/PROJECT.org"))
  (lain-kill-org-buffers)
  (dolist (file lain-org-files)
      (find-file file))
  (let ((org-agenda-files lain-org-files)
        (org-agenda-buffer-tmp-name "TASKS.html"))
    (org-agenda-list))
  (save-excursion
    (set-buffer (get-buffer-create "TASKS.html"))
    (org-agenda-write "/tmp/org/VIEW.html"))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/VIEW.html" ">Agenda View</a></html>")))

(defun todo-view (httpcon)
  (setq lain-org-files '("/small/SMALL/WORK/PROJECT.org" "/small/SMALL/THINGS/PROJECT.org" "/small/SMALL/SKILLS/PROJECT.org"))
  (lain-kill-org-buffers)
  (dolist (file lain-org-files)
      (find-file file))
  (let ((org-agenda-files lain-org-files)
        (org-agenda-buffer-name "TASKS.html"))
    (org-todo-list))
  (save-excursion
    (set-buffer (get-buffer-create "TASKS.html"))
    (org-agenda-write "/tmp/org/TODO.html" nil nil"TASKS.html"))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/TODO.html" ">Todo View</a></html>")))


(defun periodic-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (lain-check-task (elnode-http-param httpcon "text"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))

(defun task-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (lain-create-agenda-view (elnode-http-param httpcon "text"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))
 
(defun root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon my-app-routes))
 
(elnode-start 'root-handler :port 8080 :host "*")

(provide 'lain)
