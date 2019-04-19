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
     ("^.+//calendar/\\(.*\\)" . calendar-view)
     ("^.+//todo/\\(.*\\)" . todo-view)
     ("^.+//base.html" . cookie-handler)
     ("^.*//\\(.*\\)" . elnode-webserver)))
 
(setq org-agenda-export-html-style "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src * 'unsafe-inline' 'unsafe-eval'; script-src * 'unsafe-inline' 'unsafe-eval'; connect-src * 'unsafe-inline'; img-src * data: blob: 'unsafe-inline'; frame-src *; style-src * 'unsafe-inline';\">
<script src=\"http://code.jquery.com/jquery-latest.min.js\" type=\"text/javascript\"></script>
<script type=\"text/javascript\">
    $(document).ready(function(){
        var el;
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

(defun calendar-view (httpcon)
  (let ((org-agenda-files '("/small/SMALL/WORK/PROJECT.org" "/small/SMALL/THINGS/PROJECT.org" "/small/SMALL/SKILLS/PROJECT.org"))
        (org-agenda-buffer-tmp-name "TASKS.html"))
    (org-agenda-list))
  (save-excursion
    (set-buffer (get-buffer-create "TASKS.html"))
    (org-agenda-write "/tmp/org/VIEW.html"))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/VIEW.html" ">Agenda View</a></html>")))

(defun todo-view (httpcon)
  (let ((org-agenda-files '("/small/SMALL/WORK/PROJECT.org" "/small/SMALL/THINGS/PROJECT.org" "/small/SMALL/SKILLS/PROJECT.org"))
        (org-agenda-buffer-tmp-name "TODO.html"))
    (org-todo-list))
  (save-excursion
    (set-buffer (get-buffer-create "TODO.html"))
    (org-agenda-write "/tmp/org/TODO.html"))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/TODO.html" ">Todo View</a></html>")))

(defun task-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (lain-create-agenda-view (elnode-http-param httpcon "text"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))
 
(defun root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon my-app-routes))
 
(elnode-start 'root-handler :port 8080 :host "*")

(provide 'lain)
