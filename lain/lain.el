(defvar 
   my-app-routes 
   '(("^.+//lain/\\(.*\\)" . my-test-handler)
     ("^.*//\\(.*\\)" . elnode-webserver)))
 
(setq elnode-webserver-docroot "/tmp/org/")
 
(setq org-agenda-export-html-style "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src * 'unsafe-inline' 'unsafe-eval'; script-src * 'unsafe-inline' 'unsafe-eval'; connect-src * 'unsafe-inline'; img-src * data: blob: 'unsafe-inline'; frame-src *; style-src * 'unsafe-inline';\">
<script src=\"http://code.jquery.com/jquery-latest.min.js\" type=\"text/javascript\"></script>
<script type=\"text/javascript\">
    $(document).ready(function(){
        var el;
        $(\".org-agenda-calendar-event,.org-scheduled-today,.org-scheduled\").click(function(el){
           $.get(\"/lain/?text=\" + $(el.target).text()).done(function(){
               window.location = \"ORG-TASK.html\";
           });
        });
    });
</script>")
 
(let ((org-agenda-files '("/small/SMALL/WORK/PROJECT.org"))
      (org-agenda-buffer-tmp-name "TASKS.html"))
  (org-agenda-list))
 
(save-excursion
    (set-buffer (get-buffer-create "TASKS.html"))
    (org-agenda-write "/tmp/org/VIEW.html"))
 
(defun my-test-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (message (elnode-http-param httpcon "text"))
  (lain-create-agenda-view (elnode-http-param httpcon "text"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))
 
(defun root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon my-app-routes))
 
(elnode-start 'root-handler :port 8080 :host "*")

(provide 'lain)
