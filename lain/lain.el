(define-derived-mode lain-mode emacs-lisp-mode "Lain"
  "Major mode for Lain.")

(defun org-log-note-update (state date hour newstate)
  (require 'org)
  (require 'org-agenda)
  (re-search-forward (org-item-beginning-re) nil t)
  (let ((regex (concat "\\(.+\\)" state "\\(.+\\)\\[[0-9]+-[0-9]+-[0-9]+ \\(.+\\) [0-9]+:[0-9]+\\]")))
    (re-search-forward regex nil t)
    (if newstate
        (setq state newstate))
    (replace-match (concat (match-string 1) state (match-string 2) "[" date " " (match-string 3) " " hour "]"))))

(defun lain-create-agenda-view (text)
  (message "lain-create-agenda-view called for: %s" text)
  (let ((buf (get-buffer "TASKS.html")))
    (when buf
      (with-current-buffer buf
        (goto-char (point-min))
        (when (re-search-forward (regexp-quote text) nil t)
          (let ((m (org-get-at-bol 'org-marker)))
            (unless (and m (marker-buffer m))
              (message "Marker missing or dead for '%s', refreshing agenda..." text)
              (org-scrum-view)))))))
  (with-current-buffer (get-buffer-create "TASKS.html")
    (goto-char (point-min))
    (if (re-search-forward (regexp-quote text) nil t)
        (let ((m (org-get-at-bol 'org-marker)))
          (message "Found task in TASKS.html, marker: %s" m)
          ;; Fallback: if marker is dead, try to find the heading in Org files directly
          (unless (and m (marker-buffer m))
            (message "Marker still dead after refresh. Searching Org files for '%s'..." text)
            (dolist (file lain-org-files)
              (unless (and m (marker-buffer m))
                (with-current-buffer (find-file-noselect file)
                  (save-excursion
                    (goto-char (point-min))
                    (when (re-search-forward (concat "^\\*+.*" (regexp-quote text)) nil t)
                      (setq m (point-marker))
                      (message "Found fallback marker in %s: %s" file m)))))))
          (if (and m (marker-buffer m))
              (with-current-buffer (marker-buffer m)
                (widen)
                (save-excursion
                  (goto-char m)
                  (message "Switching to Org buffer: %s" (buffer-name))
                  (org-narrow-to-subtree)
                  (org-agenda-write-tmp "/tmp/org/ORG-TASK.html")
                  (message "Successfully called org-agenda-write-tmp for ORG-TASK.html")))
            (message "Error: No live marker found for '%s' in TASKS.html (even after refresh and fallback)" text)))
      (message "Error: Task '%s' not found in TASKS.html. Content size: %d" text (buffer-size)))))

(defun lain-destroy-agenda-view (text)
  (switch-to-buffer (get-buffer-create "TASKS.html"))
  (message (buffer-name (current-buffer)))
  (beginning-of-buffer)
  (re-search-forward text)
  (org-agenda-switch-to)
  (widen)
  (delete-file "/tmp/org/ORG-TASK.html"))


(defun lain-reschedule-task (text date)
  (switch-to-buffer (get-buffer-create "TASKS.html"))
  (message (buffer-name (current-buffer)))
  (beginning-of-buffer)
  (re-search-forward text)
  (org-agenda-switch-to)
  (org-schedule '(4))
  (org-schedule '() date)
  (org-narrow-to-subtree)
  (switch-to-buffer (current-buffer))
  (message (buffer-name (current-buffer)))
  (save-buffer 0)
  (org-agenda-write-tmp "/tmp/org/ORG-TASK.html"))


(defun lain-update-task (text date time link state)
  (switch-to-buffer (get-buffer-create "TASKS.html"))
  (message (buffer-name (current-buffer)))
  (beginning-of-buffer)
  (re-search-forward text)
  (org-agenda-switch-to)
  (org-todo 'right)
  ;; this is suppsed to be executed as a hook but it is not running when the command is invoked non interactively.
  (org-add-log-note)
  (org-narrow-to-subtree)
  (switch-to-buffer (current-buffer))
  (org-log-note-update "DONE" date time (if (string-empty-p link) state (concat "[[" link "]" "[" state "]]")))
  (message (buffer-name (current-buffer)))
  (save-buffer)
  (org-agenda-write-tmp "/tmp/org/ORG-TASK.html"))

(defun lain-done-task (text date time link)
  (lain-update-task text date time link "DONE"))

(defun lain-itried-task (text date time link)
  (lain-update-task text date time link "ITRIED"))

(defun lain-canceled-task (text date time link)
  (lain-update-task text date time link "CANCELED"))

(defun lain-kill-org-buffers()
  (dolist (x (buffer-list))
    (if (string-match ".*PROJECT.org" (buffer-name x) 0)
        (kill-buffer x))
    (if (string-match ".*PERIODIC.org" (buffer-name x) 0)
        (kill-buffer x))
    (if (string-match "scrum.org" (buffer-name x) 0)
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
     ("^.+//done/\\(.*\\)" . periodic-done-handler)
     ("^.+//canceled/\\(.*\\)" . periodic-canceled-handler)
     ("^.+//itried/\\(.*\\)" . periodic-itried-handler)
     ("^.+//reschedule/\\(.*\\)" . task-reschedule-handler)
     ("^.+//calendar/\\(.*\\)" . calendar-view)
     ("^.+//scrum/\\(.*\\)" . scrum-view)
     ("^.+//periodic/\\(.*\\)" . periodic-view)
     ("^.+//todo/\\(.*\\)" . todo-view)
     ("^.+//chores/\\(.*\\)" . chores-view)
     ("^.+//base.html" . cookie-handler)
     ("^.*//\\(.*\\)" . elnode-webserver)))


(defvar lain-scrum-agenda-files '("/home/agentworkstation/sources/lain-emacs/sample_files/scrum.org")) ;; SCRUM_AGENDA_FILES

(defun org-scrum-view ()
  (setq lain-org-files lain-scrum-agenda-files)
  (let ((old (get-buffer "TASKS.html")))
    (if old (kill-buffer old)))
  (lain-kill-org-buffers)
  (dolist (file lain-org-files)
    (find-file file))
  (let ((org-agenda-files lain-org-files)
        (org-agenda-sticky nil))
    (org-todo-list)
    (let ((agenda-buf (or (get-buffer "*Org Agenda*")
                          (get-buffer (org-agenda-buffer-name))))
          (old-buf (get-buffer "TASKS.html")))
      (if agenda-buf
          (with-current-buffer agenda-buf
            (if (and old-buf (not (eq old-buf agenda-buf)))
                (kill-buffer old-buf))
            (unless (string= (buffer-name) "TASKS.html")
              (rename-buffer "TASKS.html")))
        (message "Error: Agenda buffer not found! Buffers: %s" (mapcar 'buffer-name (buffer-list)))))))

(defun scrum-view (httpcon)
  (high-bright-look-and-feel)
  (org-scrum-view)
  (if (equal (elnode-http-param httpcon "format") "org")
      (let ((buf (or (get-buffer "TASKS.html") (get-buffer "*Org Agenda*"))))
        (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
        (if buf
            (with-current-buffer buf
              (elnode-http-return httpcon (buffer-string)))
          (elnode-http-return httpcon "Error: Agenda buffer not found")))
    (save-excursion
      (set-buffer (get-buffer-create "TASKS.html"))
      (org-agenda-write "/tmp/org/SCRUM.html" nil nil "TASKS.html"))
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (elnode-http-return httpcon (concat "<html><a href=" "/SCRUM.html" ">Scrum View</a></html>"))))


(setq htmlize-head-tags "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src * 'unsafe-inline' 'unsafe-eval'; script-src * 'unsafe-inline' 'unsafe-eval'; connect-src * 'unsafe-inline'; img-src * data: blob: 'unsafe-inline'; frame-src *; style-src * 'unsafe-inline';\">
<script src=\"http://code.jquery.com/jquery-latest.min.js\" type=\"text/javascript\"></script>
<script type=\"text/javascript\">
    $(document).ready(function(){
        $(document).on('click', \"span.org-todo, .org-agenda-calendar-event, .org-scheduled-today, .org-scheduled, .org-agenda-done, .org-scheduled-previously, .org-warning, .org-priority\", function(el){
           var target = $(el.target);
           if (target.text().trim() === 'ALL') return;
           
           var text = '';
           var container = target.closest('pre');
           if (container.length > 0) {
               var fullText = container.text();
               var node = target[0];
               
               var range = document.createRange();
               range.setStart(container[0], 0);
               range.setEnd(node, 0);
               var offset = range.toString().length;
               
               var start = fullText.lastIndexOf('\\n', offset);
               if (start === -1) start = 0; else start++;
               
               var end = fullText.indexOf('\\n', offset);
               if (end === -1) end = fullText.length;
               
               text = fullText.substring(start, end);
           }
           
           if (!text) {
               text = target.parent().contents().text().split('\\n')[0];
           }

           text = text.replace(/^.*?\\:\\s+/, '')
                      .replace(/^[\\s\\*\\.]*/, '')
                      .replace(/^(TODO|IN_PROGRESS|CHECK|DONE|NO_STATE|LATER|CANCELED)\\s+/, '')
                      .replace(/\\[#?[ABC]\\]/g, '')
                      .replace(/<.+?>/g, '')
                      .replace(/\\d{2}:\\d{2}/g, '')
                      .replace(/^[\\s\\.]+/g, '')
                      .replace(/:[a-zA-Z0-9_@:]+\\s*$/, '')
                      .replace('\\n', '')
                      .replace(/(.+)\\s+PROJECT:\\s+/, '$1')
                      .trim();
           if (text) {
               text = encodeURIComponent(text);
               $.ajax({
                   type: \"GET\", 
                   url: \"/lain/?text=\" + text, 
                   headers: { \"apikey\": \"mykey\" }
                 }).done(function(){
                   window.location = \"ORG-TASK.html\";
               });
           }
        });

        var setTaskTimeStamp = function(){
            var x = new Date();
            var y = [x.getHours(), x.getMinutes()];
            var z = y.join(':');
            $(\"input.time\").val(z);
            var y2 = [x.getUTCFullYear(), x.getUTCMonth() < 10 ? '0' + (x.getUTCMonth() + 1) : (x.getUTCMonth() + 1), x.getUTCDate()];
            $(\"input.date\").val(y2.join('-'));
        };

        var getTaskTimeStamp = function(){
            return { \"date\": $(\"input.date\").val(), \"time\": $(\"input.time\").val() };
        };

        if(window.location.href.indexOf('ORG-TASK.html') !== -1){
             var x = $(\"body\").html();
             $(\"body\").html(\"<button class='done'>Check task</button><button class='itried'>I tried</button><button class='canceled'>Cancel task</button><button class='reschedule'>Reschedule task</button><input type='date' class='date'/><input type='time' class='time'/><br><input type='text' class='link'/>\" + x );
             setTaskTimeStamp();
        }
         
        $(\"button.done\").click(function(){
            var matches = $(\"pre\").text().match(/^\\*+\\s+PERIODIC\\s+(.+)\\n/);
            var text = matches[1].replace(/\\[.+\\]/g,'').replace(/^\\s+/g,'').replace(/\\?/g,'\\\\?');
            text = encodeURIComponent(text);
            var timestamp = getTaskTimeStamp();
            var url = \"text=\" + text + \"&date=\" + timestamp.date + \"&time=\" + timestamp.time + \"&link=\" + $(\"input.link\").val(); 
            $.ajax({ type: \"GET\", url: \"/done/?\" + url, headers: { \"apikey\": \"mykey\" } }).done(function(){ alert(\"state updated\"); window.location = \"ORG-TASK.html\"; });
        });

        var replaceLinks = function(){
              var a = $(\"pre\").text().match(/\\[\\[\\/small\\/SMALL\\/images\\/.+\\..+\\]\\[.+\\..+\\]\\]/g);
              if (a) {
                  var b = a.map(function(x){ var m = x.match(/\\[\\[\\/small\\/SMALL\\/images\\/.+\\..+\\]\\[(.+\\..+)\\]\\]/); return m;});
                  var txt = $(\"body\").html();
                  b.forEach(function(l){ if (l) txt = txt.replace(l[0], '<a href=\"/images/' +  l[1] + '\">' + l[1] + '</a>'); });
                  $(\"body\").html(txt);
              }
        };
        replaceLinks();
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
             window.location = \"/scrum/\";
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


(defun org-periodic-view ()
  (setq lain-org-files '("/small/SMALL/PERIODIC.org"))
  (lain-kill-org-buffers)
  (dolist (file lain-org-files)
      (find-file file))
  (let ((org-agenda-files lain-org-files)
        (org-agenda-buffer-tmp-name "TASKS.html"))
    (org-agenda-list)))

(defun periodic-view (httpcon)
  (high-bright-look-and-feel)
  (org-periodic-view)
  (save-excursion
    (set-buffer (get-buffer-create "TASKS.html"))
    (org-agenda-write "/tmp/org/PERIODIC.html"))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/PERIODIC.html" ">Periodic View</a></html>")))


(defun org-calendar-view ()
  (setq lain-org-files '("/small/SMALL/WORK/PROJECT.org" "/small/SMALL/THINGS/PROJECT.org" "/small/SMALL/SKILLS/PROJECT.org" "/small/SMALL/THINGS/MAINTAINANCE/chores/PROJECT.org"))
  (lain-kill-org-buffers)
  (dolist (file lain-org-files)
      (find-file file))
  (let ((org-agenda-files lain-org-files)
        (org-agenda-buffer-tmp-name "TASKS.html"))
    (org-agenda-list)))

(defun calendar-view (httpcon)
  (high-bright-look-and-feel)
  (org-calendar-view)
  (save-excursion
    (set-buffer (get-buffer-create "TASKS.html"))
    (org-agenda-write "/tmp/org/VIEW.html"))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/VIEW.html" ">Agenda View</a></html>")))

(defun org-todo-view ()
  (setq lain-org-files '("/small/SMALL/WORK/PROJECT.org" "/small/SMALL/THINGS/PROJECT.org" "/small/SMALL/SKILLS/PROJECT.org"))
  (lain-kill-org-buffers)
  (dolist (file lain-org-files)
    (find-file file))
  (let ((org-agenda-files lain-org-files)
        (org-agenda-buffer-name "TASKS.html"))
    (org-todo-list)))

(defun todo-view (httpcon)
  (high-bright-look-and-feel)
  (org-todo-view)
  (save-excursion
    (set-buffer (get-buffer-create "TASKS.html"))
    (org-agenda-write "/tmp/org/TODO.html" nil nil"TASKS.html"))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/TODO.html" ">Todo View</a></html>")))

(defun org-chores-view ()
  (setq lain-org-files '("/small/SMALL/THINGS/MAINTAINANCE/chores/PROJECT.org"))
  (lain-kill-org-buffers)
  (dolist (file lain-org-files)
    (find-file file))
  (let ((org-agenda-files lain-org-files)
        (org-agenda-buffer-name "TASKS.html"))
    (org-todo-list)))

(defun chores-view (httpcon)
  (high-bright-look-and-feel)
  (org-chores-view)
  (save-excursion
    (set-buffer (get-buffer-create "TASKS.html"))
    (org-agenda-write "/tmp/org/CHORES.html" nil nil"TASKS.html"))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/CHORES.html" ">Chores View</a></html>")))

 (defun task-reschedule-handler (httpcon)
  (high-bright-look-and-feel)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (lain-reschedule-task (elnode-http-param httpcon "text") (elnode-http-param httpcon "date"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))

(defun periodic-itried-handler (httpcon)
  (high-bright-look-and-feel)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (lain-itried-task (elnode-http-param httpcon "text") (elnode-http-param httpcon "date") (elnode-http-param httpcon "time") (elnode-http-param httpcon "link"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))

(defun periodic-done-handler (httpcon)
  (high-bright-look-and-feel)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (lain-done-task (elnode-http-param httpcon "text") (elnode-http-param httpcon "date") (elnode-http-param httpcon "time") (elnode-http-param httpcon "link"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))

(defun periodic-canceled-handler (httpcon)
  (high-bright-look-and-feel)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (lain-canceled-task (elnode-http-param httpcon "text") (elnode-http-param httpcon "date") (elnode-http-param httpcon "time") (elnode-http-param httpcon "link"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))

(defun task-handler (httpcon)
  (high-bright-look-and-feel)
  (if (equal (elnode-http-param httpcon "format") "org")
      (let ((text (elnode-http-param httpcon "text")))
        (let ((buf (get-buffer "TASKS.html")))
          (when buf
            (with-current-buffer buf
              (goto-char (point-min))
              (when (re-search-forward (regexp-quote text) nil t)
                (let ((m (org-get-at-bol 'org-marker)))
                  (unless (and m (marker-buffer m))
                    (message "Marker missing or dead for '%s' (org), refreshing agenda..." text)
                    (org-scrum-view)))))))
        (with-current-buffer (get-buffer-create "TASKS.html")
          (goto-char (point-min))
          (if (re-search-forward (regexp-quote text) nil t)
              (let ((m (org-get-at-bol 'org-marker)))
                (if (and m (marker-buffer m))
                    (with-current-buffer (marker-buffer m)
                      (widen)
                      (save-excursion
                        (goto-char m)
                        (org-narrow-to-subtree)
                        (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
                        (let ((content (buffer-string)))
                          (widen)
                          (elnode-http-return httpcon content))))
                  (elnode-http-start httpcon 500 '("Content-type" . "text/plain"))
                  (elnode-http-return httpcon "Error: No live marker found after refresh")))
            (elnode-http-start httpcon 404 '("Content-type" . "text/plain"))
            (elnode-http-return httpcon "Error: Task not found"))))
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (lain-create-agenda-view (elnode-http-param httpcon "text"))
    (elnode-http-return httpcon (concat "<html><b>" "</b></html>"))))
 
(defun root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon my-app-routes))
 
(elnode-start 'root-handler :port 8080 :host "0.0.0.0")

(provide 'lain)
