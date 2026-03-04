(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(require 'cl-lib)

(ido-mode t)

(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(require 'iso-transl)
(setq-default indent-tabs-mode nil)

(setq-default standard-indent 2)

(show-paren-mode t)
(put 'narrow-to-region 'disabled nil)

(put 'scroll-left 'disabled nil)

(setq x-select-enable-clipboard t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq org-log-done 'time)

(setq org-clock-idle-time 15)
(setq org-clock-persist 'history)

(setq org-use-tag-inheritance nil)

(server-start)

(put 'downcase-region 'disabled nil)

(require 'package)
; add MELPA to repository list
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; Require all packages, assuming they are pre-installed
(require 'dash)
(require 'elnode)
(require 'evil)
(require 'json) ;; elnode dependency
(require 'undo-tree)
(global-undo-tree-mode 1)



(add-to-list 'load-path "~/.emacs.d/lain/")
(require 'lain)

(evil-mode 1)

(defvar scrum-agenda-files '("/home/agentworkstation/sources/lain-emacs/sample_files/scrum.org")) ;; SCRUM_AGENDA_FILES

(defun scrum-agenda ()
  (interactive)
  (let ((org-agenda-files scrum-agenda-files))
    (org-agenda)))

(defun high-bright-look-and-feel ()
  (interactive)
  (set-background-color "black")
  (set-foreground-color "orange"))