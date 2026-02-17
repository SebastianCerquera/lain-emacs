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

;; Local modules
(add-to-list 'load-path "~/.emacs.d/htmlize/")
(require 'htmlize)

(add-to-list 'load-path "~/.emacs.d/lain/")
(require 'lain)

(evil-mode 1)

(defun high-bright-look-and-feel ()
  (interactive)
  (set-background-color "black")
  (set-foreground-color "orange"))