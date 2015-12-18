;; add "lisp" to the load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Essential settings.
;; Disable the GUI stuff
(setq inhibit-splash-screen t
     inhibit-startup-message t
     inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq visible-bell t)

;; PAREN-MODE - display matching parentheses
;(setq show-paren-style 'mixed)
;(setq show-paren-delay 0.05)
(show-paren-mode 1)

;; Load personal lisp files
;; POWERLINE - Mode line customization 
(require 'init-package)
(require 'init-powerline)
(require 'init-evil)
(require 'init-lisp)

;; Zenburn THEME 
(require-package 'zenburn-theme)
(load-theme 'zenburn t)

;;; BACKUP 
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
(setq-default highlight-symbol-idle-delay 1.5)

; active the powerline 
(my-powerline-default-theme)

(evil-mode t)
