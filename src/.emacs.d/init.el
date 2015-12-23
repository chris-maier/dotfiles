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
(require 'init-project)
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

(defun my-font-set (myfont)
  ;; My font settings
  (interactive)
  (when (member myfont (font-family-list))
    (set-face-attribute 'default nil :font myfont))
)
(my-font-set "DejaVu Sans Mono")

; active the powerline 
(my-powerline-default-theme)

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(evil-mode t)
