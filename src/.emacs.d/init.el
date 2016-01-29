;; add "lisp" to the load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; try to improve slow performance on windows.
(when (eq system-type 'windows-nt)
  (setq w32-get-true-file-attributes nil)
)

;; Essential settings.
;; Disable the GUI stuff
(setq inhibit-splash-screen t
     inhibit-startup-message t
     inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Notify user visually
(setq visible-bell t)

;; Enable visual wrap lines
(setq visual-line-mode)

;; PAREN-MODE - display matching parentheses
;(setq show-paren-style 'mixed)
;(setq show-paren-delay 0.05)
(show-paren-mode 1)

;; remap yes-or-no questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Load personal lisp files
;; POWERLINE - Mode line customization
(require 'init-package)
(require 'init-powerline)
(require 'init-project)
(require 'init-evil)
(require 'init-utils)
(require 'init-lisp)

;; MAGIT does need a 24.4 version of emacs
;; MAGIT config
;; (require-package 'magit)

(require-package 'use-package)
(eval-when-compile
  (require 'use-package))

;; (use-package yasnippet
;;   :ensure t
;;   :defer t
;;   ;; :diminish yas-minor-mode
;;   :config
;;   (yas-reload-all)
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;;   (setq tab-always-indent 'complete)
;;   (setq yas-prompt-functions '(yas-completing-prompt
;;                                yas-dropdown-prompt))
;;   (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))
;;
;; (require-package 'yasnippet)
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"))
;; (yas-global-mode 1)

;; Zenburn THEME
;; does not work properly
;; (require-package 'zenburn-theme)
;; (use-package zenburn-theme
;;   :ensure t
;;   :defer t)

;; ISPELL settings
;; (setq ispell-dictionary "english")
;; (dolist (hook '(prog-mode-hook))
;;   (add-hook hook 'flyspell-prog-mode))

;; BACKUP
(defvar backup-dir "~/.emacs.d/backups/")
(unless (file-name-directory backup-dir)
  (make-directory backup-dir t))
(setq backup-by-copying t
      backup-directory-alist
      '(("." . backup-dir))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; HISTORY
(setq savehist-file "~/.emacs.d/savehist"
      savehist-additional-variables '(kill-ring
				      search-ring
				      regexp-search-ring
				      extended-command-history
				      ring
				      grep-history)
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history t
      savehist-autosave-interval 60)
(setq-default history-length 1000)
(savehist-mode t)

;; Font settings - if available
(my/font-set "DejaVu Sans Mono")

;; Zoom in and out
(global-set-key [C-wheel-up] 'text-scale-increase)
(global-set-key [C-wheel-down] 'text-scale-decrease)

;; POWERLINE
(my-powerline-default-theme)

;; Delete trailing whitespace in every programming language
(add-hook 'prog-mode-hook
	  (lambda () (add-to-list 'before-save-hook 'delete-trailing-whitespace)))

;; disable line split on org-meta-return
(setq org-M-RET-may-split-line nil)

(cond ((eq system-type 'windows-nt)
       (setq path-to-ctags "C:/Users/u2832/emacs-24.3/bin/ctags.exe"))
      ((eq system-type 'gnu/linux)
       (setq path-to-ctags "ctags"))
)

(require-package 'diminish)
(eval-after-load "ElDoc" '(diminish 'eldoc-mode))
(eval-after-load "Undo-Tree" '(diminish 'undo-tree-mode))
(eval-after-load "Helm" '(diminish 'helm-mode))
;; (eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "Abbrev" '(diminish 'abbrev-mode))

(evil-jumper-mode t)
(evil-mode t)
