;;; init.el --- My personal Emacs configuration
;; Author: C.Maier
;;; Commentary:
;; "If I waited for perfection ... I would never write a word"
;; Margaret Atwood
;; So I just start...

;;; Code:
;; add "lisp" to the load-path

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
(setq visual-line-mode t)

;; Scrolling
(setq scroll-margin 10)
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; Follow symbolic links
(setq vc-follow-symlinks t)

;; PAREN-MODE - display matching parentheses
;;(setq show-paren-style 'mixed)
;;(setq show-paren-delay 0.05)
(show-paren-mode 1)

;; remap yes-or-no questions
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'init-package)

(require-package 'use-package)
(require-package 'diminish)

;; Load personal lisp files
;; POWERLINE - Mode line customization
(require 'init-powerline)
(require 'init-evil)
(require 'init-utils)
(require 'init-lisp)

(use-package helm
  :ensure t
  ;; :defer t
  :diminish helm-mode
  :init
  (helm-mode 1)
  :bind (:map helm-find-files-map
	      ("<C-backspace>" . backward-kill-word))
  :config
  ;; rebind tab to run persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  ;; (setq helm-locate-fuzzy-match t)
  (setq helm-case-fold-search 'smart)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  )

(use-package helm-projectile
  :after helm
  :ensure t
  ;; :defer t
  :init
  (setq helm-projectile-fuzzy-match t)
  ;; :commands (helm-projectile helm-projectile-switch-project)
  (helm-projectile-on)
  )

(use-package projectile
  :ensure t
  ;; :defer t
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  )

;; (use-package yasnippet
;;   :ensure t
;;   :defer t
;;   ;; :diminish yas-minor-mode
;;   :init
;;   (yas-global-mode)
;;   :config
;;   (yas-reload-all)
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;;   ;; (setq tab-always-indent 'complete)
;;   (setq yas-prompt-functions '(yas-completing-prompt
;;                                yas-ido-prompt
;;                                yas-dropdown-prompt))
;;   ;; disable TAB so that it does not interfere with Company
;;   (define-key yas-minor-mode-map (kbd "<tab>") nil)
;;   (define-key yas-minor-mode-map (kbd "TAB") nil)
;;   (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
;;   (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet)
;; )

(use-package company
  :ensure t
  :defer t
  ;; :diminish company-mode
  :init
  (global-company-mode t)
  :config
  (setq company-idle-delay              0.3
	company-minimum-prefix-length   2
	company-show-numbers            nil
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	company-backends                '((company-capf
					   company-files
					   company-clang
					   company-gtags
					   company-c-headers))
	)
  :bind ("C-;" . company-complete-common)
  )

(use-package company-c-headers
  :after company
  :ensure t
  :config
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")
  )

(use-package company-jedi
  :after company
  :ensure t
  :config
  (lambda () (interactive)(add-hook 'python-mode-hook '(progn (add-to-list 'company-backend 'company-jedi))))
  )

;; Zenburn THEME
;; (use-package zenburn-theme
;;   :ensure t
;;   :init
;;   (load-theme 'zenburn t)
;; ;;   :defer t)

(use-package leuven-theme
  :ensure t
  :init
  (load-theme 'leuven t)
  )

;; Tango Plus THEME
;; (use-package tango-plus-theme
;;   :ensure t
;;   :init
;;   (load-theme 'tango-plus t))

;; DIRED
(use-package dired
  :config
  ;; use the same buffer for navigation
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
  (global-set-key (kbd "<f7>") 'my/show-dired)
  (put 'dired-find-alternate-file 'disabled nil)
  )

;; TERM
(use-package term
  :config
  ;; disable scroll-margin in term-mode to use the full screen
  (add-hook 'term-mode-hook (lambda () (interactive) (setq-local scroll-margin 0)))
  )

;; RAINBOW parentheses
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  ;; more color saturation
  (require 'cl-lib)
  (require 'color)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 40)))
  )

;; Fill column indicator
(use-package fill-column-indicator
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'fci-mode)
  )

;; Flycheck everything
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'prog-mode-hook #'flycheck-mode)
  )

;; MAGIT
;; (use-package 'magit
;;   :if (eq system-type 'windows-nt)
;;   :ensure t
;;   )

;; ISPELL settings
;; (setq ispell-dictionary "english")
;; (dolist (hook '(prog-mode-hook))
;;   (add-hook hook 'flyspell-prog-mode))

;; BACKUP
(defvar backup-dir "~/.emacs.d/backups/")
(unless (file-name-directory backup-dir)
  (make-directory backup-dir t))
(setq backup-by-copying t
      backup-directory-alist (list (cons "." backup-dir))
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

;; Programming Mode
(add-hook 'prog-mode-hook 'my/prog-mode-hooks)

;; C Mode programming
(setq c-default-style "linux"
      c-basic-offset 4
      tab-width 4
      indent-tabs-mode t)

;; disable line split on org-meta-return
(setq org-M-RET-may-split-line nil)

(cond ((eq system-type 'windows-nt)
       (setq path-to-ctags "C:/Users/u2832/emacs-24.3/bin/ctags.exe"))
      ((eq system-type 'gnu/linux)
       (setq path-to-ctags "ctags")))

;; Diminish all other things around
(eval-after-load "ElDoc" '(diminish 'eldoc-mode))
(eval-after-load "Undo-Tree" '(diminish 'undo-tree-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "Abbrev" '(diminish 'abbrev-mode))
;; (eval-after-load "projectile" '(diminish 'projectile-mode))

(use-package hideshow
  :ensure t
  :diminish hs-minor-mode
  :config
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  )

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :config
  (progn
    (require 'smartparens-config)
    )
  )

;; auto wrap around lines at 120 char
(use-package simple
  :diminish auto-fill-mode
  :init
  (setq-default fill-column 120)
  (add-hook 'text-mode-hook #'auto-fill-mode)
  )

(evil-mode t)

(provide 'init)
;;; init.el ends here
