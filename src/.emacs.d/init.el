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
(require 'init-evil)
(require 'init-utils)
(require 'init-lisp)

;; MAGIT does need a 24.4 version of emacs
;; MAGIT config
;; (require-package 'magit)

(require-package 'use-package)
(require-package 'diminish)

(use-package helm
  :ensure t
  ;; :defer t
  :diminish helm-mode
  :init
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  :config
  (helm-mode 1)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40))

(use-package helm-projectile
  :ensure t
  ;; :defer t
  :init
  (setq helm-projectile-fuzzy-match t)
  ;; :commands (helm-projectile helm-projectile-switch-project)
  :config
  (helm-projectile-on))

(use-package projectile
  :ensure t
  ;; :defer t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm))

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

;; (use-package company
;;   :ensure t
;;   ;; :defer 30
;;   :init
;;   (global-company-mode)
;;   :config
;;   (setq company-idle-delay nil)
;;   (setq company-minimum-prefix-length 2)
;;   (setq company-abort-manual-when-too-short t)
;;   (setq company-selection-wrap-around t)

;;   (define-key company-mode-map [tab] 'company-complete-common-or-cycle)
;;   (define-key company-active-map (kbd "C-n") 'company-select-next)
;;   (define-key company-active-map (kbd "C-p") 'company-select-previous))


;; Zenburn THEME
;; (use-package zenburn-theme
;;   :ensure t
;;   :init
;;   (load-theme 'zenburn t)
;;   :defer t)
;; Tango Plus THEME
(use-package tango-plus-theme
  :ensure t
  :init
  (load-theme 'tango-plus t))

;; DIRED
(use-package dired
  :config
  ; use the same buffer for navigation
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
)

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
;; (eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "Abbrev" '(diminish 'abbrev-mode))
(eval-after-load "hs-minor-mode" '(diminish 'hs-minor-mode))

(evil-jumper-mode t)
(evil-mode t)
(put 'dired-find-alternate-file 'disabled nil)
