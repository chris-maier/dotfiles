;;; init-evil.el --- Evil configuration
;; Author: C.Maier
;;; Commentary:
;; Evil configuration
;;; Code:

(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-numbers)
(require-package 'evil-nerd-commenter)
(require-package 'evil-escape)

(require 'init-utils)

(defun my/config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states t)
  (evil-leader/set-key
    "e" 'helm-find-files
    "," 'other-window
    "." 'switch-to-prev-buffer
    "o" 'delete-other-windows
    "w" 'save-buffer
    "k" 'kill-buffer

    ;; switch buffers
    "s" 'helm-mini
    ;; find functions in this file
    "f" 'helm-imenu
    "x" 'helm-M-x

    ;; nerd-commenter
    "cc" 'evilnc-comment-or-uncomment-lines

    ;; flycheck
    "n" 'flycheck-next-error
    ))

(defun my/config-evil ()
  "Configure evil mode."

  ;; Moving around
  (define-key evil-normal-state-map (kbd "l") '(lambda () (interactive) (evil-forward-char 1 t t)))
  (define-key evil-normal-state-map (kbd "h") '(lambda () (interactive) (evil-backward-char 1 t t)))
  (define-key evil-normal-state-map (kbd "k") '(lambda () (interactive) (ignore-errors (evil-previous-visual-line))))
  (define-key evil-normal-state-map (kbd "j") '(lambda () (interactive) (ignore-errors (evil-next-visual-line))))
  (define-key evil-normal-state-map (kbd "C-h") '(lambda () (interactive) (ignore-errors (evil-backward-word-begin))))
  (define-key evil-normal-state-map (kbd "C-l") '(lambda () (interactive) (ignore-errors (evil-forward-word-begin))))
  (define-key evil-normal-state-map (kbd "C-j") '(lambda () (interactive) (ignore-errors (evil-next-visual-line 5))))
  (define-key evil-normal-state-map (kbd "C-k") '(lambda () (interactive) (ignore-errors (evil-previous-visual-line 5))))

  (define-key evil-normal-state-map (kbd "SPC") 'evil-search-forward)

  ;; Move Text
  (define-key evil-normal-state-map (kbd "C-S-k") 'my/move-text-up)
  (define-key evil-normal-state-map (kbd "C-S-j") 'my/move-text-down)
  (define-key evil-visual-state-map (kbd "C-S-j") 'my/move-text-down)
  (define-key evil-visual-state-map (kbd "C-S-j") 'my/move-text-down)
  (define-key evil-visual-state-map (kbd "C-S-k") 'my/move-text-up)

  ;; Move in VISUAL
  (define-key evil-visual-state-map (kbd "C-j") 'my/evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "C-k") 'my/evil-previous-visual-line)

  ;; PROJECTILE
  (define-key evil-normal-state-map (kbd "C-t") 'my/find-file)
  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)
  (define-key evil-normal-state-map (kbd "C-S-P") 'helm-projectile-switch-project)

  ;; Jumper Mode mappings
  (define-key evil-normal-state-map (kbd "C-+") 'evil-jump-to-tag)
  (define-key evil-normal-state-map (kbd "C-o") 'evil-jump-backward)
  (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)

  ;; Evil numbers mappings
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-y") 'evil-numbers/dec-at-pt))

(use-package evil
  :ensure t
  :config
  (add-hook 'evil-mode-hook 'my/config-evil)
  (evil-mode 1)
  )

(use-package evil-leader
  :after evil
  :ensure t
  :config
  (global-evil-leader-mode)
  (my/config-evil-leader)
  )

(use-package evil-escape
  :after evil
  :ensure t
  :diminish evil-escape-mode
  :config
  (evil-escape-mode)
  (setq evil-escape-unordered-key-sequence t)
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-key-sequence "jk")
  )

(use-package evil-smartparens
  :after smartparens
  :ensure t
  :diminish evil-smartparens-mode
  :init
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  )

(use-package evil-org
  :after org
  :ensure t
  :diminish evil-org-mode
  )

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode t)
)

;; Vim Tabs in Emacs - Needs more configuration
;; (use-package evil-tabs
;;   :ensure t
;;   :config (global-evil-tabs-mode t)
;; )

(provide 'init-evil)
;;; init-evil.el ends here
