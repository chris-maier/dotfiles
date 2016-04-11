;;; init-evil.el --- Evil configuration
;; Author: C.Maier
;;; Commentary:
;; Evil configuration
;;; Code:

(require-package 'evil)
(require-package 'evil-jumper)
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

  ;; ORG-Mode
  (evil-define-key 'normal org-mode-map (kbd "C-S-l") 'org-shiftright)
  (evil-define-key 'normal org-mode-map (kbd "C-S-h") 'org-shiftleft)
  (evil-define-key 'insert org-mode-map (kbd "C-S-l") 'org-shiftright)
  (evil-define-key 'insert org-mode-map (kbd "C-S-h") 'org-shiftleft)
  ;; Move org element right
  (evil-define-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
  (evil-define-key 'insert org-mode-map (kbd "M-l") 'org-metaright)
  ;; Move org element left
  (evil-define-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)
  (evil-define-key 'insert org-mode-map (kbd "M-h") 'org-metaleft)
  ;; Move org element p
  (evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
  (evil-define-key 'insert org-mode-map (kbd "M-k") 'org-metaup)
  ;; Move org element down
  (evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key 'insert org-mode-map (kbd "M-j") 'org-metadown)

  ;; Jumper Mode mappings
  (define-key evil-normal-state-map (kbd "C-+") 'evil-jump-to-tag)
  (define-key evil-normal-state-map (kbd "C-o") 'evil-jumper/backward)
  (define-key evil-normal-state-map (kbd "C-i") 'evil-jumper/forward)

  ;; Evil numbers mappings
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-y") 'evil-numbers/dec-at-pt))

(use-package evil
  :ensure t
  :config
  (add-hook 'evil-mode-hook 'my/config-evil)
  (evil-mode 1))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (my/config-evil-leader))

(use-package evil-jumper
  :ensure t
  :config
  (global-evil-jumper-mode))

(use-package evil-escape
  :ensure t
  :diminish evil-escape-mode
  :config
  (evil-escape-mode)
  (setq evil-escape-unordered-key-sequence t)
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-key-sequence "jk"))

(provide 'init-evil)
;;; init-evil.el ends here
