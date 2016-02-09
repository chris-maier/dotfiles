;; EVIL leader config
(require-package 'evil)
(require-package 'evil-jumper)
(require-package 'evil-leader)
(require-package 'evil-numbers)
(require-package 'evil-org)
(require-package 'evil-nerd-commenter)
(require-package 'key-chord)

(require 'init-utils)

(global-evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
  ;; TODO - switch to helm
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
)

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
(define-key evil-normal-state-map (kbd "C-o") 'evil-jumper/backward)
(define-key evil-normal-state-map (kbd "C-i") 'evil-jumper/forward)

;; Evil numbers mappings
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

;; 'jk' fast <ESC>
(setq key-chord-two-keys-delay 0.2)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode t)

(provide 'init-evil)
