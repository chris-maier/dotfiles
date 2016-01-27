;; EVIL leader config
(require-package 'evil)
(require-package 'evil-jumper)
(require-package 'evil-leader)
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
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "C-h") 'evil-backward-word-begin)
(define-key evil-normal-state-map (kbd "C-l") 'evil-forward-word-begin)
(define-key evil-normal-state-map (kbd "C-j") '(lambda () (interactive) (ignore-errors (evil-next-visual-line 5))))
(define-key evil-normal-state-map (kbd "C-k") '(lambda () (interactive) (ignore-errors (evil-previous-visual-line 5))))

(define-key evil-normal-state-map (kbd "C-S-j") 'my/move-line-down)
(define-key evil-normal-state-map (kbd "C-S-k") 'my/move-line-up)

(define-key evil-normal-state-map (kbd "SPC") 'evil-search-forward)

;; Move in VISUAL
(define-key evil-visual-state-map (kbd "C-j") 'my/evil-next-visual-line)
(define-key evil-visual-state-map (kbd "C-k") 'my/evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "C-S-j") 'my/move-region-down)
(define-key evil-visual-state-map (kbd "C-S-k") 'my/move-region-up)

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
;; Move org element up
(evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
(evil-define-key 'insert org-mode-map (kbd "M-k") 'org-metaup)
;; Move org element down
(evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key 'insert org-mode-map (kbd "M-j") 'org-metadown)

;; Jumper Mode mappings
(define-key evil-normal-state-map (kbd "C-+") 'evil-jump-to-tag)
(define-key evil-normal-state-map (kbd "C-o") 'evil-jumper/backward)
(define-key evil-normal-state-map (kbd "C-i") 'evil-jumper/forward)

;; 'jk' fast <ESC>
(setq key-chord-two-keys-delay 0.2)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode t)

(provide 'init-evil)
