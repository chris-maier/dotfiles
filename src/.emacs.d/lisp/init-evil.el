;; EVIL leader config
(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-nerd-commenter)
(require-package 'key-chord)

(global-evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
  ;; TODO - switch to helm
  ;; "e" 'helm-find-file
  "," 'other-window
  ;; "." 'switch-to-prev-buffer
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

;; We probably should move these functions to a seperate file
(defun move-line-up ()
"Move line up one line"
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
"Move line down one line"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;; Moving around
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "C-h") 'evil-backward-word-begin)
(define-key evil-normal-state-map (kbd "C-l") 'evil-forward-word-begin)
(define-key evil-normal-state-map (kbd "C-j") '(lambda () (interactive) (next-line 5)))
(define-key evil-normal-state-map (kbd "C-k") '(lambda () (interactive) (previous-line 5)))

(define-key evil-normal-state-map (kbd "C-t") 'my/find-file)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)
(define-key evil-normal-state-map (kbd "C-S-P") 'helm-projectile-switch-project)

; TODO - improvement store/restore cursor position
(define-key evil-normal-state-map (kbd "C-S-j") 'move-line-down)
(define-key evil-normal-state-map (kbd "C-S-k") 'move-line-up)
(define-key evil-normal-state-map (kbd "SPC") 'evil-search-forward)

;; 'jk' fast <ESC>
(setq key-chord-two-keys-delay 0.2)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode t)

(provide 'init-evil)
