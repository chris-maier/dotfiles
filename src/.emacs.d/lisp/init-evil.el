;; EVIL leader config 
(require-package 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key 
  "e" 'find-file
  "," 'other-window
  "o" 'delete-other-windows
  "w" 'save-buffer
  "k" 'kill-buffer)

;; package-install evil
(require-package 'evil)

;; package-install key-chord
(require-package 'key-chord)
(key-chord-mode t)

(defun move-line-up ()
;; Move line up one line  
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
(defun move-line-down ()
;; Move line down one line
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

; up/down in screen lines instead of logical lines
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "C-h") 'backward-word)
(define-key evil-normal-state-map (kbd "C-l") 'forward-word)
(define-key evil-normal-state-map (kbd "C-j") '(lambda () (interactive) (next-line 5)))
(define-key evil-normal-state-map (kbd "C-k") '(lambda () (interactive) (previous-line 5)))
; TODO - improvement store/restore cursor position
(define-key evil-normal-state-map (kbd "C-S-j") 'move-line-down)
(define-key evil-normal-state-map (kbd "C-S-k") 'move-line-up)
(define-key evil-normal-state-map (kbd "SPC") 'evil-search-forward)

;; 'jk' fast <ESC>
(setq key-chord-two-keys-delay 0.2)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode t)

(provide 'init-evil)
