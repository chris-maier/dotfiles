(require 'evil)

(defun my/move-line-up ()
  "Move line up one line"
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun my/move-line-down ()
  "Move line down one line"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun my/move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun my/move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun my/move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (my/move-region start end (if (null n) 1 n)))

(evil-define-motion my/evil-next-visual-line ()
  "Move the cursor 5 screen lines down."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move 5)))

(evil-define-motion my/evil-previous-visual-line ()
  "Move the cursor COUNT screen lines up."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move -5)))

;; FONT settings
(defun my/font-set (myfont)
  ;; My font settings
  (interactive)
  (when (member myfont (font-family-list))
    (set-face-attribute 'default nil :font myfont))
)

;; CTAGS
(defun my/create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -e --extra=+fq -R %s" path-to-ctags (directory-file-name dir-name)))
)

(provide 'init-utils)
