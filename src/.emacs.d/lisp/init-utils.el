(require 'evil)
;; Required packages
;; - evil
;; - projectile
;; - helm
;; - helm-projectile

(defun my/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun my/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (my/move-text-internal arg))

(defun my/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (my/move-text-internal (- arg)))

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

(defun my/find-file ()
  "Find file in current project, if we are not in a project we probably want to navigate to one"
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-find-file)
    (helm-find-files-1 default-directory)
    )
  )

(provide 'init-utils)
