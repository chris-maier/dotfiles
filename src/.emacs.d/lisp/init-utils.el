;;; init-utils.el --- My personal functions
;; Author: C.Maier
;;; Commentary:
;; My personal functions
;;; Code:

(require 'evil)
;; Required packages
;; - evil
;; - projectile
;; - helm
;; - helm-projectile

(defun my/move-text-internal (arg)
  "Move lines ARG lines up or down."
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
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (my/move-text-internal arg))

(defun my/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
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
  "My font settings."
  (interactive)
  (when (member myfont (font-family-list))
    (set-face-attribute 'default nil :font myfont))
)

;; CTAGS
(defun my/create-tags (dir-name)
  "Create tags file in DIR-NAME."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -e --extra=+fq -R %s" path-to-ctags (directory-file-name dir-name)))
)

(defun my/find-file ()
  "Find file in current project, if we are not in a project we probably want to navigate to one."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-find-file)
    (helm-find-files-1 default-directory)
    )
  )

(defun my/show-dired ()
  "Create new buffer and open the directory structure of the current buffer.
Switch to home directory if the current buffer has now filename."
  (interactive)
  (setq buf (buffer-file-name))
  (if buf
      (dired (file-name-directory buf))
    (dired "~"))
)

(defun my/prog-mode-hooks ()
  "Executed when switching to prog-mode."
  ;; delete trailing whitespace before saving the file
  (add-to-list 'before-save-hook 'delete-trailing-whitespace)
  ;; enable line numbers
  ;; (linum-mode 1)
  ;; enable code folding
  (fci-mode t)
  (hs-minor-mode t))

(provide 'init-utils)
;;; init-utils.el ends here
