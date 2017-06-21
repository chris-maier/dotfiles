;; -*- mode: emacs-lisp -*-
;; This is my personal configuration file
;;

(global-company-mode t)

;; Scrolling
(setq scroll-margin 10)
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; BACKUP
(defvar backup-dir "~/.spacemacs.d/backups/")
(unless (file-name-directory backup-dir)
  (make-directory backup-dir t))
(setq backup-by-copying t
      backup-directory-alist (list (cons "." backup-dir))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; HISTORY
(setq savehist-file "~/.spacemacs.d/savehist"
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

;; evil escape configuration
(setq-default evil-escape-key-sequence "jk")
(setq evil-escape-unordered-key-sequence t)
(setq-default evil-escape-delay 0.4)

;; move over line endigs
(setq evil-cross-lines t)

;; moving over wrapped lines as they were real once
(evil-define-key 'normal global-map (kbd "j") 'evil-next-visual-line)
(evil-define-key 'normal global-map (kbd "k") 'evil-previous-visual-line)

;;  (define-key evil-normal-state-map (kbd "H") '(lambda () (interactive) (ignore-errors (evil-backward-word-begin))))
;;  (define-key evil-normal-state-map (kbd "L") '(lambda () (interactive) (ignore-errors (evil-forward-word-begin))))

(evil-define-motion cma/evil-prev-visual-line (count)
  "Move the cursor COUNT screen lines down, or -5."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count -5))))

(evil-define-motion cma/evil-next-visual-line (count)
  "Move the cursor COUNT screen lines down, or 5."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count 5))))

;; it overloads 'spacemacs/evil-smart-doc-lookup so remap it to "C-k"
(evil-define-key 'normal global-map (kbd "K") 'cma/evil-prev-visual-line)
(evil-define-key 'visual global-map (kbd "K") 'cma/evil-prev-visual-line)

(evil-define-key 'normal global-map (kbd "C-k") 'spacemacs/evil-smart-doc-lookup)

;; it overloads 'evil-join on "J"
(evil-define-key 'normal global-map (kbd "J") 'cma/evil-next-visual-line)
(evil-define-key 'visual global-map (kbd "J") 'cma/evil-next-visual-line)

;; move wordwise with H and L
(evil-define-key 'normal global-map (kbd "H") 'evil-backward-word-begin)
(evil-define-key 'visual global-map (kbd "H") 'evil-backward-word-begin)
(evil-define-key 'normal global-map (kbd "L") 'evil-forward-word-begin)
(evil-define-key 'visual global-map (kbd "L") 'evil-forward-word-begin)

;; do not kill the server on exit
(evil-leader/set-key "q q" 'spacemacs/frame-killer)

;; C/C++ settings
(setq-default c-default-style "bsd"
              c-basic-offset 4
              tab-width 4)

;; hooks
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'makefile-mode-hook 'whitespace-mode)

(provide 'cma)
