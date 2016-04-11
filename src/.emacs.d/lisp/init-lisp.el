;;; init-lisp.el --- Lisp programming configuration
;; Author: C.Maier
;;; Commentary:
;; Lisp programming configuration
;;; Code:

;; Lisp autocompletion
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; S-k gets you lisp documentation
(require-package 'elisp-slime-nav)
(defun my/lisp-hook ()
  "Gets executed on entering Lisp mode."
  (elisp-slime-nav-mode)
  (turn-on-eldoc-mode)
  )
(add-hook 'emacs-lisp-mode-hook 'my/lisp-hook)

;; 'K' show elisp description in lisp mode
(evil-define-key 'normal emacs-lisp-mode-map (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point)

(provide 'init-lisp)
;;; init-lisp.el ends here
