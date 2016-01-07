;; Project handling

(require-package 'helm)
(require-package 'helm-projectile)
(require-package 'helm-ag)

(helm-mode 1)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(helm-projectile-on)

(helm-autoresize-mode 1)
;; fuzzy matching
(setq helm-projectile-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)

(defun my/find-file ()
  "Find file in current project, if we are not in a project we probably want to navigate to one"
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-find-file)
    (helm-find-file)
    )
  )

(provide 'init-project)
