;; Project handling 
(require-package 'ido)
(require-package 'ido-vertical-mode)
(require-package 'ido-better-flex)

(ido-mode t)
(ido-vertical-mode t)
(setq ido-vertical-show-count 20)
;; enable better fuzzy search 
(ido-better-flex/enable)

(require-package 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

(defun my/find-file ()
  "Find file in current project, if we are not in a project we probably want to navigate to one"
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (ido-find-file)
    )
  )

(provide 'init-project)
