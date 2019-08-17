;;; badliveware-modeline.el -*- lexical-binding: t; -*-

(require 'use-package)

(use-package doom-modeline
    :custom
    (find-file-visit-truename t)
    (doom-modeline-icon t)
    (doom-modeline-major-mode-icon t)
    (projectile-dynamic-mode-line t)
    (doom-modeline-bar-width 3)
    (doom-modeline-github nil)
    (doom-modeline-mu4e nil)
    (doom-modeline-persp-name nil)
    (doom-modeline-minor-modes nil)
    (doom-modeline-buffer-file-name-style 'relative-from-project)
    :hook
    (after-init . doom-modeline-mode)
    (theme-load . doom-modeline-mode)
    (org-mode . doom-modeline-mode)
    (theme-load . doom-modeline-refresh-bars)
    (doom-modeline-mode . size-indication-mode) ; filesize in modeline
    (doom-modeline-mode . column-number-mode)) ; cursor column in modeline

(use-package hide-mode-line
    :config
    (defun +modeline|hide-in-non-status-buffer ()
    "Show minimal modeline in magit-status buffer, no modeline elsewhere."
    (if (eq major-mode 'magit-status-mode)
        (doom-modeline-set-project-modeline)
        (hide-mode-line-mode))))
(add-hook 'magit-mode-hook #'+modeline|hide-in-non-status-buffer)

(provide 'badliveware-modeline)
;;; badliveware-modeline.el ends here
