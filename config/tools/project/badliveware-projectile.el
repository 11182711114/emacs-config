;;; badliveware-projectile.el -*- lexical-binding: t; -*-

(require 'use-package)
(require 'badliveware-constants)

(use-package projectile
    :hook
    (after-init . projectile-global-mode)
    :init
    (setq-default projectile-cache-file (expand-file-name ".projectile-cache" my/cache-dir)
                  projectile-known-projects-file (expand-file-name ".projectile-bookmarks" my/emacs-dir))
    :custom
    (projectile-completion-system 'ivy)
    (projectile-enable-caching t))


(provide 'badliveware-projectile)
;;; badliveware-projectile.el ends here