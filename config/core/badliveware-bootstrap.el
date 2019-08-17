;;; badliveware-bootstrap.el -*- lexical-binding: t; -*-

(let ()
  (defvar after-boostrap-hook nil)

  (require 'badliveware-constants (concat user-emacs-directory "config/core/badliveware-constants"))
  (add-hook 'window-setup-hook #'emacs-init-time)

  ;; Core
  (add-to-list 'load-path (concat my/config-dir "core/"))
  (require 'badliveware-defaults)
  (require 'badliveware-package)
  (require 'badliveware-os)
  (require 'badliveware-lib)
  (require 'badliveware-keybinds)
  (add-to-list 'load-path (concat my/config-dir "core/ivy/"))
  (require 'badliveware-ivy)
  (load-file (concat my/config-dir "core/ivy/badliveware-ivy-util.el"))

  ;; Editor
  (add-to-list 'load-path (concat my/config-dir "editor/"))
  (require 'badliveware-evil)
  (require 'badliveware-evil-keybinds)

  ;; Tools
  (add-to-list 'load-path (concat my/config-dir "tools/"))
  (add-to-list 'load-path (concat my/config-dir "tools/project/"))
  (require 'badliveware-projectile)
  (load-file (concat my/config-dir "tools/project/badliveware-projectile-util.el"))
  (require 'badliveware-flycheck)
  (require 'badliveware-magit)
  (require 'badliveware-dired)
  (require 'badliveware-whitespace)
  (require 'badliveware-jump)
  (require 'badliveware-company)
  (require 'badliveware-org)


  ;; User interface
    ;; Do this last to prevent flickering
  (add-to-list 'load-path (concat my/config-dir "ui/"))
  (require 'badliveware-theme)
  (require 'badliveware-modeline)
  (require 'badliveware-sidebar)
  (require 'badliveware-tabs)
  (require 'badliveware-ui-misc)


  (toggle-frame-maximized)
  (when (not EMACS27+) (add-hook 'after-init-hook 'toggle-frame-fullscreen))

  (run-hooks 'after-bootstrap-hooks)

  (provide 'badliveware-bootstrap))
;;; badliveware-bootstrap.el ends here
