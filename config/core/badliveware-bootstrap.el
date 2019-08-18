;;; badliveware-bootstrap.el -*- lexical-binding: t; -*-

(let ()
  (defvar after-bootstrap-hook nil)

  (require 'badliveware-constants (concat user-emacs-directory "config/core/badliveware-constants"))

  ;; Load paths
  (add-to-list 'load-path (concat my/config-dir "core/"))
  (add-to-list 'load-path (concat my/config-dir "core/ivy/"))
  (add-to-list 'load-path (concat my/config-dir "ui/"))
  (add-to-list 'load-path (concat my/config-dir "editor/"))
  (add-to-list 'load-path (concat my/config-dir "tools/"))
  (add-to-list 'load-path (concat my/config-dir "tools/project/"))

  ;; Core
  (require 'badliveware-defaults)
  (require 'badliveware-package)
  (require 'badliveware-os)
  (require 'badliveware-lib)
  (require 'badliveware-keybinds)
  (require 'badliveware-ivy)
  (require 'badliveware-font)

  ;; Editor
  (require 'badliveware-evil)
  (require 'badliveware-evil-keybinds)

  ;; Tools
  (require 'badliveware-projectile)
  (require 'badliveware-flycheck)
  (require 'badliveware-magit)
  (require 'badliveware-dired)
  (require 'badliveware-whitespace)
  (require 'badliveware-jump)
  (require 'badliveware-company)
  (require 'badliveware-org)

  ;; User interface - Do this last to prevent flickering
  (require 'badliveware-theme)
  (require 'badliveware-modeline)
  (require 'badliveware-sidebar)
  (require 'badliveware-tabs)
  (require 'badliveware-ui-misc)


  (toggle-frame-maximized)
  (when (not EMACS27+) (add-hook 'after-init-hook 'toggle-frame-fullscreen))

  (run-hooks 'after-bootstrap-hook)

  (defun display-startup-echo-area-message () "Test")
  (provide 'badliveware-bootstrap))
;;; badliveware-bootstrap.el ends here
