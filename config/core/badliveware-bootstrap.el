;;; badliveware-bootstrap.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(let ()
  (defvar after-bootstrap-hook nil)

  (require 'badliveware-constants (concat user-emacs-directory "config/core/badliveware-constants"))

  ;; Load paths
  (add-to-list 'load-path (concat my/config-dir "core/libs/"))
  (add-to-list 'load-path (concat my/config-dir "core/"))
  (add-to-list 'load-path (concat my/config-dir "core/ivy/"))

  (add-to-list 'load-path (concat my/config-dir "editor/"))

  (add-to-list 'load-path (concat my/config-dir "tools/"))
  (add-to-list 'load-path (concat my/config-dir "tools/company/"))
  (add-to-list 'load-path (concat my/config-dir "tools/project/"))
  (add-to-list 'load-path (concat my/config-dir "tools/magit/"))

  (add-to-list 'load-path (concat my/config-dir "lang/"))
  (add-to-list 'load-path (concat my/config-dir "lang/web/"))
  (add-to-list 'load-path (concat my/config-dir "lang/javascript/"))

  (add-to-list 'load-path (concat my/config-dir "ui/"))

  ;; Core
  (require 'badliveware-defaults)
  (require 'badliveware-package)
  (require 'badliveware-core)
  (require 'badliveware-os)
  (require 'badliveware-lib)
  (require 'badliveware-keybinds)
  (require 'badliveware-ivy)
  (require 'badliveware-font)
  (require 'badliveware-misc)

  ;; Core/lib
  (require 'file-lib)

  ;; Editor
  (require 'badliveware-evil)

  ;; Tools
  (require 'badliveware-projectile)
  (require 'badliveware-flycheck)
  (require 'badliveware-magit)
  (require 'badliveware-dired)
  (require 'badliveware-whitespace)
  (require 'badliveware-jump)
  (require 'badliveware-company)
  (require 'badliveware-org)
  (require 'badliveware-lsp)

  ;; Languages
  (require 'web-lang)
  (require 'powershell-lang)
  (require 'json-lang)
  (require 'yaml-lang)

  ;; User interface - Do this last to prevent flickering
  (require 'badliveware-theme)
  (require 'badliveware-modeline)
  (require 'badliveware-sidebar)
  (require 'badliveware-tabs)
  (require 'badliveware-ui-misc)


  (require 'badliveware-evil-keybinds)
  (toggle-frame-maximized)
  (when (not EMACS27+) (add-hook 'after-init-hook 'toggle-frame-fullscreen))

  (run-hooks 'after-bootstrap-hook)

  (defun display-startup-echo-area-message () "Test")
  (provide 'badliveware-bootstrap))
;;; badliveware-bootstrap.el ends here
