;;; badliveware-core.el -*- lexical-binding: t; -*-

(require 'badliveware-constants)
(require 'use-package)

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history nil ; disable because unstable
        ;; undo-in-region is known to cause undo history corruption, which can
        ;; be very destructive! Disabling it deters the error, but does not fix
        ;; it entirely!
        undo-tree-enable-undo-in-region nil
        undo-tree-history-directory-alist
        `(("." . ,(concat my/cache-dir "undo-tree-hist/"))))
  (global-undo-tree-mode +1))

(provide 'badliveware-core)
;;; badliveware-core.el ends here
