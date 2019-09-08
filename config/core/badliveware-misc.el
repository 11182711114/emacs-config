;;; badliveware-misc.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'badliveware-misc)
;;; badliveware-misc.el ends here
