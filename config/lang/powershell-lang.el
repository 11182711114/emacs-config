;;; powershell-lang.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'lsp)

(use-package powershell)

(use-package lsp-pwsh
  :straight (lsp-pwsh
             :host github
             :repo "kiennq/lsp-powershell")
  :hook (powershell-mode . lsp)
  :config
  (setq lsp-pwsh-cache-dir (concat my/cache-dir "pwsh/")
        lsp-pwsh-dir	   (concat my/etc-dir "pwsh/")))

(provide 'powershell-lang)
;;; powershell-lang.el ends here
