;;; yaml-lang.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'lsp-mode)

(use-package yaml-mode
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")

(use-package lsp-yaml
  :straight (lsp-yaml
             :host github
             :repo "iquiw/lsp-yaml")
  :after lsp
  :init
  (setq lsp-yaml-server "C:/ProgramData/nvm/v11.14.0/node_modules/yaml-language-server/bin/yaml-language-server")
  :hook
  (yaml-mode . lsp)
  :config
  (setq lsp-yaml-format-enable t
        lsp-yaml-schemas '(:kubernetes "/*-k8s.yaml")))

;; (add-to-list 'lsp-language-id-configuration '(yaml-mode . "yaml"))
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection '("yaml-language-server --st"))
;;                   :major-modes '(yaml-mode)
;;                   :priority -1
;;                   :server-id 'yaml-ls
;;                   :initialized-fn (lambda (workspace)
;;                                     (with-lsp-workspace workspace
;;                                       (lsp--set-configuration)))))

(provide 'yaml-lang)
;;; yaml-lang.el ends here
