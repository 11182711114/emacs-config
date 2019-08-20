;;; badliveware-lsp.el --- lsp module -*- lexical-binding: t; -*-
;;; Commentary:
;;; Provides lsp functionality
;;; Code:
(require 'use-package)

(use-package lsp-mode
    :commands lsp)

(defun lsp! (&optional arg)
  "Enable `lsp-mode' in the current buffer.

Meant to be a lighter alternative to `lsp', which is too eager about
initializing lsp-ui-mode, company, yasnippet and flycheck. Instead, these have
been moved out to their respective modules, or these hooks:

+ `+lsp|init-company' (on `lsp-mode-hook')
+ `+lsp|init-ui-flycheck-or-flymake' (on `lsp-ui-mode-hook')"
  (require 'lsp-mode)
  (unless lsp-mode
    (when lsp-auto-configure
      (require 'lsp-clients))
    (when (and (buffer-file-name)
               (setq-local lsp--buffer-workspaces
                           (or (lsp--try-open-in-library-workspace)
                               (lsp--try-project-root-workspaces (equal arg '(4))
                                                                 (and arg (not (equal arg 1)))))))
      (lsp-mode 1)
      (lsp--info
       "Connected to %s."
       (apply #'concat (mapcar (lambda (it) (format "[%s]" (lsp--workspace-print it)))
                               lsp--buffer-workspaces))))))

(setq lsp-session-file (concat my/etc-dir "lsp-session")
      lsp-auto-guess-root t
      lsp-keep-workspace-alive nil
      lsp-groovy-server-install-dir (concat my/etc-dir "groovy-langserver/"))

(after! lsp-mode
  (when (featurep 'badliveware-lookup-lib)
    (set-lookup-handlers! 'lsp-mode
                          :async t
                          :documentation 'lsp-describe-thing-at-point
                          :definition 'lsp-find-definition
                          :references 'lsp-find-references))

  ;; The original `lsp' initializes too much, too quickly. Things like flycheck,
  ;; company, and yasnippet. Doom's modules already handle these just fine, so
  ;; leave it to us.
  (advice-add #'lsp :override #'lsp!)

  ;; Don't prompt to restart LSP servers while quitting Emacs
  (add-hook 'kill-emacs-hook (setq lsp-restart 'ignore)))


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (defun +lsp|init-ui-flycheck ()
    "Sets up flycheck-mode."
      (require 'lsp-ui-flycheck)
      (lsp-ui-flycheck-enable t))
  (add-hook 'lsp-ui-mode-hook #'+lsp|init-ui-flycheck)
  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t
        ;; lsp-ui-doc is redundant with and less invasive than
        ;; `+lookup/documentation'
        lsp-ui-doc-enable nil)

  (when (featurep 'badliveware-lookup-lib)
    (set-lookup-handlers! 'lsp-ui-mode :async t
                          :definition 'lsp-ui-peek-find-definitions
                          :references 'lsp-ui-peek-find-references)))


(use-package company-lsp
  :defer t
  :init
  ;; Make sure that `company-capf' is disabled since it is incompatible with
  ;; `company-lsp' (see lsp-mode#884)
  (defun +lsp|init-company ()
    (if (not (bound-and-true-p company-mode))
        (add-hook 'company-mode-hook #'+lsp|init-company t t)
      (setq-local company-backends
                  (cons 'company-lsp
                        (remq 'company-capf company-backends)))
      (remove-hook 'company-mode-hook #'+lsp|init-company t)))
  (add-hook 'lsp-mode-hook #'+lsp|init-company))

(provide 'badliveware-lsp)
;;; badliveware-lsp.el ends here
