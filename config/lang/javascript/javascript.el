;;; javascript.el --- javascript module -*- lexical-binding: t; -*-
;;; Commentary:
;;; Provides javascript functionality
;;; Code:
(require 'use-package)
(require 'badliveware-lookup-lib)
(require 'javascript-lib)
(require 'badliveware-lsp)

(use-package npm-mode)


;; (after! (:any js2-mode rjsx-mode web-mode)
;;   (set-pretty-symbols! '(js2-mode rjsx-mode web-mode)
;;     ;; Functional
;;     :def "function"
;;     :lambda "() =>"
;;     :composition "compose"
;;     ;; Types
;;     :null "null"
;;     :true "true" :false "false"
;;     ;; Flow
;;     :not "!"
;;     :and "&&" :or "||"
;;     :for "for"
;;     :return "return"
;;     ;; Other
;;     :yield "import")
;;     )

(after! projectile
  (pushnew! projectile-project-root-files "package.json")
  (pushnew! projectile-globally-ignored-directories "node_modules" "flow-typed"))


;;
;; Major modes

(use-package js2-mode
  :mode "\\.m?js\\'"
  :interpreter "node"
  :commands js2-line-break
  :config
  (setq js2-skip-preprocessor-directives t
        js-chain-indent t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-highlight-external-variables t)

  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
  ;; Indent switch-case another step
  (setq-hook! 'js2-mode-hook
    js-switch-indent-offset js2-basic-offset
    mode-name "JS2"))

(use-package rjsx-mode
  :mode "components/.+\\.js$"
  :init
  (defun +javascript-jsx-file-p ()
    "Detect React or preact imports early in the file."
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "js")
         (re-search-forward "\\(^\\s-*import +React\\|\\( from \\|require(\\)[\"']p?react\\)"
                            magic-mode-regexp-match-limit t)
         (progn (goto-char (match-beginning 1))
                (not (sp-point-in-string-or-comment)))))
  (add-to-list 'magic-mode-alist '(+javascript-jsx-file-p . rjsx-mode))
  :config
  (add-hook! 'rjsx-mode-hook
    ;; jshint doesn't know how to deal with jsx
    (push 'javascript-jshint flycheck-disabled-checkers))

  ;; `rjsx-electric-gt' relies on js2's parser to tell it when the cursor is in
  ;; a self-closing tag, so that it can insert a matching ending tag at point.
  ;; However, the parser doesn't run immediately, so a fast typist can outrun
  ;; it, causing tags to stay unclosed, so we force it to parse.
  (defun +javascript|reparse (n)
    ;; if n != 1, rjsx-electric-gt calls rjsx-maybe-reparse itself
    (if (= n 1) (rjsx-maybe-reparse)))
  (advice-add #'rjsx-electric-gt :before #'+javascript|reparse))


(use-package typescript-mode
  :hook
  ;; (typescript-mode . #'rainbow-delimiters-mode)
  (typescript-mode . #'lsp!)
  :config
  (setq-hook! 'typescript-mode-hook
    comment-line-break-function #'js2-line-break)
  (set-pretty-symbols! 'typescript-mode
    ;; Functional
    :def "function"
    :lambda "() =>"
    :composition "compose"
    ;; Types
    :null "null"
    :true "true" :false "false"
    :int "number"
    :str "string"
    :bool "boolean"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :return "return" :yield "import"))
;;
;;; Tools

(defun +javascript|init-lsp-or-tide-maybe ()
  "Start `lsp' or `tide' in the current buffer.

LSP will be used if the +lsp flag is enabled for :lang javascript AND if the
current buffer represents a file in a project.

If LSP fails to start (e.g. no available server or project), then we fall back
to tide."
  (let ((buffer-file-name (buffer-file-name (buffer-base-buffer))))
    (when (or (derived-mode-p 'js-mode 'typescript-mode)
              (and (eq major-mode 'web-mode)
                   (string= "tsx" (file-name-extension buffer-file-name))))
      (if (not buffer-file-name)
          ;; necessary because `tide-setup' and `lsp' will error if not a
          ;; file-visiting buffer
          (add-hook 'after-save-hook #'+javascript|init-tide-or-lsp-maybe nil 'local)
        (or (progn (lsp!) lsp-mode)
            ;; fall back to tide
            (if (executable-find "node")
                (and (require 'tide nil t)
                     (progn (tide-setup) tide-mode))
              (ignore
               (doom-log "Couldn't start tide because 'node' is missing"))))
        (remove-hook 'after-save-hook #'+javascript|init-tide-or-lsp-maybe 'local)))))

(add-hook! (js-mode typescript-mode web-mode) #'+javascript|init-lsp-or-tide-maybe)


(use-package xref-js2
  :after (:or js2-mode rjsx-mode)
  :config
  (set-lookup-handlers! '(js2-mode rjsx-mode)
    :xref-backend #'xref-js2-xref-backend))


(use-package js2-refactor
  :hook ((js2-mode rjsx-mode) . js2-refactor-mode)
  :config
    (let ((js2-refactor-mode-map (evil-get-auxiliary-keymap js2-refactor-mode-map 'normal t t)))
      (js2r-add-keybindings-with-prefix (format "%s r" doom-localleader-key))))


(use-package eslintd-fix
  :commands eslintd-fix
  :config
  (defun +javascript|set-flycheck-executable-to-eslint ()
    (setq flycheck-javascript-eslint-executable eslintd-fix-executable))
  (add-hook 'eslintd-fix-mode-hook #'+javascript|set-flycheck-executable-to-eslint))


;;;###package skewer-mode
(map! :localleader
      :prefix "s"
      (:after skewer-mode
        :map skewer-mode-map
        "E" #'skewer-eval-last-expression
        "e" #'skewer-eval-defun
        "f" #'skewer-load-buffer)

      (:after skewer-css
        :map skewer-css-mode-map
        "e" #'skewer-css-eval-current-declaration
        "r" #'skewer-css-eval-current-rule
        "b" #'skewer-css-eval-buffer
        "c" #'skewer-css-clear-all)

      (:after skewer-html
        :map skewer-html-mode-map
        "e" #'skewer-html-eval-tag))


;;;###package npm-mode
(map! :after npm-mode
      :localleader
      :map npm-mode-keymap
      :prefix "n"
      "n" #'npm-mode-npm-init
      "i" #'npm-mode-npm-install
      "s" #'npm-mode-npm-install-save
      "d" #'npm-mode-npm-install-save-dev
      "u" #'npm-mode-npm-uninstall
      "l" #'npm-mode-npm-list
      "r" #'npm-mode-npm-run
      "v" #'npm-mode-visit-project-file)


;;
;;; Projects

(def-project-mode! +javascript-npm-mode
  :modes (html-mode css-mode web-mode typescript-mode js2-mode rjsx-mode json-mode markdown-mode)
  :when (locate-dominating-file default-directory "package.json")
  :add-hooks (+javascript|add-node-modules-path npm-mode))

(def-project-mode! +javascript-gulp-mode
  :when (locate-dominating-file default-directory "gulpfile.js"))

(provide 'javascript)
;;; javascript.el ends here
