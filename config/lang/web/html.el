;;; html.el --- html module -*- lexical-binding: t; -*-
;;; Commentary:
;;; Provides html functionality
;;; Code:
(require 'use-package)
(require 'badliveware-company-lib)

(use-package web-mode
  :mode "\\.p?html?$"
  :mode "\\.\\(?:tpl\\|blade\\)\\(\\.php\\)?$"
  :mode "\\.erb$"
  :mode "\\.jsp$"
  :mode "\\.as[cp]x$"
  :mode "\\.hbs$"
  :mode "\\.mustache$"
  :mode "\\.tsx$"
  :mode "\\.vue$"
  :mode "\\.twig$"
  :mode "\\.jinja$"
  :mode "wp-content/themes/.+/.+\\.php$"
  :mode "templates/.+\\.php$"
  :config
  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 1)

  (after! smartparens
    (defun +web-is-auto-close-style-3 (_id action _context)
      (and (eq action 'insert)
           (eq web-mode-auto-close-style 3)))
    (sp-local-pair 'web-mode "<" ">" :unless '(:add +web-is-auto-close-style-3))

    ;; let smartparens handle these
    (setq web-mode-enable-auto-quoting nil
          web-mode-enable-auto-pairing t)

    ;; 1. Remove web-mode auto pairs whose end pair starts with a latter
    ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
    ;;    better.
    ;; 2. Strips out extra closing pairs to prevent redundant characters
    ;;    inserted by smartparens.
    (dolist (alist web-mode-engines-auto-pairs)
      (setcdr alist
              (cl-loop for pair in (cdr alist)
                       unless (string-match-p "^[a-z-]" (cdr pair))
                       collect (cons (car pair)
                                     ;; TODO Replace with `string-trim-right' (Emacs 26+)
                                     (let ((string (cdr pair)))
                                       (if (string-match "\\(?:>\\|]\\|}\\)+\\'" string)
                                           (replace-match "" t t string)
                                         string))))))
    (delq! nil web-mode-engines-auto-pairs))

  (map! :map web-mode-map
        (:localleader
          :desc "Rehighlight buffer" "h" #'web-mode-buffer-highlight
          :desc "Indent buffer"      "i" #'web-mode-buffer-indent

          (:prefix "a"
            "b" #'web-mode-attribute-beginning
            "e" #'web-mode-attribute-end
            "i" #'web-mode-attribute-insert
            "n" #'web-mode-attribute-next
            "s" #'web-mode-attribute-select
            "k" #'web-mode-attribute-kill
            "p" #'web-mode-attribute-previous
            "p" #'web-mode-attribute-transpose)

          (:prefix "b"
            "b" #'web-mode-block-beginning
            "c" #'web-mode-block-close
            "e" #'web-mode-block-end
            "k" #'web-mode-block-kill
            "n" #'web-mode-block-next
            "p" #'web-mode-block-previous
            "s" #'web-mode-block-select)

          (:prefix "d"
            "a" #'web-mode-dom-apostrophes-replace
            "d" #'web-mode-dom-errors-show
            "e" #'web-mode-dom-entities-encode
            "n" #'web-mode-dom-normalize
            "q" #'web-mode-dom-quotes-replace
            "t" #'web-mode-dom-traverse
            "x" #'web-mode-dom-xpath)

          (:prefix "e"
            "/" #'web-mode-element-close
            "a" #'web-mode-element-content-select
            "b" #'web-mode-element-beginning
            "c" #'web-mode-element-clone
            "d" #'web-mode-element-child
            "e" #'web-mode-element-end
            "f" #'web-mode-element-children-fold-or-unfold
            "i" #'web-mode-element-insert
            "k" #'web-mode-element-kill
            "m" #'web-mode-element-mute-blanks
            "n" #'web-mode-element-next
            "p" #'web-mode-element-previous
            "r" #'web-mode-element-rename
            "s" #'web-mode-element-select
            "t" #'web-mode-element-transpose
            "u" #'web-mode-element-parent
            "v" #'web-mode-element-vanish
            "w" #'web-mode-element-wrap)

          (:prefix "t"
            "a" #'web-mode-tag-attributes-sort
            "b" #'web-mode-tag-beginning
            "e" #'web-mode-tag-end
            "m" #'web-mode-tag-match
            "n" #'web-mode-tag-next
            "p" #'web-mode-tag-previous
            "s" #'web-mode-tag-select))

        :g  "M-/" #'web-mode-comment-or-uncomment
        :i  "SPC" #'self-insert-command
        :n  "za"  #'web-mode-fold-or-unfold
        :nv "]a"  #'web-mode-attribute-next
        :nv "[a"  #'web-mode-attribute-previous
        :nv "]t"  #'web-mode-tag-next
        :nv "[t"  #'web-mode-tag-previous
        :nv "]T"  #'web-mode-element-child
        :nv "[T"  #'web-mode-element-parent))


(use-package company-web
    :after web-mode)

(after! web-mode
  (set-company-backend! 'web-mode 'company-web-html))
(add-hook! (html-mode web-mode) #'lsp!)

(provide 'html)
;;; html.el ends here
