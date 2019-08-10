;;; core-evil.el -*- lexical-binding: t; -*-

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil);; must be set before evil/evil-collection is loaded
  :preface
  (setq evil-want-visual-char-semi-exclusive t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        evil-mode-line-format 'nil
        evil-respect-visual-line-mode t
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; cursor appearance
        evil-default-cursor '+evil-default-cursor
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(box +evil-emacs-cursor)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  
  (put 'evil-define-key* 'lisp-indent-function 'defun)

  ;; Start help-with-tutorial in emacs state
  (advice-add #'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))

    ;; Change the cursor color in emacs state. We do it this roundabout way
  ;; instead of changing `evil-default-cursor' (or `evil-emacs-state-cursor') so
  ;; it won't interfere with users who have changed these variables.
  (defvar +evil--default-cursor-color "#ffffff")
  (defvar +evil--emacs-cursor-color "#ff9999")

  (defun +evil|update-cursor-color ()
    (setq +evil--default-cursor-color (face-background 'cursor)
          +evil--emacs-cursor-color (face-foreground 'warning)))
  (add-hook 'doom-load-theme-hook #'+evil|update-cursor-color)

  (defun +evil-default-cursor ()
    (evil-set-cursor-color +evil--default-cursor-color))
  (defun +evil-emacs-cursor ()
    (evil-set-cursor-color +evil--emacs-cursor-color))

  (defun +evil|update-shift-width ()
    (setq evil-shift-width tab-width))
  (add-hook 'after-change-major-mode-hook #'+evil|update-shift-width)
    ;; --- evil hacks -------------------------
  (defun +evil|display-vimlike-save-message ()
    "Shorter, vim-esque save messages."
    (message "\"%s\" %dL, %dC written"
             (if buffer-file-name
                 (file-relative-name (file-truename buffer-file-name) (doom-project-root))
               (buffer-name))
             (count-lines (point-min) (point-max))
             (buffer-size)))
  (unless noninteractive
    (setq save-silently t)
    (add-hook 'after-save-hook #'+evil|display-vimlike-save-message))
  ;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
  (advice-add #'evil-force-normal-state :after #'+evil*escape)
  ;; Don't move cursor when indenting
  (advice-add #'evil-indent :around #'+evil*static-reindent)
  ;; monkey patch `evil-ex-replace-special-filenames' to improve support for
  ;; file modifiers like %:p:h. This adds support for most of vim's modifiers,
  ;; and one custom one: %:P (expand to the project root).
  (advice-add #'evil-ex-replace-special-filenames :override #'+evil*resolve-vim-path)

  ;; make `try-expand-dabbrev' (from `hippie-expand') work in minibuffer
  (add-hook 'minibuffer-inactive-mode-hook #'+evil*fix-dabbrev-in-minibuffer)

  ;; Focus and recenter new splits
  (advice-add #'evil-window-split  :override #'+evil*window-split)
  (advice-add #'evil-window-vsplit :override #'+evil*window-vsplit)

  ;; In evil, registers 2-9 are buffer-local. In vim, they're global, so...
  (advice-add #'evil-global-marker-p :around #'+evil*make-numbered-markers-global)

  ;; Make o/O continue comments (see `+evil-want-o/O-to-continue-comments')
  (advice-add #'evil-open-above :around #'+evil*insert-newline-above-and-respect-comments)
  (advice-add #'evil-open-below :around #'+evil*insert-newline-below-and-respect-comments)

  ;; ;; Recenter screen after most searches
  ;; (advice-add! '(evil-visualstar/begin-search-forward
  ;;                evil-visualstar/begin-search-backward
  ;;                evil-ex-search-word-backward
  ;;                evil-ex-search-word-backward
  ;;                evil-ex-search-forward
  ;;                evil-ex-search-backward)
  ;;              :after #'doom*recenter)

  ;; --- custom interactive codes -----------
  ;; These arg types will highlight matches in the current buffer
  (evil-ex-define-argument-type buffer-match :runner +evil-ex-buffer-match)
  (evil-ex-define-argument-type global-match :runner +evil-ex-global-match)
  ;; Other commands can make use of this
  (evil-define-interactive-code "<//>"
    :ex-arg buffer-match (list (if (evil-ex-p) evil-ex-argument)))
  (evil-define-interactive-code "<//g>"
    :ex-arg global-match (list (if (evil-ex-p) evil-ex-argument)))

  ;; By default :g[lobal] doesn't highlight matches in the current buffer. I've
  ;; got to write my own argument type and interactive code to get it to do so.
  (evil-ex-define-argument-type global-delim-match :runner +evil-ex-global-delim-match)
  (dolist (sym '(evil-ex-global evil-ex-global-inverted))
    (evil-set-command-property sym :ex-arg 'global-delim-match))

  ;; Forward declare these so that ex completion works, even if the autoloaded
  ;; functions aren't loaded yet.
  (evil-set-command-properties
   '+evil:align :move-point t :ex-arg 'buffer-match :ex-bang t :keep-visual t :suppress-operator t))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-escape
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp)
  ;; so that evil-escape-mode-hook runs, and can be toggled by evil-mc
  (evil-escape-mode +1))

(use-package evil-cleverparens
  :after evil
  :ensure t
  :config
  (add-hook 'lisp-mode #'evil-cleverparens-mode))

(use-package evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode 1))


(provide 'core-evil)