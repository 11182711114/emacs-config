;;; badliveware-evil.el -*- lexical-binding: t; -*-

(require 'use-package)

(defvar +evil-want-o/O-to-continue-comments t
  "If non-nil, the o/O keys will continue comment lines if the point is on a
line with a linewise comment.")
(defun +evil--insert-newline (&optional above _noextranewline)
  (let ((pos (save-excursion (beginning-of-line-text) (point)))
        comment-auto-fill-only-comments)
    (require 'smartparens)
    (evil-narrow-to-field
      (if above
          (if (save-excursion (nth 4 (sp--syntax-ppss pos)))
              (evil-save-goal-column
                (setq evil-auto-indent nil)
                (goto-char pos)
                (let ((ws (abs (skip-chars-backward " \t"))))
                  ;; FIXME oh god why
                  (save-excursion
                    (if comment-line-break-function
                        (funcall comment-line-break-function)
                      (comment-indent-new-line))
                    (when (and (derived-mode-p 'c-mode 'c++-mode 'objc-mode 'java-mode 'js2-mode)
                               (eq (char-after) ?/))
                      (insert "*"))
                    (insert
                     (make-string (max 0 (+ ws (skip-chars-backward " \t")))
                                  32)))
                  (insert (make-string (max 1 ws) 32))))
            (evil-move-beginning-of-line)
            (insert (if use-hard-newlines hard-newline "\n"))
            (forward-line -1)
            (back-to-indentation))
        (evil-move-end-of-line)
        (cond ((sp-point-in-comment pos)
               (setq evil-auto-indent nil)
               (if comment-line-break-function
                   (funcall comment-line-break-function)
                 (comment-indent-new-line)))
              ;; TODO Find a better way to do this
              ((and (eq major-mode 'haskell-mode)
                    (fboundp 'haskell-indentation-newline-and-indent))
               (setq evil-auto-indent nil)
               (haskell-indentation-newline-and-indent))
              (t
               (insert (if use-hard-newlines hard-newline "\n"))
               (back-to-indentation)))))))

(defun +evil*insert-newline-below-and-respect-comments (orig-fn count)
  (if (or (not +evil-want-o/O-to-continue-comments)
          (not (eq this-command 'evil-open-below))
          (evil-insert-state-p))
      (funcall orig-fn count)
    (cl-letf (((symbol-function 'evil-insert-newline-below)
               (lambda () (+evil--insert-newline))))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall orig-fn count)))))

(defun +evil*insert-newline-above-and-respect-comments (orig-fn count)
  (if (or (not +evil-want-o/O-to-continue-comments)
          (not (eq this-command 'evil-open-above))
          (evil-insert-state-p))
      (funcall orig-fn count)
    (cl-letf (((symbol-function 'evil-insert-newline-above)
               (lambda () (+evil--insert-newline 'above))))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall orig-fn count)))))

(defun +evil*static-reindent (orig-fn &rest args)
  "Don't move cursor on indent."
  (save-excursion (apply orig-fn args)))

(defun +evil*resolve-vim-path (file-name)
  "Take a path and resolve any vim-like filename modifiers in it. This adds
support for most vim file modifiers, as well as:

  %:P   Resolves to `doom-project-root'.

See http://vimdoc.sourceforge.net/htmldoc/cmdline.html#filename-modifiers for
more information on modifiers."
  (let* (case-fold-search
         (regexp (concat "\\(?:^\\|[^\\\\]\\)"
                         "\\([#%]\\)"
                         "\\(\\(?::\\(?:[PphtreS~.]\\|g?s[^:\t\n ]+\\)\\)*\\)"))
         (matches
          (cl-loop with i = 0
                   while (and (< i (length file-name))
                              (string-match regexp file-name i))
                   do (setq i (1+ (match-beginning 0)))
                   and collect
                   (cl-loop for j to (/ (length (match-data)) 2)
                            collect (match-string j file-name)))))
    (dolist (match matches)
      (let ((flags (split-string (car (cdr (cdr match))) ":" t))
            (path (and buffer-file-name
                       (pcase (car (cdr match))
                         ("%" (file-relative-name buffer-file-name))
                         ("#" (save-excursion (other-window 1) (file-relative-name buffer-file-name))))))
            flag global)
        (if (not path)
            (setq path "")
          (while flags
            (setq flag (pop flags))
            (when (string-suffix-p "\\" flag)
              (setq flag (concat flag (pop flags))))
            (when (string-prefix-p "gs" flag)
              (setq global t
                    flag (substring flag 1)))
            (setq path
                  (or (pcase (substring flag 0 1)
                        ("p" (expand-file-name path))
                        ("~" (concat "~/" (file-relative-name path "~")))
                        ("." (file-relative-name path default-directory))
                        ("t" (file-name-nondirectory (directory-file-name path)))
                        ("r" (file-name-sans-extension path))
                        ("e" (file-name-extension path))
                        ("S" (shell-quote-argument path))
                        ("h"
                         (let ((parent (file-name-directory (expand-file-name path))))
                           (unless (equal (file-truename path)
                                          (file-truename parent))
                             (if (file-name-absolute-p path)
                                 (directory-file-name parent)
                               (file-relative-name parent)))))
                        ("s"
                         (if (featurep 'evil)
                             (when-let (args (evil-delimited-arguments (substring flag 1) 2))
                               (let ((pattern (evil-transform-vim-style-regexp (car args)))
                                     (replace (cadr args)))
                                 (replace-regexp-in-string
                                  (if global pattern (concat "\\(" pattern "\\).*\\'"))
                                  (evil-transform-vim-style-regexp replace) path t t
                                  (unless global 1))))
                           path))
                        ("P"
                         (let ((project-root (doom-project-root (file-name-directory (expand-file-name path)))))
                           (unless project-root
                             (user-error "Not in a project"))
                           (abbreviate-file-name project-root)))
                        (_ path))
                      "")))
          ;; strip trailing slash, if applicable
          (when (and (not (string= path "")) (equal (substring path -1) "/"))
            (setq path (substring path 0 -1))))
        (setq file-name
              (replace-regexp-in-string (format "\\(?:^\\|[^\\\\]\\)\\(%s\\)"
                                                (regexp-quote (string-trim-left (car match))))
                                        path file-name t t 1))))
    (replace-regexp-in-string regexp "\\1" file-name t)))

(with-eval-after-load 'evil (evil-define-command +evil*window-split (&optional count file)
                              "Same as `evil-window-split', but focuses (and recenters) the new split."
                              :repeat nil
                              (interactive "P<f>")
                              (split-window (selected-window) count
                                            (if evil-split-window-below 'above 'below))
                              (call-interactively
                               (if evil-split-window-below
                                   #'evil-window-up
                                 #'evil-window-down))
                              (recenter)
                              (when (and (not count) evil-auto-balance-windows)
                                (balance-windows (window-parent)))
                              (if file (evil-edit file))))

(with-eval-after-load 'evil (evil-define-command +evil*window-vsplit (&optional count file)
                              "Same as `evil-window-vsplit', but focuses (and recenters) the new split."
                              :repeat nil
                              (interactive "P<f>")
                              (split-window (selected-window) count
                                            (if evil-vsplit-window-right 'left 'right))
                              (call-interactively
                               (if evil-vsplit-window-right
                                   #'evil-window-left
                                 #'evil-window-right))
                              (recenter)
                              (when (and (not count) evil-auto-balance-windows)
                                (balance-windows (window-parent)))
                              (if file (evil-edit file))))

;;;###autoload
(defun +evil*escape (&rest _)
  "Call `doom/escape' if `evil-force-normal-state' is called interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'doom/escape)))

;;;###autoload
(defun +evil*make-numbered-markers-global (orig-fn char)
  (or (and (>= char ?2) (<= char ?9))
      (funcall orig-fn char)))

;;;###autoload
(defun +evil*fix-dabbrev-in-minibuffer ()
  "Make `try-expand-dabbrev' from `hippie-expand' work in minibuffer. See
`he-dabbrev-beg', so we need to redefine syntax for '/'."
  (set-syntax-table (let* ((table (make-syntax-table)))
                      (modify-syntax-entry ?/ "." table)
                      table)))

(use-package evil
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
        evil-visual-state-cursor 'hollow
        ;; must be set before evil/evil-collection is loaded
        evil-want-keybinding nil)

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


  ;; --- keybind fixes ----------------------
  (with-eval-after-load 'wgrep
    ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
    ;; `evil-delete' in wgrep buffers.
    (define-key wgrep-mode-map [remap evil-delete] #'+evil-delete))

  (defun +evil|disable-highlights ()
    "Disable ex search buffer highlights."
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight)
      t))
  (add-hook 'doom-escape-hook #'+evil|disable-highlights)


  ;; --- evil hacks -------------------------
  (defun +evil|display-vimlike-save-message ()
    "Shorter, vim-esque save messages."
    (message "\"%s\" %dL, %dC written"
             (if buffer-file-name
                 (file-relative-name (file-truename buffer-file-name) (projectile-project-root))
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
   '+evil:align :move-point t :ex-arg 'buffer-match :ex-bang t :keep-visual t :suppress-operator t)
  (evil-mode 1))

(use-package evil-collection
  :config
  (evil-collection-init))

(use-package evil-args
  :after evil)


(use-package evil-commentary
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-yank-line
             evil-commentary-line)
  :config (evil-commentary-mode))



(use-package evil-easymotion
  :commands (evilem-create evilem-default-keybindings)
  :config
  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))

  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil))))



(use-package evil-embrace
  :commands (embrace-add-pair embrace-add-pair-regexp)
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (org-mode . embrace-org-mode-hook)
  :hook ((ruby-mode enh-ruby-mode) . embrace-ruby-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  :hook ((lisp-mode emacs-lisp-mode clojure-mode racket-mode)
         . +evil|embrace-lisp-mode-hook)
  :hook ((org-mode LaTeX-mode) . +evil|embrace-latex-mode-hook)
  :hook ((c++-mode rust-mode rustic-mode csharp-mode java-mode swift-mode typescript-mode)
         . +evil|embrace-angle-bracket-modes-hook)
  :init
  (with-eval-after-load 'evil-surround
    (evil-embrace-enable-evil-surround-integration))
  :config
  (setq evil-embrace-show-help-p nil)

  (defun +evil|embrace-latex-mode-hook ()
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex))

  (defun +evil|embrace-lisp-mode-hook ()
    (push (cons ?f (make-embrace-pair-struct
                    :key ?f
                    :read-function #'+evil--embrace-elisp-fn
                    :left-regexp "([^ ]+ "
                    :right-regexp ")"))
          embrace--pairs-list))

  (defun +evil|embrace-angle-bracket-modes-hook ()
    (set (make-local-variable 'evil-embrace-evil-surround-keys)
         (delq ?< evil-embrace-evil-surround-keys))
    (push (cons ?< (make-embrace-pair-struct
                    :key ?<
                    :read-function #'+evil--embrace-angle-brackets
                    :left-regexp "\\[a-z]+<"
                    :right-regexp ">"))
          embrace--pairs-list))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]")))



(use-package evil-escape
  :after evil
  :commands (evil-escape)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp)
  ;; so that evil-escape-mode-hook runs, and can be toggled by evil-mc
  (evil-escape-mode +1))



(use-package evil-exchange
  :commands evil-exchange
  :config
  (defun +evil|escape-exchange ()
    (when evil-exchange--overlays
      (evil-exchange-cancel)
      t))
  (add-hook 'doom-escape-hook #'+evil|escape-exchange))



(use-package evil-indent-plus
  :after evil)



(use-package evil-numbers
  :straight (:host github :repo "janpath/evil-numbers"))



(use-package evil-textobj-anyblock
  :after evil)    



(use-package evil-snipe
  :commands (evil-snipe-mode evil-snipe-override-mode
                             evil-snipe-local-mode evil-snipe-override-local-mode)
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (pushnew! evil-snipe-disabled-modes 'Info-mode 'calc-mode)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))



(use-package evil-surround
  :commands 
  (global-evil-surround-mode
   evil-surround-edit
   evil-Surround-edit
   evil-surround-region)
  :config (global-evil-surround-mode 1))



(use-package evil-visualstar
  :after evil
  :commands
  (evil-visualstar/begin-search
   evil-visualstar/begin-search-forward
   evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))



(use-package exato
  :commands (evil-outer-xml-attr evil-inner-xml-attr))

  
(defun +evil/visual-indent ()
  "vnoremap < <gv"
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun +evil/visual-dedent ()
  "vnoremap > >gv"
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun +evil/paste-preserve-register ()
  "Call `evil-paste-after' without overwriting the clipboard (by writing to the
    0 register instead). This allows you to paste the same text again afterwards."
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively #'evil-paste-after)))


(with-eval-after-load 'evil (progn
                              (evil-define-operator +evil:open-scratch-buffer (bang)
                                (interactive "<!>")
                                (doom/open-scratch-buffer bang))

                              (evil-define-command +evil:pwd (bang)
                                "Display the current working directory. If BANG, copy it to your clipboard."
                                (interactive "<!>")
                                (if (not bang)
                                    (pwd)
                                  (kill-new default-directory)
                                  (message "Copied to clipboard")))

                              (evil-define-command +evil:make (arguments &optional bang)
                                "Run make with ARGUMENTS.
    If BANG is non-nil, open compilation output in a comint buffer.

    If BANG, then run ARGUMENTS as a full command. This command understands vim file
    modifiers (like %:p:h). See `+evil*resolve-vim-path' for details."
                                (interactive "<sh><!>")
                                (+evil:compile (format "make %s"
                                                       (evil-ex-replace-special-filenames
                                                        arguments))
                                               bang))

                              (evil-define-command +evil:compile (arguments &optional bang)
                                "Run `compile-command' with ARGUMENTS.
    If BANG is non-nil, open compilation output in a comint buffer.

    This command understands vim file modifiers (like %:p:h). See
    `+evil*resolve-vim-path' for details."
                                (interactive "<sh><!>")
                                (compile (evil-ex-replace-special-filenames
                                          (format "%s %s"
                                                  (eval compile-command)
                                                  arguments))
                                         bang))

                              (evil-define-command +evil:reverse-lines (beg end)
                                "Reverse lines between BEG and END."
                                (interactive "<r>")
                                (reverse-region beg end))

                              (evil-define-command +evil:cd (&optional path)
                                "Change `default-directory' with `cd'."
                                (interactive "<f>")
                                (let ((path (or path "~")))
                                  (cd path)
                                  (message "Changed directory to '%s'" (abbreviate-file-name (expand-file-name path)))))

                              (evil-define-command +evil:kill-all-buffers (&optional bang)
                                "Kill all buffers. If BANG, kill current session too."
                                (interactive "<!>")
                                (if (and bang (fboundp '+workspace/kill-session))
                                    (+workspace/kill-session)
                                  (doom/kill-all-buffers)))

                              (evil-define-command +evil:kill-matching-buffers (&optional bang pattern)
                                "Kill all buffers matching PATTERN regexp. If BANG, only match project
    buffers."
                                (interactive "<a>")
                                (doom/kill-matching-buffers pattern bang))

                              (evil-define-command +evil:help (&optional bang query)
                                "Look up help documentation for QUERY in Emacs documentation.

    If BANG, search Doom documentation."
                                (interactive "<!><a>")
                                (if bang
                                    (doom/help-search query)
                                  (cond ((or (null query) (string-empty-p (string-trim query)))
                                         (call-interactively
                                          (or (command-remapping #'apropos)
                                              #'apropos)))
                                        ((string-match-p "^ *:[a-z]" query)
                                         (let* ((modules
                                                 (cl-loop for path in (doom-module-load-path 'all)
                                                          for (cat . mod) = (doom-module-from-path path)
                                                          for format = (format "%s %s" cat mod)
                                                          if (doom-module-p cat mod)
                                                          collect (propertize format 'module (list cat mod))
                                                          else if (and cat mod)
                                                          collect (propertize format
                                                                              'face 'font-lock-comment-face
                                                                              'module (list cat mod))))
                                                (module (completing-read "Describe module: " modules nil t query))
                                                (key (get-text-property 0 'module module)))
                                           (doom/help-modules key)))
                                        ((and (string-match-p "\\(?:SPC\\|[CMsSH]-[^ ]\\|<[^>]+>\\)" query)
                                              (helpful-key (kbd (string-trim query)))))
                                        ((apropos query t)))))

    ;;; Custom commands
                              ;; Editing
                              (evil-ex-define-cmd "@"            #'+evil:macro-on-all-lines)   ; TODO Test me
                              (evil-ex-define-cmd "al[ign]"      #'+evil:align)
                              (evil-ex-define-cmd "ral[ign]"     #'+evil:align-right)
                              (evil-ex-define-cmd "enhtml"       #'+web:encode-html-entities)
                              (evil-ex-define-cmd "dehtml"       #'+web:decode-html-entities)
                              (evil-ex-define-cmd "mc"           #'+multiple-cursors:evil-mc)
                              (evil-ex-define-cmd "iedit"        #'evil-multiedit-ex-match)
                              (evil-ex-define-cmd "na[rrow]"     #'+evil:narrow-buffer)
                              (evil-ex-define-cmd "retab"        #'+evil:retab)
                              (evil-ex-define-cmd "rev[erse]"    #'+evil:reverse-lines)

    ;;; External resources
                              ;; TODO (evil-ex-define-cmd "db"          #'doom:db)
                              ;; TODO (evil-ex-define-cmd "dbu[se]"     #'doom:db-select)
                              ;; TODO (evil-ex-define-cmd "go[ogle]"    #'doom:google-search)
                              (evil-ex-define-cmd "lo[okup]"    #'+lookup:online)
                              (evil-ex-define-cmd "dash"        #'+lookup:dash)
                              (evil-ex-define-cmd "http"        #'httpd-start)            ; start http server
                              (evil-ex-define-cmd "repl"        #'+eval:repl)             ; invoke or send to repl
                              (evil-ex-define-cmd "h[elp]"      #'+evil:help)

                              ;; TODO (evil-ex-define-cmd "rx"          'doom:regex)             ; open re-builder
                              (evil-ex-define-cmd "sh[ell]"     #'+eshell:run)
                              (evil-ex-define-cmd "t[mux]"      #'+tmux:run)              ; send to tmux
                              (evil-ex-define-cmd "tcd"         #'+tmux:cd-here)          ; cd to default-directory in tmux
                              (evil-ex-define-cmd "pad"         #'+evil:open-scratch-buffer)

    ;;; GIT
                              (evil-ex-define-cmd "gist"        #'+gist:send)  ; send current buffer/region to gist
                              (evil-ex-define-cmd "gistl"       #'+gist:list)  ; list gists by user
                              (evil-ex-define-cmd "gbrowse"     #'+vc:git-browse)       ; show file/region in github/gitlab
                              (evil-ex-define-cmd "gissues"     #'forge-browse-issues)  ; show github issues
                              (evil-ex-define-cmd "git"         #'magit-status)         ; open magit status window
                              (evil-ex-define-cmd "gstage"      #'magit-stage)
                              (evil-ex-define-cmd "gunstage"    #'magit-unstage)
                              (evil-ex-define-cmd "gblame"      #'magit-blame)
                              (evil-ex-define-cmd "grevert"     #'git-gutter:revert-hunk)

    ;;; Dealing with buffers
                              (evil-ex-define-cmd "k[ill]"      #'doom/kill-current-buffer)
                              (evil-ex-define-cmd "k[ill]all"   #'+evil:kill-all-buffers)
                              (evil-ex-define-cmd "k[ill]m"     #'+evil:kill-matching-buffers)
                              (evil-ex-define-cmd "k[ill]o"     #'doom/kill-other-buffers)
                              (evil-ex-define-cmd "k[ill]b"     #'doom/kill-buried-buffers)
                              (evil-ex-define-cmd "l[ast]"      #'doom/popup-restore)
                              (evil-ex-define-cmd "m[sg]"       #'view-echo-area-messages)
                              (evil-ex-define-cmd "pop[up]"     #'doom/popup-this-buffer)

    ;;; Project navigation
                              (evil-ex-define-cmd "a"           #'projectile-find-other-file)
                              (evil-ex-define-cmd "cd"          #'+evil:cd)
                              (evil-ex-define-cmd "pwd"         #'+evil:pwd)

                              (evil-ex-define-cmd "ag"        #'+ivy:ag)
                              (evil-ex-define-cmd "agc[wd]"   #'+ivy:ag-from-cwd)
                              (evil-ex-define-cmd "rg"        #'+ivy:rg)
                              (evil-ex-define-cmd "rgc[wd]"   #'+ivy:rg-from-cwd)
                              (evil-ex-define-cmd "pt"        #'+ivy:pt)
                              (evil-ex-define-cmd "ptc[wd]"   #'+ivy:pt-from-cwd)
                              (evil-ex-define-cmd "grep"      #'+ivy:grep)
                              (evil-ex-define-cmd "grepc[wd]" #'+ivy:grep-from-cwd)
                              (evil-ex-define-cmd "sw[iper]"  #'+ivy:swiper)
                              (evil-ex-define-cmd "todo"      #'+ivy:todo)

    ;;; Project tools
                              (evil-ex-define-cmd "compile"     #'+evil:compile)
                              (evil-ex-define-cmd "mak[e]"      #'+evil:make)
                              (evil-ex-define-cmd "debug"       #'+debugger/start)
                              (evil-ex-define-cmd "er[rors]"    #'flycheck-list-errors)

    ;;; File operations
                              (evil-ex-define-cmd "cp"          #'+evil:copy-this-file)
                              (evil-ex-define-cmd "mv"          #'+evil:move-this-file)
                              (evil-ex-define-cmd "rm"          #'+evil:delete-this-file)

    ;;; Sessions/tabs
                              (evil-ex-define-cmd "sclear"      #'+workspace/kill-session)
                              (evil-ex-define-cmd "sl[oad]"     #'doom/quickload-session)
                              (evil-ex-define-cmd "ss[ave]"     #'doom/quicksave-session)
                              (evil-ex-define-cmd "tabc[lose]"  #'+workspace:delete)
                              (evil-ex-define-cmd "tabclear"    #'doom/kill-all-buffers)
                              (evil-ex-define-cmd "tabl[ast]"   #'+workspace/switch-to-last)
                              (evil-ex-define-cmd "tabload"     #'+workspace:load)
                              (evil-ex-define-cmd "tabn[ew]"    #'+workspace:new)
                              (evil-ex-define-cmd "tabn[ext]"   #'+workspace:switch-next)
                              (evil-ex-define-cmd "tabp[rev]"   #'+workspace:switch-previous)
                              (evil-ex-define-cmd "tabr[ename]" #'+workspace:rename)
                              (evil-ex-define-cmd "tabs"        #'+workspace/display)
                              (evil-ex-define-cmd "tabsave"     #'+workspace:save)

    ;;; Org-mode
                              (evil-ex-define-cmd "cap"         #'org-capture)))


(provide 'badliveware-evil)
;;; badliveware-evil.el ends here
