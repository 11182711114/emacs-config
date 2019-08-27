;;; badliveware-magit.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'general)
(require 'evil)
(require 'badliveware-magit-util)

(setq use-magit-commit-prompt-p nil)
(defun use-magit-commit-prompt (&rest args)
  (setq use-magit-commit-prompt-p t))

(defun magit-commit-prompt ()
  "Magit prompt and insert commit header with faces."
  (interactive)
  (when use-magit-commit-prompt-p
    (setq use-magit-commit-prompt-p nil)
    (insert (ivy-read "Commit Type " pretty-magit-prompt
                      :require-match t :sort t :preselect "Add: "))
    (add-magit-faces)
    (evil-insert 1)  ; If you use evil
    ))

(remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
(add-hook 'git-commit-setup-hook 'magit-commit-prompt)
(advice-add 'magit-commit :after 'use-magit-commit-prompt)

(use-package git-commit
  :preface
  (defun me/git-commit-auto-fill-everywhere ()
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil))
  :hook
  (git-commit-mode . me/git-commit-auto-fill-everywhere)
  :custom
  (git-commit-summary-max-length 50))

(use-package magit
  :bind
  (:map magit-hunk-section-map
    ("RET" . magit-diff-visit-file-other-window)
    ([return] . magit-diff-visit-file-other-window))
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-highlight-hunk-body nil)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside magit-diff-highlight-hunk-region-using-face))
  (magit-popup-display-buffer-action '((display-buffer-same-window)))
  (magit-refs-show-commit-count 'all)
  (magit-section-show-child-count t)
  :config
  (remove-hook 'magit-section-highlight-hook #'magit-section-highlight)
  (setq transient-default-level 5
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-diff-refine-hunk t) ; show granular diffs in selected hunk

  ;; Magit uses `magit-display-buffer-traditional' to display windows, by
  ;; default, which is a little primitive. `+magit-display-buffer' marries
  ;; `magit-display-buffer-fullcolumn-most-v1' with
  ;; `magit-display-buffer-same-window-except-diff-v1', except:
  ;;
  ;; 1. Magit sub-buffers (like `magit-log') that aren't spawned from a status
  ;;    screen are opened as popups.
  ;; 2. The status screen isn't buried when viewing diffs or logs from the
  ;;    status screen.
  (setq transient-display-buffer-action '(display-buffer-below-selected)
        magit-display-buffer-function #'+magit-display-buffer)
  ;;   (set-popup-rule! "^\\(?:\\*magit\\|magit:\\| \\*transient\\*\\)" :ignore t)

  ;; Add --tags switch
  (transient-append-suffix 'magit-fetch
                           "-p" '("-t" "Fetch all tags" ("-t" "--tags")))

  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'+magit/quit)

  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one))


(use-package evil-magit
  :init
  (setq evil-magit-state 'normal
        evil-magit-use-z-for-folds t)
  :config
  (unmap! magit-mode-map "M-1" "M-2" "M-3" "M-4") ; replaced by z1, z2, z3, etc
  (evil-define-key* 'normal magit-status-mode-map [escape] nil) ; q is enough
  (evil-define-key* '(normal visual) magit-mode-map
    "zz" #'evil-scroll-line-to-center
    "%"  #'magit-gitflow-popup)
  (define-key! 'normal
    (magit-status-mode-map
     magit-stash-mode-map
     magit-revision-mode-map
     magit-diff-mode-map)
    [tab] #'magit-section-toggle)
  (with-eval-after-load 'git-rebase
    (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
      (when-let (desc (assoc (car key) evil-magit-rebase-commands-w-descriptions))
        (setcar desc (cdr key))))
    (evil-define-key* evil-magit-state git-rebase-mode-map
      "gj" #'git-rebase-move-line-down
      "gk" #'git-rebase-move-line-up)))


(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

(provide 'badliveware-magit)
;;; badliveware-magit.el ends here
