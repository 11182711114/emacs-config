;;; badliveware-org.el -*- lexical-binding: t; -*-
(require 'use-package)

(use-package org
    :preface
    (defun me/org-src-buffer-name (org-buffer-name language)
        "Construct the buffer name for a source editing buffer. See
    `org-src--construct-edit-buffer-name'."
        (format "*%s*" org-buffer-name))
    (defun me/org-set-ongoing-hydra-body ()
        (setq me/ongoing-hydra-body #'hydra-org/body))
    :bind
    (:map org-mode-map
            ([remap backward-paragraph] . me/backward-paragraph-dwim)
            ([remap forward-paragraph] . me/forward-paragraph-dwim)
            ("<C-return>" . nil)
            ("<C-S-down>" . nil)
            ("<C-S-up>" . nil)
            ("<M-S-down>" . nil)
            ("<M-S-up>" . nil))
    :hook
    ((org-mode . me/org-set-ongoing-hydra-body)
    (org-mode . org-sticky-header-mode)
    (org-mode . toc-org-enable))
    :custom
    (org-descriptive-links nil)
    (org-edit-src-content-indentation 0)
    (org-edit-src-persistent-message nil)
    (org-fontify-done-headline t)
    (org-fontify-quote-and-verse-blocks t)
    (org-src-window-setup 'current-window)
    (org-startup-folded nil)
    (org-startup-truncated nil)
    (org-support-shift-select 'always)
    :config
    (advice-add 'org-src--construct-edit-buffer-name :override #'me/org-src-buffer-name))

(use-package org-sticky-header
  :custom
  (org-sticky-header-full-path 'full)
  (org-sticky-header-outline-path-separator " / ")
  :config
  (setq-default
   org-sticky-header-header-line-format
   '(:eval (setq org-sticky-header-stickyline (concat " " (org-sticky-header--fetch-stickyline))))))

(use-package toc-org :after org)

(provide 'badliveware-org)
;;; badliveware-org.el ends here
