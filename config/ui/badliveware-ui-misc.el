;;; badliveware-ui-misc.el -*- lexical-binding: t; -*-
(require 'use-package)

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))


(use-package smartparens
    :custom
    (sp-show-pair-delay 0.25)
    ;; (sp-highlight-pair-overlay nil)
    ;; (sp-highlight-wrap-overlay nil)
    ;; (sp-highlight-wrap-tag-overlay nil)
    :config
    (show-paren-mode 0)
    (require 'smartparens-config)
    (show-smartparens-global-mode t)
    (smartparens-global-mode +1))


(use-package hl-line
    :preface (defun me/hl-line-mode-off () (setq-local global-hl-line-mode nil))
    :hook (after-init . global-hl-line-mode)
    :config
    (progn
        (defvar custom-buffer-hl-line-mode nil)

        (defun me/disable-hl-line ()
            (when hl-line-mode
                (setq-local custom-buffer-hl-line-mode t)
                (hl-line-mode -1)))
        (add-hook 'evil-visual-state-entry-hook #'me/disable-hl-line)

        (defun me/enable-hl-line-maybe ()
            (when custom-buffer-hl-line-mode
                (hl-line-mode +1)))

        (add-hook 'evil-visual-state-exit-hook  #'me/enable-hl-line-maybe)

        ;; Fixes weird bug where hl/solaire messes up on the last line of the buffer
        (defun doom--line-range ()
          (cons (line-beginning-position)
                (cond ((save-excursion
                         (goto-char (line-end-position))
                         (and (eobp) (not (bolp))))
                       (1- (line-end-position)))
                      ((or (eobp) (save-excursion (forward-line) (eobp)))
                       (line-end-position))
                      (t
                       (line-beginning-position 2)))))
        (setq hl-line-range-function #'doom--line-range)
        ))

;; (use-package vi-tilde-fringe
;;     :hook
;;     ((prog-mode text-mode conf-mode) 
;;         . vi-tilde-fringe-mode)
;;     :config
;;     (global-vi-tilde-fringe-mode))

(use-package rainbow-mode
  :hook prog-mode
  :custom (rainbow-x-colors-major-mode-list '()))

(use-package highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'fill)
  :config
  (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)

  (defun +indent-guides|disable-maybe ()
    (when highlight-indent-guides-mode
      (highlight-indent-guides-mode -1)))
  ;; `highlight-indent-guides' breaks in these modes
  (add-hook 'visual-line-mode-hook #'+indent-guides|disable-maybe)
  (add-hook 'org-indent-mode-hook #'+indent-guides|disable-maybe))

(use-package evil-goggles
  :init
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil ; too slow
        ;; evil-goggles provides a good indicator of what has been affected.
        ;; delete/change is obvious, so I'd rather disable it for these.
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
  :config
  :hook
  ((prog-mode text-mode org-mode) . evil-goggles-mode))
;; (evil-goggles-mode))


(provide 'badliveware-ui-misc)
;;; badliveware-ui-misc.el ends here
