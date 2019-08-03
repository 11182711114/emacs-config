;; Disable menu-bar, tool-bar, and scroll-bar.
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))


(use-package doom-themes)
;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-vibrant t)
;; (use-package solaire-mode
;;   :hook
;;   ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
;;   (minibuffer-setup . solaire-mode-in-minibuffer)
;;   :config
;;   (solaire-global-mode +1)
;;   (solaire-mode-swap-bg))


(use-package nlinum)
(setq nlinum-highlight-current-line t)
;; (global-hl-line-mode +1)


(use-package all-the-icons)
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))
(use-package evil-anzu)
(setq doom-modeline-lsp t)

(use-package winum)


(doom-themes-treemacs-config)
(use-package treemacs
    :ensure t
    :defer t
    :init
    (with-eval-after-load 'winum 
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
    :config
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))
(use-package treemacs-magit
  :after treemacs magit
  :ensure t)
(use-package treemacs-evil
  :after treemacs evil
  :ensure t)
(use-package treemacs-projectile
    :after treemacs projectile)
;; (treemacs-follow-mode t)
;; (treemacs-filewatch-mode t)




(use-package evil-goggles)
(setq evil-goggles-duration 0.1
        evil-goggles-pulse nil ; too slow
        ;; evil-goggles provides a good indicator of what has been affected.
        ;; delete/change is obvious, so I'd rather disable it for these.
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
(evil-goggles-mode +1)


(use-package hl-line
  ;; Highlights the current line
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  ;; I don't need hl-line showing in other windows. This also offers a small
  ;; speed boost when buffer is displayed in multiple windows.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Disable `hl-line' in evil-visual mode (temporarily). `hl-line' can make the
  ;; selection region harder to see while in evil visual mode.
  (with-eval-after-load 'evil
    (defvar custom-buffer-hl-line-mode nil)

    (defun custom|disable-hl-line ()
      (when hl-line-mode
        (setq-local doom-buffer-hl-line-mode t)
        (hl-line-mode -1)))
    (add-hook 'evil-visual-state-entry-hook #'custom|disable-hl-line)

    (defun custom|enable-hl-line-maybe ()
      (when custom-buffer-hl-line-mode
        (hl-line-mode +1)))
    (add-hook 'evil-visual-state-exit-hook  #'custom|enable-hl-line-maybe)))


(use-package ivy)
(use-package counsel)
(use-package counsel-projectile)
(use-package swiper)
(use-package all-the-icons-ivy)





(use-package dashboard
  :demand
  :config
  (progn 
    (dashboard-setup-startup-hook)
    (setq dashboard-center-content t
          dashboard-banner-logo-title "[ E M A C S ]"
          dashboard-startup-banner 'logo
          dashboard-set-heading-icons t
          dashboard-set-file-icons t
          dashboard-items '((agenda . 5)
                            (projects . 5)
                            (recents . 12)
                            (bookmarks . 8)))))
