;;; core-ui.el -*- lexical-binding: t; -*-

(require 'core-package)

;; Disable menu-bar, tool-bar, and scroll-bar.
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(setq-default
  ansi-color-for-comint-mode t
  ; disable bidirectional text for tiny performance boost
  bidi-display-reordering nil 
  ; don't blink--too distracting
  blink-matching-paren nil    
  ; kill compilation process before starting another
  compilation-always-kill t        
     ; save all buffers on `compile'
  compilation-ask-about-save nil
  compilation-scroll-output 'first-error
  confirm-nonexistent-file-or-buffer t
  ; hide cursors in other windows
  cursor-in-non-selected-windows nil 
  display-line-numbers-width 3
  echo-keystrokes 0.02
  enable-recursive-minibuffers nil
  frame-inhibit-implied-resize t
  ;; remove continuation arrow on right fringe
  fringe-indicator-alist
  (delq (assq 'continuation fringe-indicator-alist)
        fringe-indicator-alist)
  highlight-nonselected-windows nil
  image-animate-loop t
  indicate-buffer-boundaries nil
  indicate-empty-lines nil
  max-mini-window-height 0.3
   ; disable mode-line mouseovers
  mode-line-default-help-echo nil
  ; middle-click paste at point, not at click
  mouse-yank-at-point t    
  ; hide :help-echo text       
  show-help-function nil  
  ; always avoid GUI        
  use-dialog-box nil              
  uniquify-buffer-name-style 'forward
  visible-cursor nil
  x-stretch-cursor nil
  ;; Favor vertical splits
  split-width-threshold 160
  split-height-threshold nil
  ;; `pos-tip' defaults
  pos-tip-internal-border-width 6
  pos-tip-border-width 1
  ;; no beeping or blinking please
  ring-bell-function #'ignore
  visible-bell nil
  ;; don't resize emacs in steps, it looks weird
  window-resize-pixelwise t
  frame-resize-pixelwise t)

;; y/n instead of yes/no
(fset #'yes-or-no-p #'y-or-n-p)
;; Truly silence startup message
(fset #'display-startup-echo-area-message #'ignore)
;; relegate tooltips to echo area only
(if (bound-and-true-p tooltip-mode) (tooltip-mode -1))
;; enabled by default; no thanks, too distracting
(blink-cursor-mode -1)
;; Prevent the glimpse of un-styled Emacs by setting these early.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; don't resize minibuffer for large text
(setq resize-mini-windows nil)
;; Except when it's asking for input
;; (setq-hook! 'minibuffer-setup-hook resize-mini-windows 'grow-only)
(add-hook 'minibuffer-setup-hook resize-mini-windows)
;; (add-hook 'minibuffer-setup-hook 'grow-only)

 ;; Use `show-trailing-whitespace' instead of `whitespace-mode' because it's
;; faster (implemented in C). But try to only enable it in editing buffers.
(setq-default show-trailing-whitespace nil)
(add-hook 'prog-mode-hook show-trailing-whitespace t)
(add-hook 'text-mode-hook show-trailing-whitespace t)
(add-hook 'conf-mode-hook show-trailing-whitespace t)


(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)) ; if nil, italics is universally disabled
;; Global settings (defaults)
(use-package all-the-icons)
(use-package doom-modeline
      :ensure t
      :hook 
      (after-init . doom-modeline-mode)
      :config
      (setq doom-modeline-lsp t))


;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-vibrant t)
(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))



(use-package evil-anzu
  :defer t
  :after evil anzu)

(use-package nlinum
  :config  
  (setq nlinum-highlight-current-line t))
;; (global-hl-line-mode +1)

(use-package winum)

(use-package treemacs
    :ensure t
    :defer t
    :config
    (doom-themes-treemacs-config)
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

(use-package dashboard
  :preface
  (defvar my/logos '(1 2 3 ));;'logo 'emacs))
  (defun my/random-dashboard-logo ()
    "Gets a random logo to use for dashboard."
    (nth (random (length my/logos)) my/logos))
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner (my/random-dashboard-logo)
	dashboard-banner-logo-title "[ E M A C S ]"
	dashboard-center-content t
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-set-init-info t
	dashboard-items '((agenda . 5)
			    (projects . 5)
			    (bookmarks . 8)
			    (recents . 12))))


(use-package evil-goggles
  :config
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil ; too slow
        ;; evil-goggles provides a good indicator of what has been affected.
        ;; delete/change is obvious, so I'd rather disable it for these.
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
  (evil-goggles-mode +1))


(use-package hl-line
  ;; Highlights the current line
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  ;; I don't need hl-line showing in other windows. This also offers a small
  ;; speed boost when buffer is displayed in multiple windows.
  ;; (setq hl-line-sticky-flag nil
  ;;       global-hl-line-sticky-flag nil)

  ;; Disable `hl-line' in evil-visual mode (temporarily). `hl-line' can make the
  ;; selection region harder to see while in evil visual mode.
  :config
  (progn
    (defvar custom-buffer-hl-line-mode nil)

    (defun custom|disable-hl-line ()
      (when hl-line-mode
        (setq-local custom-buffer-hl-line-mode t)
        (hl-line-mode -1)))
    (add-hook 'evil-visual-state-entry-hook #'custom|disable-hl-line)

    (defun custom|enable-hl-line-maybe ()
      (when custom-buffer-hl-line-mode
        (hl-line-mode +1)))
    (add-hook 'evil-visual-state-exit-hook  #'custom|enable-hl-line-maybe)))

(use-package ivy
  :defer t)
(use-package counsel
  :defer t)
(use-package counsel-projectile
  :defer t
  :after counsel projectile)
(use-package swiper
  :defer t)
(use-package all-the-icons-ivy
  :defer t
  :after ivy)


(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(provide 'core-ui)
;;; core-ui.el ends here