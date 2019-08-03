(setq ui-file "~/.emacs.d/ui.el")
(setq keybinds-file "~/.emacs.d/keybinds.el")

    ;; STARTUP
;; increase gc limit to decrease startup time
(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
;; start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

    ;; CONFIG

;;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; explicitly set the preferred coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
;;; Useful Defaults
(setq-default cursor-type 'bar)           ; Line-style cursor similar to other text editors
(setq inhibit-startup-screen t)           ; Disable startup screen
(setq initial-scratch-message "")         ; Make *scratch* buffer blank
(setq-default frame-title-format '("%b")) ; Make window title the buffer name
(setq ring-bell-function 'ignore)         ; Disable bell sound
(fset 'yes-or-no-p 'y-or-n-p)             ; y-or-n-p makes answering questions faster
(show-paren-mode 1)                       ; Show closing parens by default
;; (setq linum-format "%4d ")                ; Prettify line number format
;; (add-hook 'prog-mode-hook                 ; Show line numbers in programming modes
;;           (if (fboundp 'display-line-numbers-mode)
;;               #'display-line-numbers-mode
;;             #'linum-mode))
(use-package undo-tree                    ; Enable undo-tree, sane undo/redo behavior
  :init (global-undo-tree-mode))

(use-package evil)
(use-package evil-cleverparens)
(use-package evil-commentary)
(evil-mode 1)

;;; Avoid littering the user's filesystem with backups
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '((".*" . "~/.emacs.d/saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;; Lockfiles unfortunately cause more pain than benefit
(setq create-lockfiles nil)

    ;; UI
(load ui-file nil t)


    ;; PROJECT
(use-package projectile
  :ensure t
  :defer t
  :config
  (setq projectile-project-search-path '("E:\\code"))
  (projectile-add-known-project '"~/.emacs.d")
  (projectile-add-known-project '"E:/code/prolog/prop.assign3")
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package ivy
  :config
  (ivy-mode +1))


    ;; ORG


    ;; SOURCE CONTROL
(use-package magit)
(use-package magit-gitflow)
(use-package evil-magit)
(use-package forge)

    ;; EDITOR


    ;; LANGUAGES

(load keybinds-file nil t)

(message "emacs stated in %s" emacs-start-time)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (doom-themes use-package undo-tree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
