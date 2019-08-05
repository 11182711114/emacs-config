;;; init.el -*- lexical-binding: t; -*-
;;; Commentary:


;;; Code:


;; STARTUP
;; increase gc limit to decrease startup time
(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
;; start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defvar core-dir "~/.emacs.d/core/")
(add-to-list 'load-path core-dir)
(require 'core-init)

(use-package ivy
  :config
  (ivy-mode +1))


    ;; SOURCE CONTROL
(use-package magit)
(use-package magit-gitflow
  :after magit)
(use-package evil-magit
  :after evil magit)
(use-package forge
  :after magit)
(use-package gitignore-mode
  :mode ("\\.gitignore\\'")
  :ensure t)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (gitignore-mode forge evil-magit magit-gitflow general which-key flycheck all-the-icons-ivy counsel-projectile counsel ivy evil-goggles dashboard treemacs-projectile treemacs-evil treemacs-magit with-editor winum use-package treemacs transient solaire-mode no-littering nlinum evil-commentary evil-collection evil-cleverparens evil-anzu doom-themes doom-modeline))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
