;;; core-init.el -*- lexical-binding: t; -*-

(require 'core-package)

;; explicitly set the preferred coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
;; Show closing parens by default
(show-paren-mode 1)
;; Enable undo-tree, sane undo/redo behavior
(use-package undo-tree                    
  :config (global-undo-tree-mode))
                       
(use-package no-littering)
;;; Avoid littering the user's filesystem with backups
(setq
; don't clobber symlinks
   backup-by-copying t      
   backup-directory-alist
   ; don't litter my fs tree
    '((".*" . "~/.emacs.d/saves/"))    
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   ; use versioned backups
   version-control t       
;;; Lockfiles unfortunately cause more pain than benefit
   create-lockfiles nil)


(require 'core-ui)
(require 'core-evil)
(require 'core-keybinds)
(require 'core-project)

(provide 'core-init)