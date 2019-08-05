;;; core-evil.el -*- lexical-binding: t; -*-

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
(use-package evil-cleverparens
  :after evil
  :ensure t
  :config
  (add-hook 'lisp-mode #'evil-cleverparens-mode))
(use-package evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode))


(provide 'core-evil)