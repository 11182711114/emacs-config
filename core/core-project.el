;;; core-project.el -*- lexical-binding: t; -*-

(require 'core-package)

(use-package projectile
  :ensure t
  :defer t
  :config
  (setq projectile-project-search-path '("E:\\code")
	projectile-completion-system 'ivy
	projectile-enable-caching t)
  (projectile-add-known-project '"~/.emacs.d")
  (projectile-add-known-project '"E:/code/prolog/prop.assign3")
  (projectile-mode +1))


  (provide 'core-project)