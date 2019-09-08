;;; css-lang.el --- css module -*- lexical-binding: t; -*-
;;; Commentary:
;;; Provides css functionality
;;; Code:
(require 'use-package)
(require 'badliveware-company-lib)

(use-package css-mode
  :straight (:type built-in)
  :init 
  ;; An improved newline+continue comment function
  (setq-hook! css-mode comment-indent-function #'+css/comment-indent-new-line)
  :hook 
  ((css-mode sass-mode stylus-mode) . rainbow-mode)
  ((css-mode) . rainbow-delimiters-mode)
  ((css-mode sass-mode less-css-mode) . #'lsp!)
  :config
  (set-company-backend! '(css-mode scss-mode) 'company-capf)
  (map! :map scss-mode-map :localleader "b" #'+css/scss-build))

(use-package counsel-css
  :commands counsel-css
  :hook (css-mode . counsel-css-imenu-setup)
  :init
  (map! :map (css-mode-map scss-mode-map less-css-mode-map)
        :localleader ";" #'counsel-css))

(after! sass-mode
  (set-company-backend! 'sass-mode 'company-css)
  (map! :map sass-mode-map :localleader "b" #'+css/sass-build))

(map! :map (css-mode-map scss-mode-map less-css-mode-map)
      :localleader
      "rb" #'+css/toggle-inline-or-block)

(after! projectile
  (pushnew! projectile-other-file-alist
            '("css"  "scss" "sass" "less" "styl")
            '("scss" "css")
            '("sass" "css")
            '("less" "css")
            '("styl" "css")))

(provide 'css-lang)
;;; css-lang.el ends here
