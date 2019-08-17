;;; badliveware-theme.el -*- lexical-binding: t; -*-
(defvar load-theme-hook '())
(use-package doom-themes
    :demand t
    :custom
    (doom-themes-enable-bold t)
    (doom-themes-enable-italic t)
    :hook
    ((prog-mode org-mode fundamental-mode) . display-line-numbers-mode)  
    (doom-load-theme . #'doom-themes-org-config)
    (doom-load-theme . #'doom-themes-treemacs-config)
    :config
    (load-theme 'doom-vibrant t))

(use-package all-the-icons)

;; Loads solaire mode to dynamically darken/lighten focused windows
(use-package solaire-mode
    :defer t
    :preface
    (defun solaire-mode-swap-bg-maybe ()
    (when-let (rule (assq doom-theme +doom-solaire-themes))
        (require 'solaire-mode)
        (when (cdr rule)
        (solaire-mode-swap-bg)
        (with-eval-after-load 'ansi-color
            (when-let (color (face-background 'default))
            (setf (aref ansi-color-names-vector 0) color))))))
    :hook
    ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
    (minibuffer-setup . solaire-mode-in-minibuffer)
    (doom-load-theme . #'solaire-mode-swap-bg-maybe)
    (focus-in . #'solaire-mode-reset)
    :config
    (solaire-global-mode +1)
    (solaire-mode-swap-bg))

(provide 'badliveware-theme)
;;; badliveware-theme.el ends here