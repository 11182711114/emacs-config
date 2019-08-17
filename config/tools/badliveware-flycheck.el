;;; badliveware-flycheck.el -*- lexical-binding: t; -*-

(require 'use-package)

(defvar +flycheck-lazy-idle-delay 3.0
  "The delay before flycheck checks the buffer, after a check that produces no
errors.")

(use-package flycheck
    :commands (flycheck-list-errors flycheck-buffer)
    :config
    ;; new-line checks are a mote excessive; idle checks are more than enough
    (setq flycheck-check-syntax-automatically
            (delq 'new-line flycheck-check-syntax-automatically))

    (defun +flycheck|buffer ()
        "Flycheck buffer on ESC in normal mode."
        (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil))
    (add-hook 'doom-escape-hook #'+flycheck|buffer 'append)

    (defun +flycheck|adjust-syntax-check-eagerness ()
        "Check for errors less often when there aren't any.
    Done to reduce the load flycheck imposes on the current buffer."
        (if flycheck-current-errors
            (kill-local-variable 'flycheck-idle-change-delay)
        (setq-local flycheck-idle-change-delay +flycheck-lazy-idle-delay)))
    (add-hook 'flycheck-after-syntax-check-hook #'+flycheck|adjust-syntax-check-eagerness)

    (global-flycheck-mode +1))

(defun +flycheck|init-popups ()
  "Activate `flycheck-posframe-mode' if available and in GUI Emacs.
Activate `flycheck-popup-tip-mode' otherwise.
Do nothing if `lsp-ui-mode' is active and `lsp-ui-sideline-enable' is non-nil."
  (unless (and (bound-and-true-p lsp-ui-mode)
               lsp-ui-sideline-enable)
    (if (and (fboundp 'flycheck-posframe-mode)
             (display-graphic-p))
        (flycheck-posframe-mode +1)
      (flycheck-popup-tip-mode +1))))

(use-package flycheck-popup-tip
    :commands (flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup)
    :init (add-hook 'flycheck-mode-hook #'+flycheck|init-popups)
    :config
    (setq flycheck-popup-tip-error-prefix "✕ ")
    (with-eval-after-load 'evil
        ;; Don't display errors while in insert mode, as it can affect the cursor's
        ;; position or cause disruptive input delays.
        (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)))

(provide 'badliveware-flycheck)
;;; badliveware-flycheck.el ends here
