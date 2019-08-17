;;; badliveware-whitespace.el -*- lexical-binding: t; -*-

(use-package whitespace
    :straight (whitespace :type built-in)
    :hook
    ((prog-mode . whitespace-turn-on)
    (text-mode . whitespace-turn-on))
    :custom
    (whitespace-style '(face empty indentation::space tab trailing)))

(use-package ws-butler
    :config
    (setq ws-butler-global-exempt-modes
            (append ws-butler-global-exempt-modes
                    '(special-mode comint-mode term-mode eshell-mode)))
    (ws-butler-global-mode))

(defvar doom-detect-indentation-excluded-modes '(fundamental-mode)
  "A list of major modes in which indentation should be automatically
detected.")

(use-package dtrt-indent
    ;; Automatic detection of indent settings
    :unless noninteractive
    :defer t
    :preface
    (defun doom|detect-indentation ()
        (unless (or (not after-init-time)
                    (member (substring (buffer-name) 0 1) '(" " "*"))
                    (memq major-mode doom-detect-indentation-excluded-modes))
            ;; Don't display messages in the echo area, but still log them
            (let ((inhibit-message t))
                (dtrt-indent-mode +1))))
    :hook
    ((change-major-mode-after-body read-only-mode) . #'doom|detect-indentation)
    :config
    (setq dtrt-indent-run-after-smie t)

    ;; always keep tab-width up-to-date
    (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)

    (defvar dtrt-indent-run-after-smie)
    (defun doom*fix-broken-smie-modes (orig-fn arg)
    "Some smie modes throw errors when trying to guess their indentation, like
    `nim-mode'. This prevents them from leaving Emacs in a broken state."
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
        (cl-letf* ((old-smie-config-guess (symbol-function 'smie-config-guess))
                    ((symbol-function 'smie-config-guess)
                    (lambda ()
                    (condition-case e (funcall old-smie-config-guess)
                        (error (setq dtrt-indent-run-after-smie t)
                                (message "[WARNING] Indent detection: %s"
                                        (error-message-string e))
                                (message "")))))) ; warn silently
        (funcall orig-fn arg))))
    (advice-add #'dtrt-indent-mode :around #'doom*fix-broken-smie-modes))

(provide 'badliveware-whitespace)
;;; badliveware-whitespace.el ends here
