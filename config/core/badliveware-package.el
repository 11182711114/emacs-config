;;; badliveware-package.el -*- lexical-binding: t; -*-

(require 'badliveware-constants)

;; Broken? Init goes from 0.6s to 14s
;; (when (and (executable-find "watchexec")
;;            (executable-find "python3"))
;;       (setq straight-check-for-modifications '(watch-files find-when-checking)))


(defvar bootstrap-version)
(let ((bootstrap-file 
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
          'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq-default use-package-always-defer t
              use-package-always-ensure nil
              straight-use-package-by-default t)
(straight-use-package 'use-package)

(provide 'badliveware-package)