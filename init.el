;;; init.el --- My Emacs configuration, shamelessly stolen from various sources
;;; Commentary:
;; My Emacs configuration, shamelessly stolen from various sources

;; Following lines load an Org file and build the configuration code out of it.

;;; Code:

(let ((gc-cons-threshold most-positive-fixnum))

  (defvar my/emacs-dir
      (eval-when-compile (file-truename user-emacs-directory))
      "The path to the currently loaded .emacs.d directory. Must end with a slash.")

  (defvar my/local-dir (concat my/emacs-dir ".local/")
    "Root directory for local storage.

  Use this as a storage location for this system's installation of Doom Emacs.
  These files should not be shared across systems. By default, it is used by
  `doom-etc-dir' and `doom-cache-dir'. Must end with a slash.")

  (defvar my/etc-dir (concat my/local-dir "etc/")
    "Directory for non-volatile local storage.

  Use this for files that don't change much, like server binaries, external
  dependencies or long-term shared data. Must end with a slash.")

  (defvar my/cache-dir (concat my/local-dir "cache/")
    "Directory for volatile local storage.

  Use this for files that change often, like cache files. Must end with a slash.")

  (defvar my/packages-dir (concat my/local-dir "packages/")
    "Where package.el and quelpa plugins (and their caches) are stored.

  Must end with a slash.")

  (setq-default
    ;; Dont litter .emacs.d/  
    package-user-dir             my/packages-dir
    abbrev-file-name             (concat my/local-dir "abbrev.el")
    async-byte-compile-log-file  (concat my/etc-dir "async-bytecomp.log")
    auto-save-list-file-name     (concat my/cache-dir "autosave")
    backup-directory-alist       (list (cons "." (concat my/cache-dir "backup/")))
    desktop-dirname              (concat my/etc-dir "desktop/")
    desktop-base-file-name       "autosave"
    desktop-base-lock-name       "autosave-lock"
    desktop-path                 (list desktop-dirname)
    desktop-save                 t
    desktop-auto-save-timeout    30
    pcache-directory             (concat my/cache-dir "pcache/")
    request-storage-directory    (concat my/cache-dir "request")
    server-auth-dir              (concat my/cache-dir "server/")
    shared-game-score-directory  (concat my/etc-dir "shared-game-score/")
    tramp-auto-save-directory    (concat my/cache-dir "tramp-auto-save/")
    tramp-backup-directory-alist backup-directory-alist
    tramp-persistency-file-name  (concat my/cache-dir "tramp-persistency.el")
    url-cache-directory          (concat my/cache-dir "url/")
    url-configuration-directory  (concat my/etc-dir "url/")
    gamegrid-user-score-file-directory (concat my/etc-dir "games/"))

  ;; Set repositories
  ;; (require 'package)
  ;; (setq-default
  ;;  load-prefer-newer t
  ;;  package-enable-at-startup nil)
  ;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  ;; (package-initialize)

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

(setq straight-check-for-modifications '(watch-files find-when-checking))
(setq-default 
  use-package-always-defer t
  use-package-always-ensure nil
  straight-use-package-by-default t)

(straight-use-package 'use-package)
  ;; Install dependencies
  ;; (unless (package-installed-p 'use-package)
  ;;   (package-refresh-contents)
  ;;   (package-install 'use-package t))
  ;; (setq-default
  ;;  use-package-always-defer t
  ;;  use-package-always-ensure t)

  ;; Use latest Org
  (use-package org)

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "emacsconfig.org" user-emacs-directory))
  (garbage-collect))

;;; init.el ends here