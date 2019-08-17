;;; badliveware-constants.el -*- lexical-binding: t; -*-

(defconst EMACS26+ (> emacs-major-version 25))
(defconst EMACS27+ (> emacs-major-version 26))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(defvar my/emacs-dir    (eval-when-compile (file-truename user-emacs-directory))    
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")
(defvar my/local-dir    (concat my/emacs-dir ".local/")     "Root directory for local storage.")

(defvar my/etc-dir      (concat my/local-dir "etc/")        "Directory for non-volatile local storage.")
(defvar my/cache-dir    (concat my/local-dir "cache/")      "Directory for volatile local storage.")
(defvar my/packages-dir (concat my/local-dir "packages/")   "Where package.el and quelpa plugins (and their caches) are stored.")
(defvar my/config-dir   (concat my/emacs-dir "config/")     "Where the user configuration files are located.")

;; Make directiories if they dont exist
(make-directory my/config-dir :parents)
(make-directory my/packages-dir :parents)
(make-directory my/cache-dir :parents)
(make-directory my/etc-dir :parents)

(provide 'badliveware-constants)
;;; badliveware-constants.el ends here
