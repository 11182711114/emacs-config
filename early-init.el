;; Emacs 27 runs (package-initialize) before loading init.el
;; we can prevent this by setting this
(setq package-enable-at-startup nil)