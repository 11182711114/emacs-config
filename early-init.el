;; Emacs 27 runs (package-initialize) before loading init.el

;; Prevent the glimpse of un-styled Emacs by setting these early.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))