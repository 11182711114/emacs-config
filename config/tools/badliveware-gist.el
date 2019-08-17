;;; badliveware-gist.el -*- lexical-binding: t; -*-

(require 'use-package)


(use-package gist
    :config
    (set-evil-initial-state! 'gist-list-mode 'normal)
    (set-popup-rule! "^\\*gist-" :ignore t)
    (defun +gist*list-render (orig-fn &rest args)
        (funcall orig-fn (car args) t)
        (unless (cadr args)
        (pop-to-buffer (current-buffer))))
    (advice-add #'gist-list-render :around #'+gist*list-render))


(provide 'badliveware-gist)
;;; badliveware-gist.el ends here
