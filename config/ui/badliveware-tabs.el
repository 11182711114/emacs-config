;;; badliveware-tabs.el -*- lexical-binding: t; -*-
(require 'use-package)
(require 'general)
(require 'evil)

(use-package centaur-tabs
    :config
    (setq centaur-tab-style "bar"
            centaur-tabs-height 32
            centaur-tabs-set-icons t
            ;; centaur-tabs-gray-out-icons 'buffer
            centaur-tabs-set-bar 'over
            centaur-tabs-set-modified-marker t)
    (centaur-tabs-group-by-projectile-project)
    (centaur-tabs-headline-match)
    :hook
    ((prog-mode dashboard-mode term-mode calendar-mode org-agenda-mode helpful-mode) 
        . centaur-tabs-local-mode)
    :config
    (centaur-tabs-mode t)
    :bind
    ;; (:map evil-normal-state-map
    ;;         ("g t" . centaur-tabs-forward)
    ;;         ("g T" . centaur-tabs-backward))
            )


(provide 'badliveware-tabs)
;;; badliveware-tabs.el ends here
