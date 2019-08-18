;;; badliveware-magit-util.el -*- lexical-binding: t; -*-

(defun +magit-display-buffer (buffer)
  "Marries `magit-display-buffer-fullcolumn-most-v1' with
`magit-display-buffer-same-window-except-diff-v1', except:

1. Magit sub-buffers that aren't spawned from a status screen are opened as
   popups.
2. The status screen isn't buried when viewing diffs or logs from the status
   screen."
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ;; If opened from an eshell window or popup, use the same window.
             ((or (derived-mode-p 'eshell-mode)
                  (eq (window-dedicated-p) 'side))
              '(display-buffer-same-window))
             ;; Open target buffers below the current one (we want previous
             ;; magit windows to be visible; especially magit-status).
             ((or (bound-and-true-p git-commit-mode)
                  (derived-mode-p 'magit-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode)
                              0.35
                            0.7)))
                `(display-buffer-below-selected
                  . ((window-height . ,(truncate (* (window-height) size)))))))
             ;; log/stash/process buffers, unless opened from a magit-status
             ;; window, should be opened in popups.
             ((memq buffer-mode '(magit-process-mode
                                  magit-log-mode
                                  magit-stash-mode))
              '(display-buffer-below-selected))
             ;; Last resort: use current window
             ('(display-buffer-same-window))))))
             
(defun +magit/quit (&optional _kill-buffer)
  "Clean up magit buffers after quitting `magit-status' and refresh version
control in buffers."
  (interactive)
  (quit-window)
  (unless (cdr
           (delq nil
                 (mapcar (lambda (win)
                           (with-selected-window win
                             (eq major-mode 'magit-status-mode)))
                         (window-list))))
    (mapc #'+magit--kill-buffer (magit-mode-get-buffers))
    (dolist (buffer (doom-buffer-list))
      (when (buffer-live-p buffer)
        (if (get-buffer-window buffer)
            (+magit--refresh-vc-in-buffer buffer)
          (with-current-buffer buffer
            (setq +magit--vc-is-stale-p t)))))))

(defun +magit--kill-buffer (buf)
  "TODO"
  (when (and (bufferp buf) (buffer-live-p buf))
    (let ((process (get-buffer-process buf)))
      (if (not (processp process))
          (kill-buffer buf)
        (with-current-buffer buf
          (if (process-live-p process)
              (run-with-timer 5 nil #'+magit--kill-buffer buf)
            (kill-process process)
            (kill-buffer buf)))))))

(provide 'badliveware-magit-util)
;;; badliveware-magit-util.el ends here
