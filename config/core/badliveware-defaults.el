;;; badliveware-defaults.el -*- lexical-binding: t; -*-

(require 'badliveware-constants)

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
 custom-file                  (concat my/etc-dir "custom/.custom.el")
 transient-history-file       (concat my/etc-dir "transient/history.el")
 transient-levels-file        (concat my/cache-dir "transient/levels.el")
 transient-values-file        (concat my/cache-dir "transient/values.el")
 treemacs-persist-file        (concat my/etc-dir "treemacs-persist.org")
 undo-tree-history-directory-alist 
 (list (cons "." (concat my/etc-dir "undo-tree-hist/")))
 user-emacs-ensime-directory  (concat my/etc-dir "ensime/")
 vimish-fold-dir              (concat my/etc-dir "vimish-fold/"))


(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))


(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                      ; Delete files to trash
 display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 recenter-positions '(5 top bottom)               ; Set re-centering positions
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace nil                     ; Display trailing whitespaces
 split-height-threshold nil                       ; Disable vertical window splitting
 split-width-threshold nil                        ; Disable horizontal window splitting
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 idle-update-delay 1                              ; update ui slightly less often
 ;; no beeping or blinking please
 ring-bell-function #'ignore
 visible-bell nil
 ;; Scrolling
 hscroll-margin 2
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0
 scroll-preserve-screen-position t
 mouse-wheel-scroll-amount '(5 ((shift) . 2))
 mouse-wheel-progressive-speed nil ; don't accelerate scrolling
 ;; be quiet at startup; don't load or display anything unnecessary
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil  ; don't create backup~ files
 ) 

(cd "~/")                                         ; Move to the user directory
(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(menu-bar-mode 0)                                 ; Disable the menu bar
(mouse-avoidance-mode 'banish)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(blink-cursor-mode -1)                            ; Disable blinking cursor

(when (fboundp 'set-charset-priority)             ; UTF-8 as the default coding system
  (set-charset-priority 'unicode))                ; pretty
(prefer-coding-system 'utf-8)                     ; pretty
(setq locale-coding-system 'utf-8)                ; please
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))          ; with sugar on top

(provide 'badliveware-defaults)
