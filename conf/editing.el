(setq
; Set org to be the default mode in scratch buffers
 initial-major-mode 'org-mode
 ; Don't make backup files
 make-backup-files nil
 ; turn off auto-save, I make too many dumb mistakes
 auto-save-default nil
 ; enable package at startup
 package-enable-at-startup t
 ; Turn off shift-selecting
 shift-select-mode nil
 ; set default tab width
 tab-width 2
 ; forget about making lockfiles
 create-lockfiles nil
 ; Set a higher kill-ring maximum
 kill-ring-max 500)
; Delete selected text when I start typing
(delete-selection-mode t)
; The default I think, setting the mark activates the region
(transient-mark-mode t)
; Auto-insert ending chars for paired chars like brackets and quotes
(electric-pair-mode t)
; make moving by word smarter when camel case is involved
(global-subword-mode 1)
; show matching parens
(show-paren-mode t)

(provide 'editing)
