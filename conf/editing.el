(setq  initial-major-mode 'org-mode
       ido-create-new-buffer 'always
       make-backup-files nil
       auto-save-default nil
       package-enable-at-startup t
       shift-select-mode nil
       tab-width 2
       create-lockfiles nil)

(delete-selection-mode t)
(transient-mark-mode t)
(electric-pair-mode t)
(global-subword-mode 1)

(provide 'editing)

