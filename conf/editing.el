(setq  initial-major-mode 'org-mode
       make-backup-files nil
       auto-save-default nil
       package-enable-at-startup t
       shift-select-mode nil
       tab-width 2
       create-lockfiles nil
       kill-ring-max 500)

(delete-selection-mode t)
(transient-mark-mode t)
(electric-pair-mode t)
(global-subword-mode 1)

(provide 'editing)

(setq make-backup-files nil
      auto-save-default nil
       create-lockfiles nil)
