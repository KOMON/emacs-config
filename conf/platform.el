(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'platform)
