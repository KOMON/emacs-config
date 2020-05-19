;;; php.el --- PHP editing configuration.
;;; Commentary:
;;; PHP is my bread and butter, let's make it easy

;;; Code:
(require 'use-package)

(use-package php-mode
  :ensure t
  :config
  (add-hook 'php-mode-hook (lambda ()
                             (setq c-basic-offset 4)
                             (local-unset-key (kbd "C-c C-r"))))
  (add-hook 'php-mode-hook 'eldoc-mode)
  (add-hook 'php-mode-hook 'ggtags-mode)
  (add-hook 'php-mode-hook 'ws-butler-mode))

(provide 'php)
;;; php.el ends here
