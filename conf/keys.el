;;;; keys --- Summary
;;; Commentary:
;;; Code:
(when window-system (global-unset-key "\C-z"))

(defmacro keybind (bind command)
  `(global-set-key (kbd ,bind) ',command))

(defun keybind-map (map bind command)
  `(define-key ,map (kbd ,bind) ',command))

(defmacro define-prefix-map (prefix name binds)
  "Define a prefix map"
  `(progn
     (define-prefix-command ',name)
     (global-set-key (kbd ,prefix) ',name)
     ,@(mapcar (lambda (bind)
                 (keybind-map name (car bind) (cdr bind)))
               binds)))

(define-prefix-map "M-r" register-map
  (("C-SPC"      . point-to-register)
   ("C-u C-SPC"  . jump-to-register)
   ("f"          . frameset-to-register)
   ("w"          . window-configuration-to-register)
   ("+"          . increment-register)
   ("n"          . number-to-register)
   ("r"          . copy-rectangle-to-register)
   ("C-y"        . insert-register)
   ("M-w"        . copy-to-register)))

(keybind "C-x C-j" join-line)
(keybind "C-j" newline-and-indent)
(keybind "M-SPC" cycle-spacing)
(keybind "M-." xref-find-definitions)
(keybind "M-?" xref-find-references)
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "M-`"))

(provide 'keys)
;;; keys.el ends here
