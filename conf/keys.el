(when window-system (global-unset-key "\C-z"))

(defmacro keybind (bind command)
  `(global-set-key (kbd ,bind) ',command))

(defun keybind-map (map bind command)
  `(define-key ,map (kbd ,bind) ',command))

(defmacro define-prefix-map (prefix name binds)
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


;; (setq 'bookmark-map
;;       (keymap (108 . bookmark-bmenu-list)
;;               (77 . bookmark-set-no-overwrite)
;;               (109 . bookmark-set)
;;               (98 . bookmark-jump)))

;; (setq 'rectangle-map
;;       (keymap
;;        (27 keymap (119 . copy-rectangle-as-kill))
;;        (78 . rectangle-number-lines)
;;        (116 . string-rectangle)
;;        (111 . open-rectangle)
;;        (121 . yank-rectangle)
;;        (100 . delete-rectangle)
;;        (107 . kill-rectangle)
;;        (99 . clear-rectangle)))





(keybind "C-x C-j" join-line)
(keybind "C-j" newline-and-indent)
(keybind "M-SPC" cycle-spacing)
(global-unset-key (kbd "C-x C-z"))

(provide 'keys)
