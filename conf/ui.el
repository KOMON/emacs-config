(setq inhibit-splash-screen t
      initial-scratch-message nil
      x-select-enable-clipboard t
      echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell nil
      tooltip-use-echo-area t
      redisplay-dont-pause t
      split-height-threshold nil)

(setq-default indicate-empty-lines t
              indent-tabs-mode nil
              ad-redefinition-action 'accept
              confirm-kill-emacs 'y-or-n-p
              display-time-default-load-average nil
              fill-column 80
              help-window-select t
              indent-tabs-mode nil
              scroll-conservatively most-positive-fixnum
              sentence-end-double-space nil
              x-stretch-cursor t)
(fringe-mode 10)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(set-face-attribute 'vertical-border nil :foreground (face-attribute 'fringe :background))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(if (version< emacs-version "26")
    (global-linum-mode 1)
    (global-display-line-numbers-mode 1))
(line-number-mode t)
(column-number-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'ui)
