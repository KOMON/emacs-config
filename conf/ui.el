(setq 
 ; Turn off the splash screen
 inhibit-splash-screen t
 ; Turn off default text in scratch buffers
 initial-scratch-message nil
 ; allow copy-paste to use the system clipboard
 select-enable-clipboard t
 ; Set the amount of delay before prefix keys show up in the minibuffer
 echo-keystrokes 0.1
 ; Turn off dialog boxes for y/n prompts
 use-dialog-box nil
 ; THE BELLS! THE BEEEEELLLLS!
 ring-bell-function 'ignore  
 ; Don't allow automatic vertical window splits 
 split-height-threshold nil)

(setq-default 
 ; show empty line markers in the fringe
 indicate-empty-lines t
 ; Disallow automatic addition of literal tabs
 indent-tabs-mode nil
 ; /shrug, something something advice, probably makes customization easier
 ad-redefinition-action 'accept
 ; use 'y-or-n-p when asking to kill emacs
 confirm-kill-emacs 'y-or-n-p
 ; Don't display load average in modeline
 display-time-default-load-average nil
 ; set default fill to 80 columns (I like)
 fill-column 80
 ; always select help window for easy quitting
 help-window-select t
 ; make bottom-of-window scrolling line-by-line
 scroll-conservatively most-positive-fixnum
 ; Make filling see a single space after period as the end of a sentence
 sentence-end-double-space nil
 ; Make the cursor as wide as the character under it (important for tabs or wide unicode characters)
 x-stretch-cursor t)
; Set fringe to 10 px wide
(fringe-mode 8)

; try really hard to make sure indicate-empty-lines is on
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(set-face-attribute 'vertical-border nil :foreground (face-attribute 'fringe :background))
; Turn off the scrollbar
(scroll-bar-mode -1)
; Turn of the toolbar
(tool-bar-mode -1)
; Turn off the menu bar
(menu-bar-mode -1)
; turn off tooltips (display them in the echo area
(tooltip-mode -1)

; Display line numbers in the fringe
(global-display-line-numbers-mode 1)
; Turn on linenumbers in the modeline
(line-number-mode t)
; Turn on column numbers in the modeline
(column-number-mode t)
; use 'y-or-n-p everywhere
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'ui)
;;; ui.el ends here
