;; -*- lexical-binding: t -*-
;;; Emacs -- My emacs configuration
;;; Commentary:
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/conf"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa"))

(package-initialize)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'use-package)

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa"))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package magit
  :ensure t
  :hook (after-save . magit-after-save-refresh-status)
  :config
  (setq magit-delete-by-moving-to-trash nil)
  (setq auth-sources '("~/.authinfo")))

(use-package forge :ensure t)




(use-package eglot
  :config
  (defun deno-reference-file-name-handler (operation &rest args)
    "Takes filenames like 'deno:/asset/lib.deno.web.d.ts' and
resolves them using the Deno LSP den/virtualTextDocument endpoint.

Currently only handles 'get-file-buffer and 'file-exists-p since those are specifically
what's called in the xref-goto-xref resolution

* Uses eglot--servers-by-xrefed-file to find a deno lsp server to ask
* Issues a deno/virtualTextDocument LSP command to get the file text
* Dumps it into a buffer
* Returns the buffer"
    (let* ((filename (car args))
           (server (or (eglot-current-server) (gethash filename eglot--servers-by-xrefed-file))))
      (cond
       ((eq operation 'file-exists-p) (not (eq server nil)))
       ((eq operation 'get-file-buffer)
        (let ((buffer (get-buffer-create filename)))
          (with-current-buffer buffer
            (unless buffer-read-only
              (when-let ((deno-part (substring filename (string-match "\\(deno:/.+\.ts\\)" filename)))
                         (response (jsonrpc-request server :deno/virtualTextDocument `(:textDocument (:uri ,deno-part)))))

                (insert response)
                (setq buffer-file-name filename)
                (setq buffer-read-only t)
                (set-buffer-modified-p nil)
                (goto-char (point-min))
                (typescript-ts-mode)
                (current-buffer))))
          buffer))
       (t (let ((inhibit-file-name-handlers
                 (cons 'deno-reference-file-name-handler
                       (and (eq inhibit-file-name-operation operation)
                            inhibit-file-name-handlers)))
                (inhibit-file-name-operation operation))
            (apply operation args))))))
  (add-to-list 'file-name-handler-alist '("deno:/.+\.ts" . deno-reference-file-name-handler)))

(use-package go-ts-mode
  :mode "\\.go$"
  :hook ((go-mode . eglot)
         (before-save . gofmt-before-save)))

(use-package add-node-modules-path
  :ensure t)

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("C-l" . vertico-directory-delete-word))
  :init
  (vertico-mode))

(use-package corfu
  :ensure t
  :config
  (global-corfu-mode 1))

(use-package terraform-mode
  :ensure t)

(use-package typescript-ts-mode
  :mode (("\\.m?ts$" . typescript-ts-mode)
         ("\\.m?tsx$" . tsx-ts-mode))
  :hook ((typescript-ts-mode . add-node-modules-path)
         (typescript-ts-mode . eglot-ensure)
         (typescript-ts-mode . flymake-eslint-enable)
         (typescript-ts-mode . prettier-mode)
         (tsx-ts-mode . add-node-modules-path)
         (tsx-ts-mode . eglot-ensure)
         (tsx-ts-mode . flymake-eslint-enable)
         (tsx-ts-mode . prettier-mode)))

(use-package rust-ts-mode
  :mode "\\.rs$"
  :hook (rust-mode . eglot))

(use-package asdf
  :ensure t
  :straight (asdf :type git :host github :repo "tabfugnic/asdf.el")
  :config
  (asdf-enable))

(use-package uuidgen :ensure t)

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "w"))

(use-package modus-themes
  :ensure t
  :init
  (load-theme 'modus-vivendi-tritanopia 't))

(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode)
  :init
  (ws-butler-global-mode 1))

(defun djr/use-local-eslint ()
  "Set project's `node_modules' binary eslint as first priority.
    If nothing is found, keep the default value flymake-eslint set or
    your override of `flymake-eslint-executable-name.'"
  (interactive)
  (let* ((root (locate-dominating-file (buffer-file-name) "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (while (eq eslint nil))
    (when (and eslint (file-executable-p eslint))
      (setq-local flymake-eslint-executable-name eslint)
      (message (format "Found local ESLINT! Setting: %s" eslint))
      (flymake-eslint-enable))))


(use-package flymake
  :demand t)

(use-package flymake-eslint
  :ensure t
  :demand t                             
  :config
  (setq flymake-eslint-prefer-json-diagnostics t))

(use-package consult
  :after (flymake)
  :ensure t
  :demand t
  :bind (("C-x b" . consult-buffer)
	 ("C-x 4 b" . consult-buffer-other-window)
	 ("M-y" . consult-yank-pop)
	 ("M-g M-g" . consult-goto-line)
	 ("M-i" . consult-line)
	 ("C-c C-j" . consult-git-grep)
	 ("C-c j" . consult-git-grep)
	 ("C-c C-/" . consult-find)
	 :map compilation-mode-map
	 ("C-c C-c" . consult-compile-error)
	 :map flymake-mode-map
	 ("C-c ! j" . consult-flymake)))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package prettier
  :ensure t
  :init
  (global-prettier-mode))

(use-package embark
  :ensure t
  :demand t
  :bind (("C-z" . embark-act)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package syntax-subword
  :ensure t
  :config
  (setq syntax-subword-skip-spaces t))

(use-package avy
  :ensure t
  :bind (("M-a" . avy-goto-char-timer))
  :config
  (defun avy-action-embark (pt)
    (save-excursion
      (goto-char pt)
      (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t))

(use-package emacs
  :init
  (setq
   ;; don't make backup files
   make-backup-files nil
   ;; don't create lockfiles
   create-lockfiles nil
   ;; up the kill ring
   kill-ring-max 500)
  ;; delete selected text when I start typing
  (delete-selection-mode t)
  (global-subword-mode 1)
  (electric-pair-mode t)
  (global-set-key (kbd "C-c ! n") 'flymake-goto-next-error)
  (global-set-key (kbd "C-c ! p") 'flymake-goto-prev-error)
  (global-set-key (kbd "C-c ! b") 'flymake-show-buffer-diagnostics)
  (global-set-key (kbd "C-c ! P") 'flymake-show-project-diagnostics)
  (global-set-key (kbd "C-x C-j") 'join-line)
  (global-set-key (kbd "C-j") 'newline-and-indent)
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "M-`"))

  (if (eq system-type 'darwin)
      (setq mac-command-modifier 'meta))

  (setq
   ;; Turn off the splash screen
   inhibit-splash-screen t
   ;; Turn off default text in scratch buffers
   initial-scratch-message nil
   ;; allow copy-paste to use the system clipboard
   select-enable-clipboard t
   ;; Set the amount of delay before prefix keys show up in the minibuffer
   echo-keystrokes 0.1
   ;; Turn off dialog boxes for y/n prompts
   use-dialog-box nil
   ;; THE BELLS! THE BEEEEELLLLS!
   ring-bell-function 'ignore
   ;; Don't allow automatic vertical window splits
   split-height-threshold nil)

  (setq-default
   ;; show empty line markers in the fringe
   indicate-empty-lines t
   ;; Disallow automatic addition of literal tabs
   indent-tabs-mode nil
   ;; /shrug, something something advice, probably makes customization easier
   ad-redefinition-action 'accept
   ;; use 'y-or-n-p when asking to kill emacs
   confirm-kill-emacs 'y-or-n-p
   ;; Don't display load average in modeline
   display-time-default-load-average nil
   ;; set default fill to 80 columns (I like)
   fill-column 80
   ;; always select help window for easy quitting
   help-window-select t
   ;; make bottom-of-window scrolling line-by-line
   scroll-conservatively most-positive-fixnum
   ;; Make filling see a single space after period as the end of a sentence
   sentence-end-double-space nil
   ;; Make the cursor as wide as the character under it (important for tabs or wide unicode characters)
   x-stretch-cursor t
   ;; turn off auto-saving
   auto-save-default nil)
  (scroll-bar-mode -1)
  ;; Turn of the toolbar
  (tool-bar-mode -1)
  ;; Turn off the menu bar
  (menu-bar-mode -1)
  ;; turn off tooltips (display them in the echo area
  (tooltip-mode -1)

  ;; Display line numbers in the fringe
  (global-display-line-numbers-mode 1)
  ;; Turn on linenumbers in the modeline
  (line-number-mode t)
  ;; Turn on column numbers in the modeline
  (column-number-mode t)
  ;; use 'y-or-n-p everywhere
  (fset 'yes-or-no-p 'y-or-n-p)
  )

(defun djr/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'djr/kill-this-buffer)

(defadvice pop-to-mark-command (around ensure-new-position activate)
  "When popping the mark, continue popping until the cursor actually does move.
 Also, if the last command was a copy - skip past all the expand-region cruft."
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  "Offer to create parent directories if they do not exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it? " parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

(require 'ansi-color)
(defun djr/colorize-compilation-buffer ()
  "Colorize compilation buffer with ansi colors."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(add-hook 'compilation-filter-hook 'djr/colorize-compilation-buffer)

(put 'set-goal-column 'disabled nil)

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
