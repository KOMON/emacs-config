
;;; Emacs -- My emacs configuration -*- lexical-binding: t -*-
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

(straight-use-package '(use-package :type built-in))
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
  :straight (eglot :type built-in)
  :config
  (defun lsp-reference-handler (handler-name regex endpoint parse)
    "Return a file handler function that takes a file reference that the
currently-active, eglot-connected LSP server can turn into a source
document with a JSON-RPC request to ENDPOINT with the response parsed by
PARSE.

File reference strings must match REGEX.

HANDLER-NAME is the name used to suppress the handler in recursive
handling calls... see info node `(elisp)Magic File Names' for more
details"
    (lambda (operation &rest args)
      (:documentation
       (format
        "A file-name-handler function handling magic file names matching the
regex \"%s\".

Currently handles:
* `expand-file-name'
* `file-exists-p'
* `get-file-buffer'

Where `get-file-buffer' is the most interesting function.

Finds the currently-active, eglot-connected LSP server that can handle
the %s endpoint and submits the file name, parsing the source text from
the response and returning a buffer containing that source.

To suppress this handler for any of the above primitives, add `%s' to
`inhibit-file-name-handlers' before the call, like so:

  (let ((inhibit-file-name-handlers
         (cons inhibit-file-name-handlers '%s)))
   (file-exists-p filename))

Generated via `lsp-reference-handler'.

See info node `(elisp)Magic File Names' for more examples on how to use this
function."
        regex
        endpoint
        handler-name
        handler-name))
      (let* ((filename (car args))
             ;; get only the portion of the filename (less expansions, etc) that
             ;; the LSP will recognize and care about
             (lsp-part (substring filename (string-match regex filename)))
             (server (or
                      (eglot-current-server)
                      ;; tbh I don't know if this is ever... right, but it seems
                      ;; to work in isolation
                      (gethash filename eglot--servers-by-xrefed-file))))
        (cond
         ;; this is less of an exapansion and more of a contraction... no idea if
         ;; this will cause problems. Shouldn't, it seems
         ((eq operation 'expand-file-name) lsp-part)
         ;; We'll just assume it works if we found a server
         ((eq operation 'file-exists-p) (not (eq server nil)))
         ;; The real meat
         ((eq operation 'get-file-buffer)
          (let ((buffer (get-buffer-create lsp-part)))
            (with-current-buffer buffer
              ;; we assume that the buffer is filled and up-to-date if it's marked read-only
              (unless buffer-read-only
                ;; make the RPC request
                (when-let ((response (jsonrpc-request
                                      server
                                      endpoint
                                      (list :textDocument (list :uri lsp-part)))))

                  ;; insert the parsed response
                  (insert (funcall parse response))
                  ;; set the file name
                  (setq buffer-file-name lsp-part)
                  ;; set read-only
                  (setq buffer-read-only t)
                  ;; clear the dirty bit
                  (set-buffer-modified-p nil)
                  ;; go to the beginning of the file
                  ;; in xref scenarios the xref handling will go the right line later
                  (goto-char (point-min))
                  ;; activate whatever modes should go in this type of file
                  (set-auto-mode))))
            ;; return the buffer
            buffer))
         ;; Copy-pasted from `(elisp)Magic File Names', call into the primitive
         ;; recursively with the our handler name inhibited to pass handling down
         ;; the line
         (t (let ((inhibit-file-name-handlers
                   (cons handler-name
                         (and (eq inhibit-file-name-operation operation)
                              inhibit-file-name-handlers)))
                  (inhibit-file-name-operation operation))
              (apply operation args)))))))
  
  (defmacro define-lsp-reference-file-name-handler (name &rest options)
    "Define and register a file name handler function that is powered by the
current eglot-connected LSP server by hitting a specified JSON-RPC
endpoint

Usage:

  (define-lsp-reference-file-name-handler handler-name
    [:keyword [option]]...)

:regex     The regex pattern that will match candidate file names
:endpoint  The name of the JSON-RPC endpoint that will power the source retrieval
:parse     A function to parse the final JSON-RPC response (defaults to identity)"
    `(let* ((handler-name ',name)
            (regex ,(plist-get options :regex))
            (endpoint ,(plist-get options :endpoint))
            (parse (or ,(plist-get options :parse) 'identity))
            (handler (lsp-reference-handler
                      handler-name
                      regex
                      endpoint
                      parse)))
       (defalias handler-name handler)
       (add-to-list 'file-name-handler-alist (cons regex ',name))))
  (define-lsp-reference-file-name-handler
   deno-reference-file-name-handler
   :regex "deno:/.+\.ts"
   :endpoint :deno/virtualTextDocument)
  (define-lsp-reference-file-name-handler
   csharp-reference-file-name-handler
   :regex "csharp:/.+\.cs"
   :endpoint :csharp/metadata
   :parse (lambda (response) (plist-get response :source)))

  (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/src/elixir-ls/language_server.sh"))
  (add-to-list 'eglot-server-programs '(csharp-mode  . ("~/bin/omnisharp/OmniSharp" "-lsp"))))

(use-package csharp-mode
  :straight nil
  :mode ("\\.cs")
  :hook (csharp-mode . eglot-ensure))

(use-package web-mode
  :ensure t
  :mode ("\\.html?" "\\.cshtml?" "\\.razor"))

(use-package project
  :demand t
  :straight (project :type built-in)
  :config
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module))

(use-package xref
  :demand t
  :straight (xref :type built-in))

(use-package go-ts-mode
  :mode "\\.go$"
  :hook ((go-ts-mode . eglot-ensure)
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
  :mode (("\\.m?[jt]s$" . typescript-ts-mode)
         ("\\.m?[jt]sx$" . tsx-ts-mode))
  :hook ((typescript-ts-mode . add-node-modules-path)
         (typescript-ts-mode . eglot-ensure)
         (typescript-ts-mode . djr/use-local-eslint)
         (typescript-ts-mode . prettier-mode)
         (tsx-ts-mode . add-node-modules-path)
         (tsx-ts-mode . eglot-ensure)
         (tsx-ts-mode . djr/use-local-eslint)
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

(use-package elixir-ts-mode
  :mode ("\\.exs?$")
  :hook (elixir-ts-mode . eglot-ensure))

(defun djr/deno-or-node-root ()
    (let ((deno-root (locate-dominating-file (buffer-file-name) "deno.json"))
          (node-root (locate-dominating-file (buffer-file-name) "node_modules")))
      (if deno-root
          `(deno . ,deno-root)
        `(node . ,node-root))))

(defun djr/use-local-eslint ()
  "Set project's `node_modules' binary eslint as first priority.
If nothing is found, keep the default value flymake-eslint set or
your override of `flymake-eslint-executable-name.'"
  (interactive)
  (pcase (djr/deno-or-node-root)
      (`(deno . ,root) (progn
                        (setq-local flymake-eslint-executable-name "deno")
                        (setq-local flymake-eslint-executable-args "run eslint --allow-env --allow-read --allow-write")
                        (flymake-eslint-enable)))
      (`(node . ,root) (let ((eslint (expand-file-name "node_modules/.bin/eslint" root)))
                         (when (and eslint (file-executable-p eslint))
                           (setq-local flymake-eslint-executable-name eslint)
                           (flymake-eslint-enable))))))

(use-package flymake
  :demand t
  :straight (flymake :type built-in))

(use-package flymake-mypy
  :straight (flymake-mypy
             :type git
             :host github
             :repo "com4/flymake-mypy")
  :hook ((python-mode . (lambda () (flymake-mypy-enable)))))

(use-package flymake-eslint
  :ensure t
  :demand t
  :config
  (setq flymake-eslint-prefer-json-diagnostics t))

(use-package flymake-racket
  :after (flymake)
  :ensure t
  :demand t)

(use-package racket-mode
  :ensure t
  :demand t
  :mode ("\\.rkt$"))

(use-package geiser
  :ensure t
  :demand t)

(use-package geiser-racket
  :after (geiser)
  :ensure t
  :demand t)

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
  :init
  (global-syntax-subword-mode t)
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

(use-package glsl-mode
  :ensure t)

(use-package zig-mode
  :ensure t)

(use-package glsl-mode
  :ensure t)

(use-package zig-mode
  :ensure t)

(use-package fennel-mode
  :ensure t
  :mode ("\\.fnl$")
  :hook (fennel-mode . fennel-proto-repl-minor-mode))

(use-package howm
  :ensure t
  :config
   (setq howm-directory "~/src/notes"
         howm-history-file "~/src/notes/.howm-history"
         howm-keyword-file "~/src/notes/.howm-keys"
         howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.md"
         howm-view-split-horizontally t))

(use-package emacs ;; c++
  :mode ("\\.(h|hpp|cc|cpp|c++|cxx)$")
  :hook ((c++-ts-mode . eglot-ensure)
         (c-or-c++-ts-mode . eglot-ensure))
  :config
  (setq c-basic-indent 8
        c-ts-mode-indent-style #'djr-c++-ts-indent-style)
  (c-set-offset 'innamespace 0)
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

  :preface
  (defun djr-c++-ts-indent-style ()
      (let* ((default-style (copy-alist (alist-get 'k&r (c-ts-mode--indent-styles 'cpp))))
             (custom-style '(((node-is "preproc") column-0 0)
                             ((n-p-gp nil nil "namespace_definition") grand-parent 0)
                             ((and (parent-is "requirement_seq") (not (node-is "}"))) standalone-parent c-ts-mode-indent-offset)))
             (combined-style (append custom-style default-style)))
        combined-style)))

(use-package dape
  :hook
  ((kill-emacs . dape-breakpoint-save)
   (after-init . dape-breakpoint-load))
  :custom
  (dape-breakpoint-global-mode +1)
  (dape-buffer-window-arrangement 'right))

(use-package cmake-mode
  :mode ("CMakeLists.txt"))

(use-package font-utils
  :ensure t
  :straight (font-utils :type git :host github :repo "rolandwalker/font-utils"))

(use-package ucs-utils
  :ensure t
  :straight (ucs-utils :type git :host github :repo "rolandwalker/ucs-utils"))

(use-package list-utils
  :ensure t
  :straight (list-utils :type git :host github :repo "rolandwalker/list-utils"))

(use-package persistent-soft
  :ensure t
  :straight (persistent-soft :type git :host github :repo "rolandwalker/persistent-soft"))

(use-package unicode-fonts
  :ensure t
  :straight (unicode-fonts :type git :host github :repo "rolandwalker/unicode-fonts")
  :config
  (unicode-fonts-setup))

(use-package dime
  :config
  (dime-setup '(dime-repl dime-note-tree))
  (setq dime-dylan-implementations
        '((opendylan ("/home/komon/src/opendylan/bin/dswank")
          :env ("OPEN_DYLAN_USER_REGISTRIES=/tmp/dime-test/registries")))))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-editing-commands
                         slime-repl
                         slime-c-p-c
                         slime-autodoc
                         slime-asdf
                         slime-fancy-inspector
                         slime-references
                         slime-xref-browser
                         slime-highlight-edits
                         slime-trace-dialog
                         slime-sprof
                         slime-mdot-fu
                         slime-quicklisp
                         slime-package-fu)))

(use-package r3-mode
  :straight (r3-mode :type git :host github :repo "Inaimathi/r3-mode")
  :mode ("\\.r$" . r3-mode))

(use-package elpy
  :mode ("\\.py" . elpy-mode)
  :config (elpy-enable))

(use-package python-mode
  :mode ("\\.py$" . python-mode))

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
  (electric-pair-mode t)
  (global-set-key (kbd "C-c ! n") 'flymake-goto-next-error)
  (global-set-key (kbd "C-c ! p") 'flymake-goto-prev-error)
  (global-set-key (kbd "C-c ! b") 'flymake-show-buffer-diagnostics)
  (global-set-key (kbd "C-c ! P") 'flymake-show-project-diagnostics)
  (global-set-key (kbd "C-x C-j") 'join-line)
  (global-set-key (kbd "C-j") 'newline-and-indent)
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "M-`"))
  (repeat-mode +1)

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
  (flymake-mode)
  (flymake-start))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-ts-mode-indent-offset 8)
 '(eglot-connect-timeout 60)
 '(elpy-project-ignored-directories
   '(".tox" "build" "dist" ".cask" ".ipynb_checkpoints" ".venv"))
 '(elpy-test-pytest-runner-command '("pytest"))
 '(elpy-test-runner 'elpy-test-pytest-runner)
 '(go-ts-mode-indent-offset 4)
 '(howm-directory "~/src/notes")
 '(howm-history-file "~/src/notes/.howm-history")
 '(howm-keyword-file "~/src/notes/.howm-keys")
 '(howm-view-split-horizontally t)
 '(python-flymake-command '("flake8" "-"))
 '(python-shell-interpreter "python3")
 '(treesit-font-lock-level 4)
 '(warning-suppress-types '((treesit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 113 :width normal :foundry "PfEd" :family "DejaVu Sans Mono")))))
(put 'downcase-region 'disabled nil)
