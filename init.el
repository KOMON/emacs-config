;;; init.el --- Load 'er up Johnny.
;;; Commentary:
;;; My havent' we grown over time?

;;; Code:

(add-to-list 'load-path (expand-file-name "~/.emacs.d/conf"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa"))
(package-initialize)

(require 'use-package)

(use-package exec-path-from-shell
  :ensure t)

(require 'ui)
(require 'platform)
(require 'editing)
(require 'keys)

(use-package package
  :config
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/")
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa"))

(use-package markdown-mode :ensure t
  :bind (("C-M-%" . vr/query-replace)))
(use-package dockerfile-mode :ensure t)

(use-package visual-regexp :ensure t)
(use-package visual-regexp-steroids :ensure t)

(use-package rvm
  :ensure t
  :init
  (rvm-use-default))

(use-package moe-theme
  :ensure t
  :init
  (load-theme 'moe-dark t))

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
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("C-l" . vertico-directory-delete-word)
  :config
  (vertico-mode))

(use-package marginalia
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package prescient
  :ensure t)

(use-package consult-flycheck
  :ensure t)


(use-package robe
  :ensure t
  :init
  (global-robe-mode))

(use-package rspec-mode
  :ensure t
  :init)


(use-package tree-sitter
  :ensure t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))


(use-package tree-sitter-langs
  :ensure t)

(use-package consult
  :after (compile projectile)
  :ensure t
  :demand t
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("M-y" . consult-yank-pop)
         ("M-g M-g" . consult-goto-line)
         ("C-c C-SPC" . consult-mark)
         ("M-i" . consult-line)
         ("C-c C-j" . consult-git-grep)
         ("C-c j" . consult-git-grep)
         ("C-c C-/" . consult-find)
         :map compilation-mode-map
         ("C-c C-c" . consult-compile-error)
         :map flycheck-mode-map
         ("C-c ! j" . consult-flycheck))
  :config
  (setq consult-project-root-function #'projectile-project-root))

(use-package expand-region
  :ensure t
  :after embark
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region))
  :config
  (embark-define-keymap embark-expand-region-keymap
    ""
    ("w" er/mark-word)
    ("s" er/mark-symbol)
    ("S" er/mark-symbol-with-prefix)
    ("." er/mark-next-accessor)
    ("m" er/mark-method-call)
    ("'" er/mark-inside-quotes)
    ("\"" er/mark-outside-quotes)
    (";" er/mark-comment))
  (define-key embark-region-map "=" embark-expand-region-keymap))

(use-package embark
  :ensure t
  :bind
  (("C-z" . embark-act)
   ("C-S-z" . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (add-to-list 'display-buffer-alist
               '("\\ \\*Embark Collect \\(Live\\|Completions\\)\\?\\*"
                 nill
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "w"))


(use-package company
  :ensure t
  :config
  (global-company-mode 1));

(use-package emacs
  :init
  (setq read-extended-command-predicate nil))

(use-package ws-butler
  :ensure t)

(use-package terraform-mode
  :ensure t
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package fic-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package flycheck
  :ensure t
  :after tide
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (defun npm-eslint-config-exists-p ()
    (eql 0 (flycheck-call-checker-process
            'npm-eslint-lint nil nil nil
            "eslint"
            "--print-config" (or buffer-file-name "index.js"))))
  
  (flycheck-define-checker npm-eslint-lint
    "doc string"
    :command ("npx" "eslint" "--format=json" "--stdin" "--stdin-filename" source-original)
    :standard-input t
    :error-parser flycheck-parse-eslint
    :predicate (lambda () (not (equal (file-name-extension (buffer-file-name)) "json")))
    :enabled (lambda () (npm-eslint-config-exists-p))
    :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode
                    typescript-mode)
    :working-directory flycheck-eslint--find-working-directory
    :verify
    (lambda (_)
      (let* ((default-directory
               (flycheck-compute-working-directory 'javascript-eslint))
             (have-config (npm-eslint-config-exists-p)))
        (list
         (flycheck-verification-result-new
          :label "config file"
          :message (if have-config "found" "missing or incorrect")
          :face (if have-config 'success '(bold error))))))
    :error-explainer
    (lambda (err)
      (let ((error-code (flycheck-error-id err))
            (url "https://eslint.org/docs/rules/%s"))
        (and error-code
             ;; skip non-builtin rules
             (not ;; `seq-contains-p' is only in seq >= 2.21
              (with-no-warnings (seq-contains error-code ?/)))
             `(url . ,(format url error-code))))))
  (add-to-list 'flycheck-checkers 'npm-eslint-lint)
  (flycheck-add-next-checker 'typescript-tide '(error . npm-eslint-lint)))

(use-package syntax-subword
  :ensure t
  :config
  (setq syntax-subword-skip-spaces t)
  (global-syntax-subword-mode t))

(use-package magit
  :demand t
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :hook (after-save . magit-after-save-refresh-status)
  :config
  (setq magit-delete-by-moving-to-trash nil))

(use-package projectile
  :ensure t
  :demand t
  :bind-keymap (("C-c p" . projectile-command-map))
  :config
  (setq projectile-switch-project-action #'projectile-commander)
  (projectile-mode t))

(use-package graphql-mode
  :ensure t)

(use-package rust-mode
  :mode "\\.rs$"
  :ensure t)

(use-package web-mode
  :ensure t
  :mode ("\\.phtml$" "\\.[tj]sx$" "\\.html.erb")
  :init
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 2))
  :config
  (setq web-mode-engines-alist '(("php" . "\\.phtml$")))
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (add-hook 'web-mode-hook 'ws-butler-mode)
  ;; better syntax highlights for jsx
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))

(use-package nvm
  :ensure t
  :hook ((typescript-mode . nvm-use-for-buffer)
         (js2-mode . nvm-use-for-buffer)
         (web-mode . nvm-use-for-buffer))
  :commands nvm-use-for-buffer)

(use-package js2-mode
  :mode ("\\.js$")
  :ensure t
  :config
  (defun js-custom ()
    "js-mode-hook"
    (setq js-indent-level 2))
  (add-hook 'js2-mode-hook 'js-custom)
  (add-hook 'js2-mode-hook 'ws-butler-mode))

(use-package json-mode
  :ensure t
  :mode "\\.json$")

(use-package eldoc
  :config
  (global-eldoc-mode))

(use-package mustache-mode
  :ensure t)

(use-package typescript-mode
  :ensure t
  :mode "\\.m?tsx?$")

(use-package tide
  :ensure t
  :after projectile
  :commands (tide-setup tide-mode tide-custom)
  :hook ((typescript-mode . tide-custom)
         (js2-mode . tide-custom)
         (web-mode . tide-custom))
  :config
  (defun tide-custom ()
    "tide-mode-hook"
    (interactive)
    (tide-setup)
    (tide-hl-identifier-mode +1)
    (when (string-equal
           "purpose"
           (file-name-nondirectory (directory-file-name (projectile-project-root))))
      (flycheck-add-next-checker 'typescript-tide '(error . koan-lint)))
    (setq flycheck-checker 'typescript-tide))
  :bind (:map tide-mode-map
              ("M-?" . 'tide-references)
              ("M-'" . 'tide-documentation-at-point)
              ("C-c C-t" . 'typescript-transient-menu)))

(use-package prettier
  :ensure t
  :config
  (global-prettier-mode))

(use-package haskell-mode :ensure t)

(use-package phpactor :ensure t :after php-mode)
(use-package php-mode
  :ensure t
  :after magit
  :hook ((php-mode . php-custom)
         (php-mode . ws-butler-mode))
  :config
  (defun php-custom ()
    "php-mode-hook"
    (advice-add 'djr/other-window-first :before 'eww-browse-url)
    (setq c-basic-offset 4
          php-manual-path "~/.emacs.d/docs/php"
          browse-url-browser-function 'eww-browse-url)
    (local-unset-key (kbd "C-c C-r"))
    (set (make-local-variable 'company-backends)
         '(company-phpactor company-files))
    (make-local-variable 'eldoc-documentation-function)
    (setq eldoc-documentation-function 'phpactor-hover))

  (transient-define-prefix php-transient-menu ()
    "Php"
    [["Class"
      ("cc" "Copy" phpactor-copy-class)
      ("cn" "New" phpactor-create-new-class)
      ("cr" "Move" phpactor-move-class)
      ("ci" "Inflect" phpactor-inflect-class)
      ("n"  "Namespace" phpactor-fix-namespace)]
     ["Properties"
      ("a"  "Accessor" phpactor-generate-accessors)
      ("pc" "Constructor" phpactor-complete-constructor)
      ("pm" "Add missing props" phpactor-complete-properties)
      ("r" "Rename var locally" phpactor-rename-variable-local)
      ("R" "Rename var in file" phpactor-rename-variable-file)]
     ["Extract"
      ("ec" "constant" phpactor-extract-constant)
      ;;    ("ee" "expression" phpactor-extract-expression)
      ;;    ("em"  "method" phpactor-extract-method)
      ]
     ["Methods"
      ("i" "Implement Contracts" phpactor-implement-contracts)
      ("m"  "Generate method" phpactor-generate-method)]
     ["Navigate"
      ("x" "List refs" phpactor-list-references)
      ("X" "Replace refs" phpactor-replace-references)
      ("."  "Goto def" phpactor-goto-definition)]
     ["Phpactor"
      ("s" "Status" phpactor-status)
      ("u" "Install" phpactor-install-or-update)]])

  :bind (:map php-mode-map
              ("M-." . 'phpactor-goto-definition)
              ("M-?" . 'phpactor-find-references)
              ("C-c C-p" . 'php-transient-menu)))

(use-package go-mode
  :ensure t
  :hook ((go-mode . go-custom)
         (before-save . gofmt-before-save))
  :config
  (defun go-custom ()
    "go-mode-hook"
    (setq tab-width 4)))


(use-package yaml-mode
  :ensure t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-color "white"))

(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 50))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))


(defun djr/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'djr/kill-this-buffer)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
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

(advice-add 'magit-process-filter
            :after (lambda (proc &rest args)
                     (with-current-buffer (process-buffer proc)
                       (read-only-mode -1)
                       (ansi-color-apply-on-region (point-min) (point-max))
                       (read-only-mode 1))))

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(put 'narrow-to-region 'disabled nil)
(put 'magit-edit-line-commit 'disabled nil)
(put 'list-timers 'disabled nil)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(mustache-mode lsp-mode visual-regexp-steroids visual-regexp rspec-mode robe robe-mode rvm rvm-mode dockerfile-mode markdown-mode phpactor expand-region avy json-mode fic-mode slime tree-sitter haskell haskell-mode tree-sitter-langs graphql-mode company yaml-mode wgrep embark-consult embark consult-flycheck consult prescient orderless marginalia ws-butler web-mode vertico use-package tide terraform-mode syntax-subword sublime-themes rust-mode projectile prettier php-mode moe-theme magit js2-mode go-mode exec-path-from-shell editorconfig beacon afternoon-theme))
 '(resize-mini-windows t)
 '(warning-suppress-types '((comp)))
 '(safe-local-variable-values
   '((tide-tsserver-executable . "node_modules/typescript/bin/tsserver")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#303030" :foreground "#c6c6c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight medium :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(mode-line-active ((t (:inherit mode-line)))))

