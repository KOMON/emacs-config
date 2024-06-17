;;; Init.el --- Load 'er up Johnny.
;;; Commentary:
;;; My havent' we grown over time?

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
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'use-package)

(require 'ui)
(require 'platform)
(require 'editing)
(require 'keys)

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa"))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package markdown-mode
  :ensure t
  :bind (("C-M-%" . vr/query-replace)))

(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile")

(use-package visual-regexp :ensure t)
(use-package visual-regexp-steroids :ensure t)

(use-package vterm :ensure t)

(use-package rvm
  :ensure t
  :init
  (rvm-use-default))

(use-package moe-theme
  :ensure t
  :init
  (load-theme 'moe-dark 't))

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
              ("C-l" . vertico-directory-delete-word))
  :init
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

(use-package rspec-mode
  :ensure t
  :init)

(use-package typescript-mode :ensure t)

;; (use-package tree-sitter
;;   :hook (tree-sitter-after-on . tree-sitter-hl-mode)
;;   :config
;;   (global-tree-sitter-mode))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter)

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
  (global-company-mode 1))

(use-package emacs
  :init
  (setq read-extended-command-predicate nil)
  :config
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode))))

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
  (setq flycheck-emacs-lisp-load-path 'inherit)
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
                    typescript-mode typescript-ts-mode)
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
         ("C-x M-g" . magit-dispatch))
  :hook (after-save . magit-after-save-refresh-status)
  :config
  (setq magit-delete-by-moving-to-trash nil)
  (setq auth-sources '("~/.authinfo")))

(use-package forge :ensure t)

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
  :ensure t
  :hook ((rust-mode . lsp))
  :config
  (setq rust-format-on-save t))

(use-package web-mode
  :ensure t
  :after tide
  :mode ("\\.phtml$" "\\.html.erb")
  :init
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 2)
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (tide-setup)))
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

;; (use-package nvm
;;   :ensure t
;;   :hook ((typescript-mode . nvm-use-for-buffer)
;;          (typescript-ts-mode . nvm-use-for-buffer)
;;          (js2-mode . nvm-use-for-buffer)
;;          (web-mode . nvm-use-for-buffer))
;;   :commands (nvm-use-for-buffer))

(use-package asdf
  :ensure t
  :straight (asdf :type git :host github :repo "tabfugnic/asdf.el")
  :config
  (asdf-enable))

(use-package js2-mode
  :mode ("\\.[cm]?js$")
  :ensure t
  :config
  (defun js-custom ()
    "js-mode-hook"
    (setq js-indent-level 2))
  (add-hook 'js2-mode-hook 'js-custom)
  (add-hook 'js2-mode-hook 'ws-butler-mode)
  :bind (:map js2-mode-map (("C-c C-p" . 'djr/find-dominating-package-json))))

(use-package json-mode
  :ensure t
  :mode "\\.json$"
  :bind (:map json-mode-map
              (("C-c C-p" . 'djr/find-dominating-package-json))))

(use-package eldoc
  :config
  (global-eldoc-mode))

(use-package mustache-mode
  :ensure t)

(use-package add-node-modules-path
  :ensure t)

(use-package
  typescript-ts-mode
  :mode (("\\.m?ts$" . typescript-ts-mode)
         ("\\.m?tsx$" . tsx-ts-mode))
  :hook ((typescript-ts-mode . add-node-modules-path)
         (tsx-ts-mode . add-node-modules-path)))
  

(defun tide--npmmonorepo ()
  "Return a single path to the package-local typescript package directory for a monorepo or nil"
  (-when-let (packages-folder (locate-dominating-file default-directory "package.json"))
	     (-when-let (monorepo-folder (locate-dominating-file (file-name-directory (directory-file-name packages-folder)) "package.json"))
			(concat monorepo-folder "node_modules/typescript/lib/"))))
(defun tide-tsserver-locater-npmlocal-npmmonorepo-projectile-npmglobal ()
  "Locate tsserver through project-local or global system settings."
  (or
   (tide--locate-tsserver (tide--npm-local))
   (tide--locate-tsserver (tide--npmmonorepo))
   (tide--locate-tsserver (tide--project-package))
   (tide--locate-tsserver (tide--npm-global))
   (tide--locate-tsserver (tide--npm-global-usrlocal))))

(use-package tide
  :ensure t
  :after projectile
  :commands (tide-setup tide-mode tide-custom)
  :hook ((typescript-mode . tide-custom)
         (typescript-ts-mode . tide-custom)
         (tsx-ts-mode . tide-custom)
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
              ("C-c C-t" . 'typescript-transient-menu)
              ("C-c C-p" . 'djr/find-dominating-package-json)))

(use-package prettier
  :ensure t
  :config
  (global-prettier-mode))

(use-package haskell-mode :ensure t)

;; (use-package phpactor :ensure t :after php-mode)
;; (use-package php-mode
;;   :ensure t
;;   :after magit
;;   :hook ((php-mode . php-custom)
;;          (php-mode . ws-butler-mode))
;;   :config
;;   :bind (:map php-mode-map
;;               ("M-." . 'phpactor-goto-definition)
;;               ("M-?" . 'phpactor-find-references)
;;               ("C-c C-p" . 'php-transient-menu)))

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

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(defun djr/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'djr/kill-this-buffer)

(defun djr/find-buffer-dominating-file ()
  "Find dominating file-name relative to current buffer"
  (interactive)
  (let ((file-name (read-string "Enter dominating file name:")))
    (djr/find-dominating-file (buffer-file-name) file-name)))

(defun djr/find-dominating-file (from-file-name dominating-file-name)
  (find-file
   (concat
    (locate-dominating-file from-file-name dominating-file-name)
    dominating-file-name)))

(defun djr/find-dominating-package-json ()
  (interactive)
  (djr/find-dominating-file (buffer-file-name) "package.json"))

(use-package uuidgen :ensure t)

(defun djr/regex-replace-with-unique-string (pattern)
  "Find each instance of pattern and replace with a unique string"
  (while (re-search-forward pattern nil t)
    (replace-match )))

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
 '(custom-safe-themes
   '("27a1dd6378f3782a593cc83e108a35c2b93e5ecc3bd9057313e1d88462701fcd" default))
 '(flycheck-checker-error-threshold 1000)
 '(package-selected-packages
   '(yaml-mode ws-butler web-mode vterm visual-regexp-steroids vertico uuidgen
               use-package typescript-mode tree-sitter-langs tide terraform-mode
               syntax-subword rvm rust-mode rspec-mode robe rg purescript-mode
               projectile prettier prescient phpactor php-mode orderless
               mustache-mode moe-theme mermaid-mode marginalia lua-mode
               kubernetes json-mode js2-mode haskell-mode graphql-mode go-mode
               forge fic-mode expand-region exec-path-from-shell embark-consult
               emacsql-sqlite-builtin dockerfile-mode csharp-mode
               consult-flycheck company beacon add-node-modules-path ace-window))
 '(resize-mini-windows t)
 '(tide-tsserver-executable nil)
 '(tide-tsserver-locator-function
   'tide-tsserver-locater-npmlocal-npmmonorepo-projectile-npmglobal)
 '(treesit-font-lock-level 4)
 '(typescript-indent-level 2)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#303030" :foreground "#c6c6c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(font-lock-bracket-face ((t nil)))
 '(font-lock-function-name-face ((t (:foreground "#ffd700"))))
 '(font-lock-number-face ((t (:foreground "#d18aff"))))
 '(font-lock-operator-face ((t (:foreground "#a1db00"))))
 '(font-lock-punctuation-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(mode-line-active ((t (:inherit mode-line)))))



