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

(use-package moe-theme
  :ensure t
  :init
  (load-theme 'moe-dark t))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :bind (("M-A" . marginalia-cycle)
        :map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package prescient
  :ensure t)

(use-package consult-flycheck
  :ensure t)

(use-package consult
  :after compile
  :ensure t
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
         ("C-c ! j" . consult-flycheck)))

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

(use-package emacs
  :init
  (setq read-extended-command-predicate nil))

(use-package ws-butler
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-define-checker koan-lint
    "doc string"
    :command ("yarn" "lint" "--format=json" "--stdin" "--stdin-filename" source-original)
    :standard-input t
    :error-parser flycheck-parse-eslint
    :predicate (lambda () (not (equal (file-name-extension (buffer-file-name)) "json")))
    :enabled (lambda () (flycheck-eslint-config-exists-p))
    :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode
                    typescript-mode)
    :working-directory flycheck-eslint--find-working-directory
    :verify
    (lambda (_)
      (let* ((default-directory
               (flycheck-compute-working-directory 'javascript-eslint))
             (have-config (flycheck-eslint-config-exists-p)))
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

  (flycheck-define-checker koan-test
    "doc string"
    :command ("yarn" "test"
              "--json"
              "--testPathPattern" (eval buffer-file-name)
              "--outputFile" (eval (flycheck-jest--result-path)))
    :error-parser flycheck-jest--parse
    :modes (web-mode js-mode typescript-mode rjsx-mode)
    :predicate (lambda () (funcall #'flycheck-jest--should-use-p))
    :working-directory
    (lambda (checker)
      (flycheck-jest--find-jest-project-directory checker)))
  (add-to-list 'flycheck-checkers 'koan-lint)
  (add-to-list 'flycheck-checkers 'koan-test))

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
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  (setq magit-delete-by-moving-to-trash nil))

(use-package projectile
  :ensure t
  :config
  (setq projectile-switch-project-action #'projectile-commander)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (projectile-mode t))

(use-package rust-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode)))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[tj]sx$" . web-mode))
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

(use-package js2-mode
  :ensure t
  :init
  (defun js-custom ()
    "js-mode-hook"
    (setq js-indent-level 2))
  :config
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-hook 'js2-mode-hook 'js-custom)
  (add-hook 'js2-mode-hook 'ws-butler-mode)
  (add-hook 'js2-mode-hook 'indium-interaction-mode))

(use-package tide
  :demand t
  :ensure t
  :after magit
  :init
  (defun tide-custom ()
    "typescript-mode-hook"
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))
  (transient-define-prefix typescript-transient-menu ()
    "Typescript"
    [["Tide"
      ("d" "Documentantion" tide-documentation-at-point)
      ("?" "References" tide-references)
      ("rs" "Rename symbol" tide-rename-symbol)
      ("rf" "Rename file" tide-rename-file)
      ("rr" "Refactor" tide-refactor)
      ("ri" "Organize Imports" tide-organize-imports)
      ("fo" "Format" tide-format)
      ("fi" "Fix" tide-fix)
      ("xr" "Restart" tide-restart-server)]])
  :config
  (add-hook 'typescript-mode-hook 'tide-custom)
  (add-hook 'web-mode-hook 'tide-custom)
  (add-hook 'js2-mode-hook 'tide-custom)
  (flycheck-add-next-checker 'typescript-tide '(warning . koan-test))
  (flycheck-add-next-checker 'typescript-tide '(error . koan-lint))
  :bind (:map tide-mode-map
              ("M-?" . 'tide-references)
              ("M-'" . 'tide-documentation-at-point)
              ("C-c C-t" . 'typescript-transient-menu)))


;; (use-package phpactor)

(use-package prettier
  :ensure t
  :config
  (global-prettier-mode))

(use-package php-mode
  :ensure t
  :after magit
  :init

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
              ("C-c C-p" . 'php-transient-menu))
  :config
  (add-hook 'php-mode-hook 'php-custom)
  (add-hook 'php-mode-hook 'eldoc-mode)
  (add-hook 'php-mode-hook 'ws-butler-mode))

(use-package go-mode
  :ensure t
  :init
  (defun go-custom ()
    "go-mode-hook"
    (setq tab-width 4))
  :config
  (add-hook 'go-mode-hook 'go-custom))

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
   '(wgrep embark-consult embark consult-flycheck consult prescient orderless marginalia ws-butler web-mode vertico use-package tide terraform-mode syntax-subword sublime-themes rust-mode projectile prettier php-mode moe-theme magit js2-mode go-mode exec-path-from-shell editorconfig beacon afternoon-theme))
 '(resize-mini-windows t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
