;;; init.el --- Load 'er up Johnny.
;;; Commentary:
;;; My havent' we grown over time?

;;; Code:
(add-to-list 'load-path (expand-file-name "~/.emacs.d/djr"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/conf"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa"))
(package-initialize)

(require 'ui)
(require 'keys)
(require 'editing)
(require 'platform)
(require 'use-package)
(require 'djr)
(use-package package
  :config
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/")
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa"))

(use-package moe-theme
  :ensure t
  :init
  (load-theme 'moe-dark t)
  :config
  (setq moe-theme-highlight-buffer-id t))

(use-package org
  :config
  (setq org-agenda-files
        '("~/org/work.org" "~/org/home.org"))
  :bind
  (("C-c l" . 'org-store-link)
   ("C-c a" . 'org-agenda)))

(use-package auto-virtualenvwrapper
  :ensure t
  :config
  (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
  (add-hook 'projectile-after-switch-project-hook #'auto-virtualenvwrapper-activate))

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

(use-package flycheck-jest
  :ensure t
  :config
  (flycheck-jest-setup))

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
    (tide-hl-identifier-mode +1)
    (company-mode +1))
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

(use-package indium)

(use-package yasnippet
  :ensure t
  :config
  :init
  (yas-global-mode 1))

(use-package string-inflection
  :ensure t)

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
  (add-hook 'after-save-hook 'magit-after-save-refresh-status))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode -1))

(use-package forge
  :after magit)

(use-package projectile
  :ensure t
  :config
   (setq projectile-completion-system 'helm
        projectile-switch-project-action #'projectile-commander)

  :bind-keymap ("C-c p" . projectile-command-map))

(use-package flycheck-projectile)

(use-package geiser
  :ensure t)

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

;; (use-package phpactor)
;; (use-package company-phpactor
;;   :config
;;   (add-to-list 'company-backends 'company-phpactor))

(use-package prettier
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

(use-package company
  :ensure t
  :bind
  (("M-/" . company-complete))
  :config
  (global-company-mode 1))


(use-package go-mode
  :ensure t
  :init
  (defun go-custom ()
    "go-mode-hook"
    (setq tab-width 4))
  :config
  (add-hook 'go-mode-hook 'go-custom))


(use-package expand-region
  :ensure t
  :bind (("C-="     . er/expand-region)
         ("C-c x c" . er/contract-region)
         ("C-c x t" . er/mark-outer-tag)))

(use-package hydra
  :ensure t)

(use-package helm
  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c j" . helm-grep-do-git-grep)
         ("C-c C-j" . helm-imenu)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (setq-default helm-split-window-inside-p t
                helm-display-header-line nil
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-apropos-fuzzy-match t
                helm-window-prefer-horizontal-split))

(use-package helm-config
  :config
  (helm-mode 1))

(use-package helm-swoop
  :ensure t
  :config
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-multi-swoop)
         ("C-c M-I" . helm-multi-swoop-all)
         :map helm-swoop-map
         ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop)
         :map isearch-mode-map
         ("C-s" . helm-swoop-from-isearch)))

(use-package helm-projectile
  :ensure t
  :bind (:map projectile-command-map
              ("SPC" . helm-projectile))
  :config
  (helm-projectile-on))

;; (use-package selectrum
;;   :config
;;   (selectrum-mode +1)
;;   (selectrum-prescient-mode +1)
;;   (prescient-persist-mode +1))

;; (use-package consult
;;   :bind (("C-c h" . consult-history)
;;          ("C-c o" . consult-outline)
;;          ("C-x b" . consult-buffer)
;;          ("C-x 4 b" . consult-buffer-other-window)
;;          ("C-x 5 b" . consult-buffer-other-frame)
;;          ("C-x r x" . consult-register)
;;          ("C-x r b" . consult-bookmark)
;;          ("M-s o" . consult-outline)
;;          ("M-s m" . consult-mark)
;;          ("M-s l" . consult-line)
;;          ("M-s M-l" . consult-line)
;;          ("M-s i" . consult-imenu)
;;          ("M-s e" . consult-error)
;;          ("M-s m" . consult-multi-occur)
;;          ("M-y" . consult-yank-pop)
;;          ("C-c ! !" . consult-flycheck)
;;          ("<help> a" . consult-apropos))
;;   :init
;;   (fset 'multi-occur #'consult-multi-occur)

;;   :config
;;   (consult-preview-mode))

;; (use-package marginalia
;;   :init
;;   (marginalia-mode)
;;   (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
;;   (setq marginalia-command-categories
;;         '((imenu . imenu)
;;           (projectile-find-file . file)
;;           (projectile-find-file-other-window . file)
;;           (projectile-find-dir . file)
;;           (projectile-find-test-file . file)
;;           (projectile-recentf . file)
;;           (projectile-find-dir-other-window . file)
;;           (projectile-switch-to-buffer . buffer)
;;           (projectile-switch-to-buffer-other-window . buffer)
;;           (projectile-display-buffer . buffer))))

(use-package smex
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

(use-package rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode t))

(use-package zzz-to-char
  :ensure t
  :bind (("M-z" . zzz-up-to-char)
         ("M-Z" . zzz-to-char)))

(use-package avy
  :ensure t
  :config (setq avy-keys
                (nconc (number-sequence ?a ?z)
                       (number-sequence ?A ?Z)
                       (number-sequence ?1 ?9)
                       '(?0)))
  :bind (("M-g c" . avy-goto-char)
         ("M-g a" . avy-goto-char-timer)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package tramp
  :ensure t
  :config 
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-remote-path "/usr/local/git/bin"))

(defun djr/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'djr/kill-this-buffer)

(defadvice eww-browse-url (before other-window-first activate)
  "Browse a url in another window."
  (other-window 1))

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
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

(defun djr/eshell-clear-buffer ()
  "Clear Terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-l") 'djr/eshell-clear-buffer)))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(company-backends
   '(company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                   (company-dabbrev-code company-etags company-keywords)
                   company-oddmuse company-dabbrev))
 '(custom-enabled-themes '(moe-dark))
 '(custom-safe-themes
   '("7675ffd2f5cb01a7aab53bcdd702fa019b56c764900f2eea0f74ccfc8e854386" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "e2fd81495089dc09d14a88f29dfdff7645f213e2c03650ac2dd275de52a513de" "ed0b4fc082715fc1d6a547650752cd8ec76c400ef72eb159543db1770a27caa7" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "4ee4a855548a7a966fe8722401441499b0d8b2fcf3d12438f81e016b6efed0e6" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "d411730c6ed8440b4a2b92948d997c4b71332acf9bb13b31e9445da16445fe43" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "17cda1304ba8d26d62bf247cab2c161d12957054b6be4477abb5972a74eea4e1" "715fdcd387af7e963abca6765bd7c2b37e76154e65401cd8d86104f22dd88404" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" default))
 '(debug-on-error nil)
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(dired-bind-jump nil)
 '(display-line-numbers-type t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(fci-rule-color "#222222")
 '(flycheck-php-phpcs-executable "/usr/local/bin/phpcs")
 '(flycheck-php-phpmd-executable "/usr/local/bin/phpmd")
 '(flycheck-phpcs-standard "PSR2")
 '(geben-dbgp-default-port 9001)
 '(geben-path-mappings '(("/Users/danr/src/magento-commerce-tfaw/tfaw_23" "/app")))
 '(geben-pause-at-entry-line nil)
 '(geben-predefined-breakpoints nil)
 '(geben-query-on-clear-breakpoints nil)
 '(ggtags-auto-jump-to-match 'history)
 '(global-aggressive-indent-mode nil)
 '(global-flycheck-mode t)
 '(global-prettier-mode t)
 '(gnus-logo-colors '("#528d8d" "#c0c0c0") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(helm-ff-keep-cached-candidates nil)
 '(helm-grep-file-path-style 'absolute)
 '(js-indent-level 2)
 '(org-agenda-files
   '("~/src/puppetstack/meeting-notes.org" "~/org/work.org" "~/org/home.org"))
 '(org-startup-truncated nil)
 '(org-use-speed-commands t)
 '(package-selected-packages
   '(php-mode projectile-ripgrep ripgrep consult-selectrum consult-flycheck flycheck-projectile marginalia consult selectrum-prescient selectrum prettier dockerfile-mode graphql-mode rjsx-mode htmlize feature-mode indium terraform-mode tide typescript-mode flycheck-jest editorconfig handlebars-mode geben-helm-projectile auto-virtualenvwrapper magit-todos forge helm-rg moe-theme helm-swoop helm-smex helm-projectile helm js2-mode company-phpactor phpactor geben rust-mode counsel-gtags ws-butler ggtags ivy-phpunit ac-php string-inflection yasnippet loccur git-gutter company-php company markdown-mode nginx-mode jinja2-mode django-mode ivy-prescient flymake-python-pyflakes avy wgrep doom-themes flycheck syntax-subword ivy-hydra material-theme sublime-themes racket-mode yaml-mode puppet-mode dumb-jump zenburn-theme gruvbox-theme alect-themes organic-green-theme hamburg-theme counsel-projectile projectile ivy-mode exec-path-from-shell vagrant-tramp magit dart-mode paredit geiser slime counsel swiper ivy beacon use-package change-inner ido-grid-mode ido-vertical-mode ido-ubiquitous expand-region go-mode lua-mode gnu-apl-mode emmet-mode sql-indent web-mode abyss-theme rainbow-delimiters flx-ido flx smex))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#1d2021"))
 '(php-mode-template-compatibility nil)
 '(phpactor-executable "/Users/danr/.emacs.d/phpactor/phpactor.sh")
 '(prettier-enabled-parsers
   '(babel babel-flow css flow json json5 json-stringify less html markdown scss typescript vue yaml))
 '(projectile-enable-caching nil)
 '(projectile-mode t nil (projectile))
 '(tab-width 4)
 '(tide-always-show-documentation t)
 '(vc-annotate-background "#222222")
 '(vc-annotate-color-map
   '((20 . "#fa5151")
     (40 . "#ea3838")
     (60 . "#f8ffa0")
     (80 . "#e8e815")
     (100 . "#fe8b04")
     (120 . "#e5c900")
     (140 . "#32cd32")
     (160 . "#8ce096")
     (180 . "#7fb07f")
     (200 . "#3cb370")
     (220 . "#099709")
     (240 . "#2fdbde")
     (260 . "#1fb3b3")
     (280 . "#8cf1f1")
     (300 . "#94bff3")
     (320 . "#62b6ea")
     (340 . "#30a5f5")
     (360 . "#e353b9")))
 '(vc-annotate-very-old-color "#e353b9")
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-markup-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(put 'narrow-to-region 'disabled nil)
;;; init.el ends here
(put 'magit-edit-line-commit 'disabled nil)
(put 'list-timers 'disabled nil)
