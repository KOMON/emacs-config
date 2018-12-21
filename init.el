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

(use-package flycheck
  :init (global-flycheck-mode))

(use-package yasnippet
  :config
  :init
  (yas-global-mode 1))

(use-package string-inflection)

(use-package syntax-subword
  :config
  (setq syntax-subword-skip-spaces t)
  (global-syntax-subword-mode t))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (global-magit-file-mode 't))

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))


(use-package geiser)

(use-package web-mode
  :ensure
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 2))
  :config
  (setq web-mode-engines-alist '(("php" . "\\.phtml$")))
  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package js
  :init
  (defun js-custom ()
    "js-mode-hook"
    (setq js-indent-level 2))
  :config
  (add-hook
   'js-mode-hook 'js-custom))

(use-package php-mode
  :init
  (defun php-custom ()
    "php-mode-hook"
    (advice-add 'djr/other-window-first :before 'eww-browse-url)
    (setq c-basic-offset 4
          php-manual-path "~/.emacs.d/docs/php"
          browse-url-browser-function 'eww-browse-url)
    (local-unset-key (kbd "C-c C-r")))
  :config
  (add-hook 'php-mode-hook 'php-custom))

(use-package company
  :bind
  (("M-/" . company-complete))
  :config
  (add-hook 'after-init-hook 'global-company-mode))
 
(use-package go-mode
  :init
  (defun go-custom ()
    "go-mode-hook"
    (setq tab-width 4))
  :config
  (add-hook 'go-mode-hook 'go-custom))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package expand-region
  :ensure
  :bind (("C-="     . er/expand-region)
         ("C-c x c" . er/contract-region)
         ("C-c x t" . er/mark-outer-tag)))

(use-package hydra)

(use-package ivy-mode
  :config
  (ivy-mode t)
  (setq enable-recursive-minibuffers t
        ivy-use-selectable-prompt t)
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b"   . ivy-switch-buffer)))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))
  
(use-package swiper)

(defun djr/counsel-M-x ()
  "Call counsel's extended execute without an initial ^."
  (interactive)
  (counsel-M-x ""))

(use-package smex)

(use-package counsel
  :bind (("M-x" . djr/counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-c C-j" . counsel-imenu)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c r" . counsel-rg)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h u" . counsel-unicode-char)
         ("C-s"   . counsel-grep-or-swiper))
  :config
  (define-key read-expression-map (kbd "C-r")
    'counsel-expression-history)
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package beacon
  :ensure
  :config
  (beacon-mode 1)
  (setq beacon-color "white"))

(use-package recentf
  :ensure
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 50)
  :bind ("C-x C-r" . ivy-recentf))

(use-package rainbow-delimiters
  :ensure
  :config
  (rainbow-delimiters-mode t))

(use-package change-inner
  :ensure
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package zzz-to-char
  :ensure
  :bind (("M-z" . zzz-up-to-char)
         ("M-Z" . zzz-to-char)))

(use-package avy
  :ensure
  :config (setq avy-keys
                (nconc (number-sequence ?a ?z)
                       (number-sequence ?A ?Z)
                       (number-sequence ?1 ?9)
                       '(?0)))
  :bind (("M-g c" . avy-goto-char)
         ("M-g a" . avy-goto-char-timer)))

(use-package tramp
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("e2fd81495089dc09d14a88f29dfdff7645f213e2c03650ac2dd275de52a513de" "ed0b4fc082715fc1d6a547650752cd8ec76c400ef72eb159543db1770a27caa7" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "4ee4a855548a7a966fe8722401441499b0d8b2fcf3d12438f81e016b6efed0e6" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "d411730c6ed8440b4a2b92948d997c4b71332acf9bb13b31e9445da16445fe43" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "17cda1304ba8d26d62bf247cab2c161d12957054b6be4477abb5972a74eea4e1" "715fdcd387af7e963abca6765bd7c2b37e76154e65401cd8d86104f22dd88404" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"#######..#\" };")))
 '(fci-rule-color "#222222")
 '(global-aggressive-indent-mode nil)
 '(gnus-logo-colors (quote ("#528d8d" "#c0c0c0")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"###########.######\" };")) t)
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (ivy-phpunit ac-php string-inflection yasnippet loccur git-gutter company-php company markdown-mode nginx-mode jinja2-mode django-mode ivy-prescient flymake-python-pyflakes avy wgrep doom-themes flycheck syntax-subword ivy-hydra material-theme sublime-themes racket-mode yaml-mode puppet-mode dumb-jump zenburn-theme gruvbox-theme alect-themes organic-green-theme hamburg-theme counsel-projectile projectile ivy-mode exec-path-from-shell vagrant-tramp magit dart-mode paredit geiser slime counsel swiper ivy beacon use-package change-inner ido-grid-mode ido-vertical-mode ido-ubiquitous expand-region go-mode lua-mode gnu-apl-mode emmet-mode sql-indent php-mode web-mode abyss-theme rainbow-delimiters flx-ido flx smex)))
 '(projectile-mode t nil (projectile))
 '(tab-width 4)
 '(vc-annotate-background "#222222")
 '(vc-annotate-color-map
   (quote
    ((20 . "#fa5151")
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
     (360 . "#e353b9"))))
 '(vc-annotate-very-old-color "#e353b9"))
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

(load-theme 'gruvbox-dark-hard)
;;; init.el ends here
