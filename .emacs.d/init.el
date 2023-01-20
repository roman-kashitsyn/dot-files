(setq inhibit-startup-screen t
      make-backup-files nil
      visible-bell t
      custom-file "~/.emacs.d/custom.el")

(setf split-height-threshold 300
      split-width-threshold 300)

(setq-default indent-tabs-mode nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(setf epa-pinentry-mode 'loopback)
(setq auth-sources '("~/.authinfo.gpg"))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(load custom-file)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 '(ediff-window-setup-function #'ediff-setup-windows-plain)
 '(ediff-split-window-function 'split-window-horizontally))

(set-frame-font "PragmataPro Mono-11:weight=medium")
;(set-frame-font "JetBrains Mono-10:weight=medium")
(setq-default line-spacing 2)

(defvar packages-to-install
  (list 'company
        'ace-window
        'paredit
        'projectile
        'company
        'company-tabnine
        'colorless-themes
        'use-package
        'bazel
        'eglot
        'format-all
        'flycheck
        'go-mode
        'haskell-mode
        'j-mode
        'hydra
        'magit
        'nix-mode
        'proof-general
        'protobuf-mode
        'pollen-mode
        'markdown-mode
        'racer
        'ripgrep
        'rust-mode
        'helm
        'typescript-mode))

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-archive-priorities '(("melpa-stable" . 20)
                                   ("melpa" . 10)
                                   ("gnu" . 5)))

(let ((not-installed (seq-filter #'(lambda (pack) (not (package-installed-p pack)))
                                 packages-to-install)))
  (when not-installed
    (package-refresh-contents)
    (mapc #'package-install not-installed)))
(package-initialize)

(use-package ace-window
  :ensure t
  :bind (("C-x w" . ace-window)))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'helm)
  :config
  (projectile-mode +1)
  :bind (("C-c p c" . 'projectile-compile-project)
         ("C-c p f" . 'projectile-find-file)))

(use-package hippie-exp
  :ensure t
  :bind (([remap dabbrev-expand] . 'hippie-expand)))

(use-package flycheck)

(use-package colorless-themes
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-downcase nil)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package company-tabnine
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends #'company-tabnine))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package racer
  :ensure t
  :config
  (add-hook 'racer-mode-hook #'company-mode))

(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("C-c C-f" . haskell-mode-stylish-buffer)))

(use-package j-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode)))

(if (executable-find "rg")
 (grep-apply-setting
  'grep-find-command
  '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package protobuf-mode
  :config
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "dfn/proto" '((c-basic-offset . 4) (indent-tabs-mode . nil)) t))))

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))
