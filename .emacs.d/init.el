(setq inhibit-startup-screen t
      make-backup-files nil
      create-lockfiles nil
      ring-bell-function 'ignore
      visible-bell t
      custom-file "~/.emacs.d/custom.el")

(setf split-height-threshold 300
      split-width-threshold 300)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq-default cursor-type 'bar)

(setq project-vc-extra-root-markers '(".project.el" ".projectile" "pyproject.toml"))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(column-number-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode t)

(require 'epa-file)
(setf epa-pinentry-mode 'loopback)
(setq auth-sources '("~/.authinfo.gpg"))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(load custom-file)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 '(ediff-window-setup-function #'ediff-setup-windows-plain)
 '(ediff-split-window-function 'split-window-horizontally))

(set-frame-font "PragmataPro Mono-11:weight=medium")
;(set-frame-font "PragmataPro Mono-11:weight=medium")
;(set-frame-font "Aporetic Serif Mono-12:weight=medium")
;(set-frame-font "Aporetic Sans Mono-12:weight=medium")
;(set-frame-font "PragmataPro Mono-13:weight=medium")
;(set-frame-font "JetBrains Mono-10:weight=medium")
;(setq-default line-spacing 2)
(setq-default line-spacing 1)

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
        'hydra
        'magit
        'nix-mode
        'proof-general
        'protobuf-mode
        'markdown-mode
        'ripgrep
        'rust-mode
        'helm
        'typescript-mode
        'yaml-mode
        'yaml-pro))

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

(use-package flycheck
  :ensure t)

(use-package colorless-themes
  :ensure t)

(use-package darkokai-theme
  :ensure t
  :config (load-theme 'darkokai t))

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

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("C-c C-f" . haskell-mode-stylish-buffer)))

(if (executable-find "rg")
 (grep-apply-setting
  'grep-find-command
  '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package protobuf-mode
  :config
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "dfn/proto" '((c-basic-offset . 4) (indent-tabs-mode . nil)) t))))

(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always)
  (ido-mode 1))

(use-package ido-vertical-mode
  :after ido
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-completing-read+
  :after ido
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  (smex-initialize))

;; (use-package helm
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "M-x") #'helm-M-x)
;;   (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;;   (global-set-key (kbd "C-x C-f") #'helm-find-files)
;;   (helm-mode 1))

(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'gofmt-before-save)
                            )))
(use-package yaml-mode
  :ensure t)

(use-package yaml-pro
  :ensure t)

(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
              ("C-c C-d" . eldoc)
              ("<f2>" . eglot-rename)
              ("C-c C-f" . eglot-format-buffer))
  :hook (python-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(python-ts-mode "/opt/homebrew/bin/pyright")))

(use-package treesit
  :config
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(use-package wolfram-mode
  :ensure t)

(use-package merlin
  :ensure t)

(use-package doom-modeline
  :ensure t)

(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package dune
  :ensure t)

(use-package merlin
  :ensure t
  :config
  (setq merlin-error-after-save t)
  (let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      ;; Register Merlin
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'merlin-mode "merlin" nil t nil)
      ;; Automatically start it in OCaml buffers
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)
      ;; Use opam switch to lookup ocamlmerlin binary
      (setq merlin-command 'opam)
      ;; To easily change opam switches within a given Emacs session, you can
      ;; install the minor mode https://github.com/ProofGeneral/opam-switch-mode
      ;; and use one of its "OPSW" menus.
      )))

(use-package merlin-eldoc
  :ensure t
  :hook ((tuareg-mode) . merlin-eldoc-setup))

(use-package flycheck-ocaml
  :ensure t
  :config
  (flycheck-ocaml-setup))

(use-package julia-repl
 :ensure t)

(use-package eglot-jl
 :ensure t)

(use-package flycheck-julia
 :ensure t)

(use-package julia-mode
  :ensure t
  :mode "\\.jl\\'"
  :interpreter ("julia" . julia-mode)
  :init
  (setenv "JULIA_NUM_THREADS" "8")
  :config
  (add-hook 'julia-mode-hook 'eglot-jl-init)
  (add-hook 'julia-mode-hook (lambda () (setq eglot-connect-timeout 120)))
  (add-hook 'julia-mode-hook (lambda () (setq eglot-autoshutdown t)))
  (add-hook 'julia-mode-hook 'company-mode)
  (add-hook 'julia-mode-hook (lambda () (setq julia-repl-set-terminal-backend 'vterm))))
