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
(repeat-mode 1)

(require 'epa-file)
(setf epa-pinentry-mode 'loopback)
(setq auth-sources '("~/.authinfo.gpg"))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(load custom-file t)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 '(ediff-window-setup-function #'ediff-setup-windows-plain)
 '(ediff-split-window-function 'split-window-horizontally))

;; Prevent the compile command from truncating lines
(setq compilation-max-output-line-length nil)

;; Re-stolen from https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq redisplay-skip-fontification-on-input t)
(setq read-process-output-max (* 4 1024 1024))
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq save-interprogram-paste-before-kill t)
(setq kill-do-not-save-duplicates t)
(setq ffap-machine-p-known 'reject)
(setq window-combination-resize t)
(setq set-mark-command-repeat-pop t)
(setq help-window-select t)

;; See https://emacsredux.com/blog/2025/03/18/you-have-no-idea-how-powerful-isearch-is/
(setq isearch-allow-motion t
      isearch-motion-changes-direction t)

;;(set-frame-font "PragmataPro Mono-11:weight=medium")
;;(set-frame-font "PragmataPro Mono-12:weight=medium")
;;(set-frame-font "PragmataPro Mono-13:weight=medium")
;;(set-frame-font "PragmataPro Mono 1.3-11:weight=medium")
;;(set-frame-font "PragmataPro Mono 1.3-12:weight=medium")
;;(set-frame-font "PragmataPro Mono 1.3-14:weight=medium")
;;(set-frame-font "Iosevka Charon Mono-11:weight=medium")
;;(set-frame-font "Iosevka Charon Mono-12:weight=regular")
;;(set-frame-font "Iosevka Charon Mono-14:weight=medium")
;;(set-frame-font "Iosevka-12:weight=medium")
;;(set-frame-font "Iosevka-14:weight=medium")
;;(set-frame-font "Iosevka SS08-12:weight=regular")
;;(set-frame-font "Aporetic Serif Mono-12:weight=medium")
;;(set-frame-font "Aporetic Sans Mono-12:weight=medium")
;;(set-frame-font "PragmataPro Mono-13:weight=medium")
;;(set-frame-font "JetBrains Mono-12:weight=regular")
;;(set-frame-font "DM Mono-12:weight=medium")
;;(set-frame-font "Inconsolata-12:weight=medium")
;;(set-frame-font "JuliaMono-12:weight=medium")
;;(set-frame-font "M PLUS 1 Code-12:weight=medium")
;;(set-frame-font "Cartograph CF-12:weight=medium")
;;(setq-default line-spacing 0)
;;(setq-default line-spacing 5)

;; Custom functions
(defun my/vim-open-line-below ()
  "Open a line below the current one, like Vim's o."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun my/vim-open-line-above ()
  "Open a line above the current one, like Vim's O."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "C-o") #'my/vim-open-line-below)

;; Package management

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-archive-priorities '(("melpa-stable" . 10)
                                   ("melpa" . 20)
                                   ("gnu" . 5)))

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
  (setq treesit-font-lock-level 4)
  (treesit-font-lock-recompute-features)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go" "v0.25.0")
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
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))

(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package go-ts-mode
  :ensure t)

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "SSH_AGENT_PID")
    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")))

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package devdocs
  :ensure t
  :config
  (global-set-key (kbd "C-h D") 'devdocs-lookup)
  (add-hook 'go-mode-hook
            (lambda () (setq-local devdocs-current-docs '("go")))))

(use-package dictionary
  :config
  (global-set-key (kbd "C-c l") #'dictionary-lookup-definition))

;; "Modal" editing with view mode.
;; Inspired by https://www.emacs.dyerdwelling.family/emacs/20250731123820-emacs--discovering-view-mode-emacss-hidden-modal-editing-gem/
(use-package view
  :ensure nil
  :config
  ;; Enable view-mode when entering read-only
  (setq view-read-only t)
  (define-key view-mode-map (kbd "n") 'next-line)
  (define-key view-mode-map (kbd "p") 'previous-line)
  (define-key view-mode-map (kbd "f") 'forward-char)
  (define-key view-mode-map (kbd "b") 'backward-char)

  ;; Beginning/end of line
  (define-key view-mode-map (kbd "a") 'beginning-of-line)
  (define-key view-mode-map (kbd "e") 'end-of-line)

  ;; Vim-ish keys

  (defun my/view-exit-open-line-below ()
    (interactive)
    (View-exit-and-edit)
    (my/vim-open-line-below))

  (defun my/view-exit-open-line-above ()
    (interactive)
    (View-exit-and-edit)
    (my/vim-open-line-above))

  (defun my/vim-window-top ()
    "Move point to the top of the window, like Vim's H."
    (interactive)
    (move-to-window-line 0))

  (defun my/vim-window-middle ()
    "Move point to the middle of the window, like Vim's M."
    (interactive)
    (move-to-window-line nil))

  (defun my/vim-window-bottom ()
    "Move point to the bottom of the window, like Vim's L."
    (interactive)
    (move-to-window-line -1))


  ;; Quick exit to edit mode
  (define-key view-mode-map (kbd "i") 'View-exit-and-edit)
  (define-key view-mode-map (kbd "o") 'my/view-exit-open-line-below)
  (define-key view-mode-map (kbd "O") 'my/view-exit-open-line-above)

  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "h") 'backward-char)
  (define-key view-mode-map (kbd "l") 'forward-char)
  (define-key view-mode-map (kbd "w") 'forward-word)
  (define-key view-mode-map (kbd "b") 'backward-word)
  (define-key view-mode-map (kbd "*") 'isearch-forward-symbol-at-point)
  (define-key view-mode-map (kbd "H") 'my/vim-window-top)
  (define-key view-mode-map (kbd "M") 'my/vim-window-middle)
  (define-key view-mode-map (kbd "L") 'my/vim-window-bottom)

  ;; Page movement
  (define-key view-mode-map (kbd "u") '(lambda()
                                         (interactive)
                                         (View-scroll-page-backward 3)))
  (define-key view-mode-map (kbd "d") '(lambda()
                                         (interactive)
                                         (View-scroll-page-forward 3)))

  ;; Beginning/end of line (Vim style)
  (define-key view-mode-map (kbd "0") 'beginning-of-line)
  (define-key view-mode-map (kbd "$") 'end-of-line)

  ;; Beginning/end of buffers
  (define-key view-mode-map (kbd "g") 'beginning-of-buffer)
  (define-key view-mode-map (kbd "G") 'end-of-buffer)

  ;; Quick toggle keys
  (global-set-key (kbd "C-<escape>") 'view-mode)
  (global-set-key (kbd "C-<tab>") 'view-mode)
  ;; Visual feedback - box cursor in view mode, bar when editing
  (add-hook 'view-mode-hook
            (defun view-mode-hookee+ ()
              (setq cursor-type (if view-mode 'box 'bar))))  
  )



