(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/modules")

(load-library "~/.emacs.d/basics.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar packages-to-install
  (list 'company
        'autopair
        'ace-jump-mode
        'dash
        'smex
        'helm
        'helm-gtags
        'wrap-region
        'expand-region
        'magit
        'git-messenger
        'clojure-mode
        'csharp-mode
        'clang-format
        'projectile
        'ag
        'paredit
        'js2-mode
        'markdown-mode
        'php-mode
        'haskell-mode
        'merlin
        'ocp-indent
        'ensime
        'scala-mode2
        'groovy-mode
        'cmake-mode
        'color-theme
        'multiple-cursors
        'jabber
        'yasnippet
        'sublime-themes
        'iy-go-to-char
        'gtags
        'go-mode))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("milkbox" . "http://melpa.milkbox.net/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)
(dolist (pack packages-to-install)
  (when (not (package-installed-p pack))
    (message "Installing package: %s" pack)
    (package-refresh-contents)
    (package-install pack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules initialization *must* be placed after the package system
;;; initialization.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'my-keybindings)
(require 'my-color-theme)
(require 'my-autocomplete)
(require 'my-orgmode)
(require 'my-cpp)
(require 'my-lisp)
(require 'my-php)
(require 'my-haskell)
(require 'my-ocaml)
(require 'my-scala)
(require 'my-csharp)
(require 'my-js)
(require 'my-markup)
(require 'my-gyp)
(require 'my-gradle)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load-file custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Include machine-specific preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar machine-specific-file "~/.emacs.d/machine-specific.el")
(if (file-exists-p machine-specific-file)
    (load machine-specific-file))
