(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/modules")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar packages-to-install
  (list 'auto-complete
	'dash
	'wrap-region
	'magit
	'clojure-mode
	'csharp-mode
	'projectile
	'ack-and-a-half
	'paredit
	'js2-mode
	'markdown-mode
	'php-mode
	'haskell-mode
	'scala-mode
	'cmake-mode
	'color-theme
	'multiple-cursors
	'jabber
	'yasnippet))

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)
(dolist (pack packages-to-install)
  (when (not (package-installed-p pack))
    (message "Installing package: %s" pack)
    (package-refresh-contents)
    (package-install pack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'basics)
(require 'my-keybindings)
(require 'my-autocomplete)
(require 'my-cpp)
(require 'my-lisp)
(require 'my-php)
(require 'my-csharp)
(require 'my-js)
(require 'my-markup)
(require 'my-gyp)
(require 'my-jabber)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
