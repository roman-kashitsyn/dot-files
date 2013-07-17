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
	'paredit
	'js2-mode
	'markdown-mode
	'php-mode
	'scala-mode
	'color-theme
	'multiple-cursors
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
(require 'my-autocomplete)
(require 'my-cpp)
(require 'my-lisp)
(require 'my-php)
(require 'my-csharp)
(require 'my-js)
(require 'my-markup)
(require 'my-gyp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
