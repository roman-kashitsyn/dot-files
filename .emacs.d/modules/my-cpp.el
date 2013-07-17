;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C++ indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setup-default-c-indentation ()
  "Setup default indentation for c-like languages."
  (setq indent-tabs-mode nil
        show-trailing-whitespace t
        c-basic-offset 4
        tab-width 4)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'topmost-intro-cont 0)
  (c-set-offset 'access-label '-))

(add-hook 'c-mode-common-hook 'setup-default-c-indentation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CEDET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-cedet-home "~/cedet-1.1")

(defun init-cedet ()
  (load-file (expand-file-name "cedet.el"
			       (expand-file-name "common"
						 my-cedet-home)))

  ;; Enable EDE (Project Management) features
  (require 'semantic-lex-spp)
  (global-ede-mode t)
  (ede-enable-generic-projects)
  ;; Enable EDE for a pre-existing C++ project
  ;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")

  ;; Enabling Semantic (code-parsing, smart completion) features
  ;; Select one of the following:

  ;; * This enables the database and idle reparse engines
  ;;(semanqtic-load-enable-minimum-features)

  ;; * This enables some tools useful for coding, such as summary mode,
  ;;   imenu support, and the semantic navigator
  (semantic-load-enable-code-helpers)

  ;; * This enables even more coding tools such as intellisense mode,
  ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
  (semantic-load-enable-gaudy-code-helpers)

  ;; Enable SRecode (Template management) minor-mode.
  (global-srecode-minor-mode 1))

(defun my-cedet-hook ()
  (init-cedet)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook 'my-cedet-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clang integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enable-clang-tools ()
  (load-library "clang-format")
  (load-library "clang-completion-mode"))

(add-hook 'c-mode-common-hook 'enable-clang-tools)

(provide 'my-cpp)
