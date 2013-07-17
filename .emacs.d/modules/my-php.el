;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PHP Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flymake-php-init ()
  "Use php to check the syntax of the current file."
  (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
	 (local (file-relative-name temp (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local "-l")))

  (add-to-list 'flymake-err-line-patterns 
	       '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))

  (add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init)))

(add-hook 'php-mode-hook '(lambda ()
			    (flymake-mode 1)
			    (setup-default-c-identation)
			    (define-key php-mode-map '[M-S-up] 'flymake-goto-prev-error)
			    (define-key php-mode-map '[M-S-down] 'flymake-goto-next-error)))

(add-to-list 'auto-mode-alist
	     '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))

(provide 'my-php)
