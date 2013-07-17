(defun turn-on-paredit-mode ()
  (paredit-mode +1))
(add-hook 'clojure-mode-hook 'turn-on-paredit-mode)
(add-hook 'lisp-mode-hook 'turn-on-paredit-mode)

(provide 'my-lisp)
