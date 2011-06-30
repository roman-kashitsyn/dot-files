;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom functions to make emacs more usable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-lines-below (&optional arg)
  "Inserts empty lines below and moves cursor to bottom one,
just like `o' vi command."
  (interactive "p")
  (progn
    (end-of-line)
    (newline arg)))

(define-key global-map (kbd "C-c o") 'insert-lines-below)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-lines-above (&optional arg)
  "Inserts empty lines above and moves cursor to top one,
just like `O' vi command."
  (interactive "p")
  (progn
    (beginning-of-line)
    (save-excursion
      (newline arg))))

(define-key global-map (kbd "C-c O") 'insert-lines-above)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kill-current-line (&optional arg)
  "Kills current line, just like `dd' vi command."
  (interactive "p")
  (save-excursion
    (progn
      (beginning-of-line)
      (kill-line arg))))

(define-key global-map (kbd "C-c d") 'kill-current-line)
