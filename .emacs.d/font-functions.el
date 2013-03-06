(require 'cl)

(defun make-font (family size)
  "Constructs new font from specified name and size."
  (cons family size))

(defun get-font-family (font)
  "Returns font family."
  (car font))

(defun get-font-size (font)
  "Returns font size."
  (cdr font))

(defun make-font-name (font)
  "Creates font from name and size."
  (concat (get-font-family font) "-"
	  (number-to-string (get-font-size font))))

(defun font-exist-p (font)
  "Checks if font with specified name exist."
  (not (null (member (get-font-family font)
		     (font-family-list)))))

(defun setup-default-font (font-list)
  "Setup default font using specified font list."
  (let ((existing-fonts
	 (remove-if-not 'font-exist-p font-list)))
    (if existing-fonts
	(set-face-attribute
	 'default nil :font
	 (make-font-name (car existing-fonts))))))

(defun use-font (font)
  "Tries to setup specified font"
  (setup-default-font (list font)))
