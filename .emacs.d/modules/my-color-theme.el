(when window-system
  (require 'color-theme)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes/solarized")
  (load-theme 'monokai t))

(provide 'my-color-theme)
