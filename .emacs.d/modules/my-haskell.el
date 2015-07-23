(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))

(eval-after-load 'haskell-cabal
    '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(provide 'my-haskell)
