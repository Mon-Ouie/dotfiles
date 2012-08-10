(require 'flymake)
(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'flymake-mode)
