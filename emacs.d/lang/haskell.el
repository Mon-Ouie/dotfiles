(require 'flymake)
(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'flymake-mode)

(setq haskell-saved-check-command "")

(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
