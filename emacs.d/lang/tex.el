(dolist (hook '(LaTeX-mode-hook latex-mode-hook))
  (add-hook hook
            (lambda()
              (flyspell-mode t)
              (auto-fill-mode t))))
