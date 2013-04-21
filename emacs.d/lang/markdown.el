(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook 'turn-on-flyspell)
(add-hook 'markdown-mode-hook 'detect-language-per-paragraph)
