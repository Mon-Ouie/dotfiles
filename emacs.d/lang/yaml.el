(require 'yaml-mode)
(add-hook 'yaml-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))
(add-hook 'yaml-mode-hook 'rainbow-mode)
