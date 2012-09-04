(require 'message)
(require 'org)

(define-key message-mode-map (kbd "C-c o") 'browse-url-at-point)

(add-hook 'message-mode-hook 'turn-on-orgstruct++)
(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook '(lambda () (footnote-mode t)))

(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'message-mode-hook 'detect-language)
