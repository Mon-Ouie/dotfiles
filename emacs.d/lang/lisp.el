(require 'slime)
(require 'slime-autoloads)

(setq inferior-lisp-program "/usr/bin/clisp")

(slime-setup)

(add-hook 'comint-mode-hook
          #'(lambda ()
              (setq autopair-dont-activate t)
              (autopair-mode -1)))
(add-hook 'sldb-mode-hook
          #'(lambda ()
              (setq autopair-dont-activate t)
              (autopair-mode -1)))

(add-hook 'lisp-mode #'(lambda () (auto-fill-mode t)))
