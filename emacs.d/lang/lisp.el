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

(add-hook 'lisp-mode-hook 'turn-on-auto-fill)

(add-hook 'lisp-mode-hook
          #'(lambda ()
              (push 'ac-source-slime ac-sources)))

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (setq ac-sources
                    (append '(ac-source-emacs-lisp-features
                              ac-source-functions
                              ac-source-symbols
                              ac-source-variables)
                            ac-sources))))
