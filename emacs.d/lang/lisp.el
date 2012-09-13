(setq inferior-lisp-program (executable-find "sbcl"))

(require 'slime-autoloads)
(slime-setup '(slime-fancy))

(add-hook 'sldb-mode-hook
          #'(lambda ()
              (setq autopair-dont-activate t)
              (autopair-mode -1)))

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

(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-auto-fill)
  (add-hook hook
            #'(lambda ()
                (autopair-mode -1)
                (paredit-mode 1))))

(require 'ielm) ;; REPL for elisp
