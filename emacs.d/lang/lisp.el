(setq inferior-lisp-program (executable-find "sbcl"))

(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-indentation slime-package-fu
               slime-fontifying-fu slime-highlight-edits))

(add-hook 'lisp-mode-hook
          #'(lambda ()
              (push 'ac-source-slime ac-sources)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (setq ac-sources
                    (append '(ac-source-emacs-lisp-features
                              ac-source-functions
                              ac-source-symbols
                              ac-source-variables)
                            ac-sources))))

;; Clojure
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook clojure-mode-hook))
  (add-hook hook 'turn-on-auto-fill)
  (add-hook hook 'rainbow-mode))

(require 'ielm) ;; REPL for elisp
