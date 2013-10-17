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
(add-hook 'nrepl-mode-hook 'ac-nrepl-compliment-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-compliment-setup)

(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-compliment-popup-doc)

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook clojure-mode-hook))
  (add-hook hook 'turn-on-auto-fill)
  (add-hook hook 'rainbow-mode))

(require 'ielm) ;; REPL for elisp
