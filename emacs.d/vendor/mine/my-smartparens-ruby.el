;; my-smartparens-ruby.el — Extensions to smartparens-ruby.el

(require 'smartparens-ruby)

(defun sp-ruby-post-insert-newline (id action context)
  (when (string= action 'insert)
    (save-excursion (newline-and-indent))
    (insert " ")))

(sp-with-modes '(ruby-mode)
  (sp-local-pair "case" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-post-insert-newline))
  (sp-local-pair "until" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-post-insert-newline)))

(provide 'my-smartparens-ruby)
