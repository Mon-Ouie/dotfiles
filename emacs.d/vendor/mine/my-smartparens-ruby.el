;; my-smartparens-ruby.el — Extensions to smartparens-ruby.el

(require 'smartparens-ruby)

(defun sp-ruby-post-case-insert (id action context)
  (when (string= action 'insert)
    (cond
     ((equal id "case")
      (save-excursion (newline-and-indent))
      (insert " ")))))

(sp-with-modes '(ruby-mode)
  (sp-local-pair "case" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-post-case-insert)))

(provide 'my-smartparens-ruby)
