;;; smartparens-oz.el — smartparens support for oz-mode

(defun sp-oz-in-string-or-word-p (id action context)
  (or (sp-in-string-p id action context)
      (and (looking-back id)
           (not (looking-back (sp--strict-regexp-quote id))))))

(defun sp-oz-post-keyword-insert (id action context)
  (when (string= action 'insert)
    (cond
     ((equal id "if")
      (insert " then ")
      (save-excursion (newline-and-indent))
      (backward-char 5))
     ((equal id "case")
      (save-excursion
        (newline-and-indent)
        (insert "of ")
        (newline-and-indent))
      (insert " "))
     ((equal id "local")
      (insert " in")
      (save-excursion (newline-and-indent))
      (backward-char 3))
     ((equal id "for")
      (insert " do")
      (save-excursion (newline-and-indent))
      (backward-char 3))
     ((member id '("class" "meth" "or" "dis" "cond" "not"))
      (save-excursion (newline-and-indent))
      (insert " "))
     ((member id '("fun" "proc" "thread" "raise" "functor" "lock"))
      (insert " ")
      (backward-char 1)))))

(defun sp-oz-pre-handler (id action context)
  (when (equal action 'slurp-backward)
    (save-excursion
      (sp-forward-sexp)
      (delete-indentation -1))
    (save-excursion
      (newline))
    (just-one-space))

  (when (equal action 'barf-backward)
    (save-excursion
      (newline))
    (save-excursion
      (sp-backward-sexp)
      (delete-indentation))
    (just-one-space))

  (when (equal action 'slurp-forward)
    (save-excursion
      (sp-backward-sexp)
      (delete-indentation))
    (newline))

  (when (equal action 'barf-forward)
    (save-excursion
      (sp-forward-sexp)
      (delete-indentation -1))
    (newline)))

(sp-with-modes '(oz-mode)
  (sp-local-pair "if" "end"
                 :when '(("SPC"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "case" "end"
                 :when '(("SPC"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "thread" "end"
                 :when '(("SPC"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "raise" "end"
                 :when '(("SPC"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "fun" "end"
                 :when '(("SPC"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "proc" "end"
                 :when '(("SPC"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "meth" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "local" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "functor" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "for" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "class" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "lock" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "or" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "dis" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "cond" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "not" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-oz-in-string-or-word-p)
                 :pre-handlers '(sp-oz-pre-handler)
                 :post-handlers '(sp-oz-post-keyword-insert))
  (sp-local-pair "<<" ">>"
                 :post-handlers '(sp-oz-post-keyword-insert)))

(provide 'smartparens-oz)
