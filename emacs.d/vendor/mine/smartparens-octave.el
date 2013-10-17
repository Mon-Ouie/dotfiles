;; smartparens-octave.el — Smartparens support or octave

(require 'smartparens)

(defun sp-octave-in-string-or-word-p (id action context)
  (or (sp-in-string-p id action context)
      (and (looking-back id)
           (not (looking-back (sp--strict-regexp-quote id))))))

(defun sp-octave-pre-handler (id action context)
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

(defun sp-octave-post-insert-newline (id action context)
  (when (string= action 'insert)
    (save-excursion (newline-and-indent))
    (insert " ")))

(sp-with-modes '(octave-mode)
  (sp-local-pair "for" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-octave-in-string-or-word-p)
                 :pre-handlers '(sp-octave-pre-handler)
                 :post-handlers '(sp-octave-post-insert-newline))
  (sp-local-pair "function" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-octave-in-string-or-word-p)
                 :pre-handlers '(sp-octave-pre-handler)
                 :post-handlers '(sp-ruby-post-insert-newline))
  (sp-local-pair "if" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-octave-in-string-or-word-p)
                 :pre-handlers '(sp-octave-pre-handler)
                 :post-handlers '(sp-ruby-post-insert-newline))
  (sp-local-pair "while" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-octave-in-string-or-word-p)
                 :pre-handlers '(sp-octave-pre-handler)
                 :post-handlers '(sp-ruby-post-insert-newline)))

(provide 'smartparens-octave)
