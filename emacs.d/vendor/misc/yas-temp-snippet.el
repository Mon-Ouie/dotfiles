;; For the record, I wrote this one myself!

(defvar yas/temp-snippet nil
  "Content of the temporary snippet")

(defun yas/save-temp-snippet ()
  "Saves the temporary snippet"
  (interactive)
  (setq yas/temp-snippet
        (buffer-substring (region-beginning) (region-end))))

(defun yas/expand-temp-snippet ()
  "Expands the temporary snippet"
  (interactive)
  (yas/expand-snippet yas/temp-snippet))

(defun yas/expand-or-save-temp-snippet ()
  "Expand the temporary snippet, or saves it if the mark is active"
  (interactive)
  (if mark-active (yas/save-temp-snippet) (yas/expand-temp-snippet)))

(provide 'yas-temp-snippet)
