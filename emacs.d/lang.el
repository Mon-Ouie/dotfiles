(dolist (elem (directory-files (config-dir "lang")))
  (when (string-match-p "\\.el$" elem)
    (let ((filename (concat (config-dir "lang") elem)))
      (load-file filename))))
