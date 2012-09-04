(defun texcount-setup ()
  (defun latex-word-count ()
    (interactive)
    (let*
        ((this-file (buffer-file-name))
         (enc-str (symbol-name buffer-file-coding-system))
         (enc-opt
          (cond
           ((string-match "utf-8" enc-str) "-utf8")
           ((string-match "latin" enc-str) "-latin1")
            ("-encoding=guess")))
         (word-count
          (with-output-to-string
            (with-current-buffer standard-output
              (call-process "texcount" nil t nil "-0" enc-opt this-file)))))
      (message word-count)))

  (define-key LaTeX-mode-map "\C-cw" 'latex-word-count))

(autoload 'flyspell-babel-setup "flyspell-babel")

(dolist (hook '(LaTeX-mode-hook latex-mode-hook))
  (add-hook hook
            (lambda()
              (flyspell-mode t)
              (auto-fill-mode t)
              (texcount-setup)))
  (add-hook hook 'flyspell-babel-setup))
