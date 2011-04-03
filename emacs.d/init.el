(setq config-root "~/.emacs.d")

(defun config-file (file)
  (concat (file-name-as-directory config-root) file))

(defun config-dir (file)
  (file-name-as-directory (config-file file)))

(setq on-osx (featurep 'ns))

(if on-osx
    (setq custom-file (config-file "custom-osx.el"))
  (setq custom-file (config-file "custom-linux.el")))

(dolist (elem (directory-files (config-dir "vendor")))
  (unless (string-match-p "^\\.\\.?$" elem) ;; Ignore . and ..
    (let ((filename (concat (config-dir "vendor") elem)))
      (if (file-directory-p filename)
          (push filename load-path)))))

(load-file (config-file "env.el"))
(load-file (config-file "packages.el"))
(load-file (config-file "look.el"))
(load-file (config-file "editing.el"))
(load-file (config-file "completion.el"))
(load-file (config-file "lang.el"))
(load-file (config-file "binding.el"))

(load-file custom-file)
