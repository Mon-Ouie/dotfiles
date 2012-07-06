(setq config-root "~/.emacs.d")

(defun config-file (file)
  (concat (file-name-as-directory config-root) file))

(defun config-dir (file)
  (file-name-as-directory (config-file file)))

(setq on-osx (featurep 'ns))

(if on-osx
    (setq custom-file (config-file "custom-osx.el"))
  (setq custom-file (config-file "custom-linux.el")))

(defun add-vendor-directory (dir)
  (dolist (elem (directory-files dir))
    (unless (string-match-p "^\\.\\.?$" elem) ;; Ignore . and ..
      (let ((filename (concat dir elem)))
        (if (file-directory-p filename)
            (push filename load-path))))))

(add-vendor-directory (config-dir "vendor"))
(add-vendor-directory (config-dir "el-get"))

(load-file (config-file "env.el"))
(load-file (config-file "packages.el"))
(load-file (config-file "look.el"))
(load-file (config-file "editing.el"))
(load-file (config-file "completion.el"))
(load-file (config-file "mail.el"))
(load-file (config-file "lang.el"))
(load-file (config-file "binding.el"))

(load-file custom-file)

(server-start) ; Start server now
