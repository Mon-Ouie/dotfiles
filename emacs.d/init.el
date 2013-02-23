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
  (when (and (file-exists-p dir)
	     (file-directory-p dir))
    (dolist (elem (directory-files dir))
      (unless (or (equal "." elem) (equal ".." elem))
	(let ((filename (concat dir elem)))
	  (if (file-directory-p filename)
	      (add-to-list 'load-path filename)))))))

(add-vendor-directory (config-dir "vendor"))
(add-vendor-directory (config-dir "el-get"))
(add-vendor-directory "/usr/local/share/emacs/site-lisp/")

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
