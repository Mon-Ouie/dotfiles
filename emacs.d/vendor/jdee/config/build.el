;; Create autoloads and build the lisp source code.  The paths are substituted
;; by the ant build.

(defun jde-make-autoloads (dir libname)
  "Generate the jde-autoloads.el for all elisp source files in DIR."
  (let* ((filename (format "%s.el" libname))
	 (filename-long (expand-file-name filename dir))
	 (buf (find-file-noselect filename-long))
	 files)
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (dolist (file (remove nil
			    (mapcar #'(lambda (file)
					(unless
					    (or (string= filename file)
						(string-match "^\\.#" file))
					  file))
					(directory-files dir nil
							 "\\.el$"))))
	(generate-file-autoloads file))
      ;; users can now use (require 'jde); which in turn, loads the autoloads
      (insert (format "\n(provide '%s)\n" libname))
      (save-buffer buf)
      (eval-buffer buf)
      buf)))

(defun jde-make-autoloads-and-compile (dir lisp-src-dir elib-dir cedet-dir
					   cedet-paths autoload-libname)
  "Create autoloads and compile lisp code in DIR.
LISP-SRC-DIR is the base directory for all third party lisp code use to
compile.

ELIB-DIR is the directory to elib-1.0 \(no longer needed for Emacs 23+).

CEDET-DIR is the cedet lisp code base directory (see PATHS).

CEDET-PATHS are sub directories under CEDET-DIR we use to compile.

AUTOLOAD-LIBNAME the name of the generated autoload file."
  (dolist (path (list dir lisp-src-dir elib-dir cedet-dir))
    (if (and (> (length path) 0) (not (file-directory-p path)))
	(error "Doesn't exist or not a directory: %s" path)))
  (let ((autoload-buf (jde-make-autoloads dir autoload-libname)))
    (if (> (length elib-dir) 0)
	(add-to-list 'load-path elib-dir t))
    (when (and cedet-paths (> (length cedet-dir) 0))
      (dolist (path cedet-paths)
	(add-to-list 'load-path (expand-file-name path cedet-dir) t)))
    (add-to-list 'load-path lisp-src-dir t)
    (eval-buffer autoload-buf)
    (message "load path: %s" (mapconcat #'identity load-path ":"))
    (byte-recompile-directory dir 0)))



(require 'autoload)
(jde-make-autoloads-and-compile (expand-file-name "@{build.lisp.dir}")
				"@{src.lisp.dir}"
				"@{elib.dir}"
				"@{cedet.dir}"
				'("common"
				  "eieio"
				  "semantic"
				  "semantic/bovine"
				  "speedbar"
				  )
				"@{build.lisp.autoload.libname}")
