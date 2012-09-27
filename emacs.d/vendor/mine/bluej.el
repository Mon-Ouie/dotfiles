;;; bluej.el — Copies part of BlueJ's submitter feature.

(require 'cl)
(require 'em-glob)

;;; I could parse the whole submission.defs structure. But all I really need is
;;; a susbet of it.

(defstruct bluej-scheme
  transport-url globs jar)

(defun bluej-retrieve (field code &optional delimiter)
  (or (and (string-match (concat field "[ ]*=[ ]*\\(.+\\)"
                                 (or delimiter ";")) code)
           (match-string 1 code))
      ""))

(defun bluej-retrieve-list (field code)
  (remove "" (split-string (bluej-retrieve field code) "[, ]")))

(defun bluej-get-scheme ()
  (let ((code (with-temp-buffer
                (insert-file-contents "submission.defs")
                (buffer-string)))
        (scheme (make-bluej-scheme)))
    (setf (bluej-scheme-transport-url scheme)
          (bluej-retrieve ".transport" code))
    (setf (bluej-scheme-jar scheme)
          (bluej-retrieve ".file.jar" code))
    (setf (bluej-scheme-globs scheme)
          (bluej-retrieve-list ".file.include" code))
    scheme))

(defun bluej-get-email ()
  (bluej-retrieve
   "extensions.org.bluej.extensions.submitter.Submitter.settings.useraddr"
   (with-temp-buffer
     (insert-file-contents "~/.bluej/bluej.properties")
     (buffer-string))
   "\n"))

(defun bluej-archive-dirname ()
  (concat (car (last (remove "" (split-string default-directory "/"))))
          "/"))

(defun bluej-files-glob (path)
  (directory-files (or (file-name-directory path) ".")
                   nil
                   (eshell-glob-regexp (file-name-nondirectory path))))

(defun bluej-archive-files (scheme)
  (map 'list
       (lambda (f) (concat (bluej-archive-dirname) f))
       (reduce #'append (map 'list #'bluej-files-glob (bluej-scheme-globs scheme))
               :initial-value nil)))

(defun bluej-make-archive (scheme)
  (let ((tmp-name (expand-file-name
                   (concat temporary-file-directory (bluej-scheme-jar scheme)))))
    (let ((archive-dir (bluej-archive-dirname))
          (files (bluej-archive-files scheme)))
      (cd "..")
      (apply #'call-process "7z" nil "*bluej-archive*" nil
             `("a" ,tmp-name ,@files))
      (cd archive-dir))
    tmp-name))

(defun bluej-expand-args (args)
  (replace-regexp-in-string "<address>" (bluej-get-email) args))

(defun bluej-split-url (scheme)
  (let ((split (split-string (bluej-scheme-transport-url scheme)
                             "?")))
    (cons (nth 0 split) (nth 1 split))))

(defun bluej-get-url (scheme)
  (car (bluej-split-url scheme)))

(defun bluej-get-arguments (scheme)
  (url-parse-query-string
   (bluej-expand-args
    (cdr (bluej-split-url scheme)))))

(defun bluej-format-arguments (args)
  (loop for arg in args
        append
        (list "-F" (concat (nth 0 arg) "=" (nth 1 arg)))))

(defun bluej-send-archive (scheme)
  (let ((archive (bluej-make-archive scheme)))
    (apply #'call-process "curl" nil "*bluej-send*" t
           `(,@(bluej-format-arguments (bluej-get-arguments scheme))
             "-F" ,(concat "file1=@" archive)
             ,(bluej-get-url scheme)))))

(defun bluej-submit ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to send this project?")
    (bluej-send-archive (bluej-get-scheme))))

(provide 'bluej)
