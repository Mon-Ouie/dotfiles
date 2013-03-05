;;; gist-paste.el — gisting code and storing the URL in the clipboard.

(require 'gist)

(defun gist-paste-callback (gist)
  (let ((url (oref gist :html-url)))
    (pop-mark)
    (cond
     ((fboundp 'x-set-selection) (x-set-selection 'primary url))))
  (gist-created-callback gist))

(defun gist-region-paste (begin end &optional private)
  (interactive "r\nP")
  (gist-region begin end private 'gist-paste-callback))

(defun gist-region-private-paste (begin end)
  (interactive "r\nP")
  (gist-region begin end t 'gist-paste-callback))

(defun gist-buffer-paste (&optional private)
  (gist-region (point-min) (point-max) private))

(defun gist-buffer-private-paste ()
  (gist-region-private (point-min) (point-max)))

(defun gist-region-or-buffer-paste (&optional private)
  (interactive "P")
  (condition-case nil
      (gist-region-paste (point) (mark) private)
    (mark-inactive (gist-buffer-paste private))))

(defun gist-region-or-buffer-private-paste ()
  (gist-region-or-buffer-private t))

(provide 'gist-paste)
