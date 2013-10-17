(require 'elfeed)

(global-set-key (kbd "<f12>") 'elfeed)

(dolist (el (read (with-temp-buffer
                    (insert-file-contents (config-file "feeds.el"))
                    (buffer-string))))
  (destructuring-bind (e-url title e-tags) el
    (lexical-let ((url e-url) (tags e-tags))
      (add-to-list 'elfeed-feeds url)
      (add-hook 'elfeed-new-entry-hook
                (lambda (entry)
                  (when (equal (elfeed-feed-url (elfeed-entry-feed entry))
                               url)
                    (apply 'elfeed-tag entry tags)))))))

(defun elfeed-current-entry-watch ()
  "Starts mplayer to watch the current entry, using youtube-dl to get a link
to it."
  (interactive)
  (let* ((entry (elfeed-search-selected :ignore-region))
         (url (and entry (elfeed-entry-link entry))))
    (when url
      (elfeed-untag entry 'unread)
      (start-process (concat "mplayer: " (elfeed-entry-title entry)) nil
                     "mplayer"
                     (replace-regexp-in-string
                      "\\s-+" ""
                      (shell-command-to-string
                       (concat "youtube-dl -g '"
                               url
                               "' 2>/dev/null")))))))

(define-key elfeed-search-mode-map (kbd "o") 'elfeed-search-browse-url)
(define-key elfeed-search-mode-map (kbd "w") 'elfeed-current-entry-watch)
