(require 'elfeed)

(global-set-key (kbd "<f12>") 'elfeed)

(defvar feeds-data (read (with-temp-buffer
                           (insert-file-contents (config-file "feeds.el"))
                           (buffer-string))))

(dolist (el feeds-data)
  (destructuring-bind (e-url e-title e-tags e-font) el
    (lexical-let ((url e-url) (title e-title) (tags e-tags) (font e-font))
      (add-to-list 'elfeed-feeds url)
      (add-hook 'elfeed-new-entry-hook
                (lambda (entry)
                  (when (equal (elfeed-feed-url (elfeed-entry-feed entry))
                               url)
                    (apply 'elfeed-tag entry tags)))))))

(defadvice elfeed-search-update (before feed-name activate)
  (dolist (el feeds-data)
    (destructuring-bind (url title tags font) el
      (when title
        (let ((feed (elfeed-db-get-feed url)))
          (setf (elfeed-feed-title feed) title))))))

(defun elfeed-search-print (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-entry-title entry) ""))
         (feed (elfeed-entry-feed entry))
         (feed-info (assoc (elfeed-feed-url feed) feeds-data))
         (feed-font (nth 2 (cdr feed-info)))
         (title-faces (list (or feed-font 'elfeed-search-title-face)))
         (feed-title (if feed (elfeed-feed-title feed)))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    (when (elfeed-tagged-p 'unread entry)
      (push 'bold title-faces))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces) " ")
    (when feed-title
      (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags
      (insert "(" tags-str ")"))))

(defun elfeed-search-format-date (date)
  (let ((s (format-time-string "%A %e %B %Y, %H:%M" (seconds-to-time date))))
    (format "%40s" s)))

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
                       (concat "youtube-dl --prefer-insecure -g '"
                               url
                               "' 2>/dev/null"))))
      (elfeed-search-update-entry entry))))

(define-key elfeed-search-mode-map (kbd "o") 'elfeed-search-browse-url)
(define-key elfeed-search-mode-map (kbd "w") 'elfeed-current-entry-watch)
