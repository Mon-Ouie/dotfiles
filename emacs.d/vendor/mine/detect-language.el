;;; detect-language.el — Detect a language to select a dictionnary for ispell.

(require 'cl)
(require 'ispell)
(require 'ispell-multi)

(defvar detect-language-languages (make-hash-table :test 'equal)
  "Map that matches a language name to a word-frequency table.")

(defvar detect-language-totals (make-hash-table :test 'equal)
  "Map that matches a language name to the total amount of words seen it.")

(defvar detect-language-punctuation
  '(?\( ?\) ?\[ ?\] ?, ?. ?{ ?} ?? ?! ?\; ?: ?\" ?\' ?« ?»
        ?… ?“ ?” ?/ ?— ?− ?_ ?* ?+)
  "Characters that will be stripped off words when retrieving them.")

(defvar detect-language-word-regexp "[ \f\t\n\r\v -—+|=*−]+"
  "Regular expression to split a text into words.")

(defvar detect-language-db-file "~/.emacs.d/.detect_language.db"
  "File to store the word frequency database into.")

(defun detect-language-clear ()
  "Completely clears the word database for detecting languages."
  (interactive)
  (clrhash detect-language-languages)
  (clrhash detect-language-totals))

(defun detect-language-hash-to-alist (hash)
  "Converts a hash table to an association list, for serialization purposes."
  (let ((alist nil))
    (maphash #'(lambda (k v)
                 (setq
                  alist
                  (acons k
                         (cond
                          ((hash-table-p v) (detect-language-hash-to-alist v))
                          (t v)) alist)))
             hash)
    alist))

(defun detect-language-load-alist (alist)
  "Merges the current databsase with the content of an association list."
  (mapc #'(lambda (assoc)
            (let ((table (make-hash-table :test 'equal))
                  (lang  (car assoc))
                  (freq  (cdr assoc)))
              (puthash lang table detect-language-languages)
              (puthash lang 0 detect-language-totals)
              (mapc #'(lambda (assoc)
                        (puthash lang (+ (cdr assoc)
                                         (gethash lang detect-language-totals))
                                 detect-language-totals)
                        (puthash (car assoc) (cdr assoc) table))
                    freq)))
        alist))

(defun detect-language-save-db ()
  "Saves the word frequency database to disk."
  (interactive)
  (with-temp-file detect-language-db-file
    (print (detect-language-hash-to-alist detect-language-languages)
           (current-buffer))))

(defun detect-language-load-db ()
  "Loads the word frequency database from disk."
  (interactive)
  (let ((alist (read (with-temp-buffer
                       (insert-file-contents detect-language-db-file)
                       (buffer-string)))))
    (detect-language-clear)
    (detect-language-load-alist alist)))

(when (file-exists-p detect-language-db-file)
  (detect-language-load-db))

(defun detect-language-add-lang (name)
  "Creates an empty table for a new language."
  (puthash name 0 detect-language-totals)
  (puthash name (make-hash-table :test 'equal) detect-language-languages))

(defun detect-language-known-languages ()
  "Returns a list containing all languages that have a frequency table."
  (let ((langs nil))
    (maphash #'(lambda (lang table) (push lang langs))
             detect-language-languages)
    langs))

(defun detect-language-table-for (name)
  "Returns the frequency table for a given language, creating it if needed."
  (or (gethash name detect-language-languages)
      (detect-language-add-lang name)))

(defun detect-language-strip-punctuation (word)
  "Removes punctuation, etc. from a word"
  (remove-if #'(lambda (c) (find c detect-language-punctuation)) word))

(defun detect-language-get-words (string)
  "Maps a string to a list of words."
  (remove ""
          (mapcar #'(lambda (w) (downcase (detect-language-strip-punctuation w)))
                  (split-string string detect-language-word-regexp))))

(defun detect-language-mark-word (lang word)
  "Marks a word as being part of a language, increasing its score."
  (let ((table (detect-language-table-for lang)))
    (puthash lang (1+ (gethash lang detect-language-totals)) detect-language-totals)
    (puthash word (1+ (gethash word table 0)) table)))

(defun detect-language-mark-string (lang string)
  "Marks every word of a string as being part of a language."
  (mapc #'(lambda (word) (detect-language-mark-word lang word))
        (detect-language-get-words string)))

(defun detect-language-mark-region (lang start end)
  "Marks the current region (or buffer if no region is selected) as being
written in a specific language, also changing the ispell dictionnary."
  (interactive
   (list
    (let ((dict (or ispell-local-dictionary ispell-dictionary)))
      (read-string (format "Language (%s): " dict) nil nil dict))
    (if (use-region-p) (region-beginning) (point-min))
    (if (use-region-p) (region-end) (point-max))))

  (detect-language-mark-string lang (buffer-substring start end))
  (detect-language-save-db) ; maybe we shouldn't do that so often?

  (ispell-change-dictionary lang))

(defun detect-language-count-in (lang word)
  "Returns the amount of times a word has been seen in a certain language."
  (let ((table (detect-language-table-for lang)))
    (gethash word table 0)))

(defun detect-language-score-for (lang words)
  "Returns the score a string gets for a certain language. Higher is better.

The algorithm used for this is rather naïve: just sum the amount of times a word
has been detected as being written in a certain language, and divide that but
the total amonut of words that have been seen."
  (let ((total-count (gethash lang detect-language-totals)))
    (if (zerop total-count) 0
      (/ (reduce #'+ words :key #'(lambda (w) (detect-language-count-in lang w)))
         (float total-count)))))

(defun detect-language-score-map (string)
  "Builds a hash table showing the score for a string in every language."
  (let ((table (make-hash-table :test 'equal))
        (words (detect-language-get-words string)))
      (dolist (lang (detect-language-known-languages))
        (puthash lang (detect-language-score-for lang words) table))
      table))

(defun detect-language-max-by (func list)
  "Returns the value in a list for which a certain function returns the greatest
value."
  (let ((acc-val (funcall func (first list))))
    (reduce #'(lambda (acc el)
                (let ((val (funcall func el)))
                  (if (<= val acc-val) acc
                    (setq acc-val val)
                    el)))
            (rest list) :initial-value (first list))))

(defun detect-language-for (string)
  "Returns the language that a string is most likely to be written in, or nil
if the score is null for all known languages."
  (let ((scores (detect-language-score-map string))
        (langs  (detect-language-known-languages)))
    (let ((best-lang
           (detect-language-max-by #'(lambda (l) (gethash l scores)) langs)))
      (if (zerop (gethash best-lang scores)) nil
        best-lang))))

(defun detect-language ()
  "Attempts to detect the language of the current buffer and change ispell
dictionnary appropirately."
  (interactive)
  (let ((lang (detect-language-for (buffer-string))))
    (when lang (ispell-change-dictionary lang))))

(defun detect-language-for-paragraph ()
  "Detects the language of a given paragraph."
  (let ((lang (detect-language-for (thing-at-point 'paragraph)))
        (paragraph (bounds-of-thing-at-point 'paragraph)))
    (add-text-properties (car paragraph) (cdr paragraph)
                         (list 'ispell-multi-lang lang
                               'rear-nonsticky t))))

(defun detect-language-per-paragraph ()
  "Enables detection of the language on a per-paragraph basis."
  (interactive)
  (setq flyspell-generic-check-word-predicate 'ispell-multi-verify)
  (setq ispell-multi-nil-callback 'detect-language-for-paragraph))

(provide 'detect-language)
