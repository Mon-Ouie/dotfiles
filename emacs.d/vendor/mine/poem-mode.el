(require 'cl)

(defgroup poem nil
  "Major mode for editing poems."
  :prefix "poem-"
  :group 'wp)

(defgroup poem-faces nil
  "Faces used in poem-mode."
  :group 'poem-mode
  :group 'faces)

(defface poem-title-face
  '((t (:inherit font-lock-function-name-face
                 :weight bold
                 :height 300)))
  "Face for the title of the poem."
  :group 'poem-faces)

(defface poem-first-verse-char-face
  '((t (:inherit default
                 :weight bold)))
  "Face for the first character of each verse."
  :group 'poem-faces)

(defvar poem-title-face 'poem-title-face)
(defvar poem-first-verse-char-face 'poem-first-verse-char-face)

(defun poem-title-matcher (limit)
  (let ((end-of-line (position ?\n (buffer-string))))
    (when (and end-of-line (< (+ 2 end-of-line) limit) (< (point) (+ 2 end-of-line)))
      (store-match-data (list (point) (+ 2 end-of-line)))
      t)))

(defvar poem-font-lock-keywords
  '((poem-title-matcher . poem-title-face)
    ("^." . poem-first-verse-char-face)))

(define-derived-mode poem-mode text-mode
  "Poem"
  (set (make-local-variable 'font-lock-defaults)
      '(poem-font-lock-keywords)))

(define-key poem-mode-map "'" "’")

(add-to-list 'auto-mode-alist '("\\.poem\\'" . poem-mode))

(provide 'poem-mode)
