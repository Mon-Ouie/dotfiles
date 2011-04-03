(require 'ruby-block)
(setq ruby-block-highlight-toggle t)

(add-hook 'ruby-mode-hook '(lambda ()
                             (setq tab-width 2)
                             (ruby-block-mode t)))

(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc" . ruby-mode))

(defun ruby-run-buffer()
  (interactive)
  (save-current-buffer)
  (compile (concat (executable-find "ruby") " " (buffer-file-name))))

(define-key ruby-mode-map (kbd "C-c r") 'ruby-run-buffer)
(define-key ruby-mode-map (kbd "C-c d") 'yari)

(define-key ruby-mode-map (kbd "s-r") 'ruby-run-buffer)
(define-key ruby-mode-map (kbd "s-d") 'yari)

;; Shamelessly stolen code from dominikh to colorize documentation.

(defface font-lock-yard-param-name-face
  '((t (:weight bold)))
  "Font Lock mode face used to highlight YARD param names."
  :group 'font-lock-faces)

;; @option opts [String] :subject The subject
;; @option opts [String] :body ('') The email's body
(defvar font-lock-yard-param-name-face 'font-lock-yard-param-name-face
  "Face name to use for YARD param names.")

(defface font-lock-yard-option-default-value-face
  '((t (:slant italic)))
  "Font Lock mode face used to highlight YARD option default values."
  :group 'font-lock-faces)

;; @option opts [String] :subject The subject
;; @option opts [String] :body ('') The email's body
(defvar font-lock-yard-option-default-value-face 'font-lock-yard-option-default-value-face
  "Face name to use for YARD option default values.")

(defface font-lock-yard-face
  '()
  "Font Lock mode face used to highlight YARD lines."
  :group 'font-lock-faces)

;; @option opts [String] :subject The subject
;; @option opts [String] :body ('') The email's body
(defvar font-lock-yard-face 'font-lock-yard-face
  "Face name to use for YARD lines")

(font-lock-add-keywords 'ruby-mode
                        '(
                          ("^[ ]*# *\\(@.+$\\)" 1 font-lock-yard-face prepend)
                          ;; tags
                          ("^[ ]*# *\\(@[a-z]+\\)" 1 font-lock-keyword-face prepend)
                          ;; types ("@tag [Type] ...")
                          ("^[ ]*# *@[a-z]+ \\(\\[.+?\\]\\)" 1 font-lock-type-face prepend)
                          ;; name of params ("@tag [Type] argname Description")
                          ("^[ ]*# *@\\(param\\|yieldparam\\|attr\\|attr_reader\\|attr_writer\\) +\\[.+?\\] +\\([^ ]+\\)" 2 font-lock-yard-param-name-face prepend)
                          ;; references
                          ("^[ ]*# *@see \\(.+\\)" 1 font-lock-doc-face prepend)
                          ;; references
                          ("^[ ]*# *@[a-z]+ \\((see .+)\\)" 1 font-lock-doc-face prepend)
                          ;; @option hash names
                          ("^[ ]*# *@option \\([^ ]+\\)" 1 font-lock-yard-param-name-face prepend)
                          ;; @option param types
                          ("^[ ]*# *@option [^ ]+ \\(\\[.+?\\]\\)" 1 font-lock-type-face prepend)
                          ;; @option param names
                          ("^[ ]*# *@option [^ ]+ \\[.+?\\] \\([^ ]+\\)" 1 font-lock-yard-param-name-face prepend)
                          ;; @option default values
                          ("^[ ]*# *@option [^ ]+ \\[.+?\\] [^ ]+ \\((.+?)\\)" 1 font-lock-yard-option-default-value-face prepend)
                          ("^[ ]*# *@overload \\(.+?\\)(" 1 font-lock-function-name-face prepend)
                          ("^[ ]*# *" (0 nil) ("{.+?\\(}\\|$\\)" nil nil (0 font-lock-doc-face prepend)))
                          ("^[ ]*# *" (0 nil) ("[^{]+?}" nil nil (0 font-lock-doc-face prepend)))
                          ))

;; Flymake

(require 'flymake)

(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-intemp))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

;; inf-ruby
(setq-default inf-ruby-prompt-pattern ">> *")
(setq-default inf-ruby-first-prompt-pattern ">> *")
