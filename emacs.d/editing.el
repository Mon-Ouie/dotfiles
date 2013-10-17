;; Won't be the file where I will put 99% percent of my config, this time.

(setq-default fill-column 80)

(fset 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq require-final-newline t)

(setq-default indent-tabs-mode nil)

(prefer-coding-system       'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq x-select-enable-primary t)
(setq windmove-wrap-around t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'ido)
(ido-mode t)

;; thanks to all those programs that use ~ and not ~/.config, ~ ends up being way
;; too big…
(setq ido-max-directory-size 40000)

(require 'dired+)

(require 'smartparens-config)
(require 'my-smartparens-ruby)
(require 'smartparens-lua)
(require 'smartparens-latex)
(require 'smartparens-oz)
(require 'smartparens-octave)

(smartparens-global-mode t)
(show-smartparens-global-mode t)

(setq sp-show-pair-from-inside t)

(setq sp-base-key-bindings 'paredit)
(sp-use-paredit-bindings)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-e") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-»") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-M-»") 'sp-select-next-thing)

(global-undo-tree-mode)

(require 'linum)
(global-linum-mode t)

(require 'magit)

(require 'tramp)
(setq tramp-default-method "scp")

(defun find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(global-set-key (kbd "C-x F") 'find-file-as-root)

(when on-osx
  (setq x-select-enable-clipboard t)
  (setq mac-option-modifier nil))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'flymake)
(require 'flymake-extension)
(require 'flymake-point)
(require 'flymake-fringe-icons)
(require 'pos-tip)

(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(setq temporary-file-directory (config-dir "flymake.tmp"))

;; Flyspell
(require 'flyspell)
(require 'detect-language)
(setq-default ispell-program-name "aspell")
(ispell-change-dictionary "fr")

;; Annoying echo in inf-ruby
(setq-default comint-process-echoes nil)

;; ibuffer

(require 'ibuffer)

(setq ibuffer-modified-char ?✍)
(setq ibuffer-read-only-char ?✗)
(setq ibuffer-marked-char ?✓)
(setq ibuffer-deletion-char ?␡)

(setq ibuffer-show-empty-filter-groups nil)

(setq ibuffer-saved-filters
      '(("doc" ((or (mode . org-mode)
                    (mode . org-agenda-mode)
                    (mode . calendar)
                    (mode . poem-mode)
                    (mode . latex-mode)
                    (mode . text-mode)
                    (mode . markdown-mode))))
        ("mail" ((or (mode . gnus-group-mode)
                     (mode . gnus-article-mode)
                     (mode . mail-mode)
                     (mode . message-mode)
                     (mode . gnus-server-mode)
                     (mode . gnus-summary-mode))))
        ("code" ((or (mode . emacs-lisp-mode)
                     (mode . cperl-mode)
                     (mode . c-mode)
                     (mode . java-mode)
                     (mode . idl-mode)
                     (mode . lisp-mode)
                     (mode . io-mode)
                     (mode . haskell-mode)
                     (mode . smalltalk-mode)
                     (mode . ruby-mode))))))

;; grep

(setq ack-default-command "ack --nogroup ")
(defvar ack-history nil "History list for ack.")

(defun ack (command)
  (interactive
   (list
    (read-string "Run command: " ack-default-command ack-history
                 ack-default-command)))
  (compile command))

;; pcache, for gists
(require 'pcache)
(setq pcache-directory (config-dir ".var/pcache"))

(defvar scratch-major-modes
  '((elisp . emacs-lisp-mode)
    (ruby . ruby-mode)
    (haskell . haskell-mode)
    (lisp . lisp-mode)
    (clojure . clojure-mode)
    (c . c-mode)
    (octave . octave-mode)
    (oz . oz-mode)
    (org . org-mode)
    (text . text-mode)
    (latex . latex-mode))
  "An alist mapping language names to mode names for `new-scratch'")

(defun new-scratch (mode-name)
  "Creates a new scratch buffer using MODE-NAME, which should be a key in
`scratch-major-modes'."
  (interactive
   (list (intern
          (completing-read "Scratch language: "
                           (mapcar (lambda (x) (symbol-name (car x)))
                                   scratch-major-modes)))))
  (let ((mode (cdr (assoc mode-name scratch-major-modes))))
   (labels ((find-buffer-name (i)
                              (let ((name (format "*%s scratch <%d>*"
                                                  mode-name i)))
                                (if (not (get-buffer name)) name
                                  (find-buffer-name (1+ i))))))
     (switch-to-buffer (find-buffer-name 0))
     (funcall mode))))

(require 'helm-mpd)
