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

(require 'textmate)
(textmate-mode t)

(require 'autopair)
(autopair-global-mode t)

(require 'linum)
(global-linum-mode t)

(add-hook 'fundamental-mode-hook 'auto-fill-mode)

(require 'magit)

(require 'tramp)
(setq tramp-default-method "scp")

(when on-osx
  (setq x-select-enable-clipboard t)
  (setq mac-option-modifier nil))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
(setq-default ispell-program-name "aspell")
(ispell-change-dictionary "fr")

;; Annoying echo in inf-ruby
(setq-default comint-process-echoes t)
