(require 'cc-mode)

(add-hook 'c-mode-common-hook 'turn-on-auto-fill)

(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))
(require 'column-marker)
(add-hook 'c-mode-common-hook '(lambda ()
                                 (flymake-mode t)
                                 (column-marker-2 80)))


(setq-default c-default-style "k&r")
(setq-default c-basic-offset 2)

(c-set-offset 'inextern-lang 0)

;; Auto-completion for ObjC
(setq ac-modes (append '(objc-mode) ac-modes))

(require 'auto-complete-clang)
(add-hook 'c-mode-common-hook
          '(lambda()
             (setq ac-sources (append '(ac-source-clang) ac-sources))))

(setq ac-clang-flags '("-I." "-I.." "-I/usr/include"))

(defun objc-wrap-brackets (&optional count)
  (interactive "*p")
  (backward-up-list count)
  (insert "[")
  (forward-sexp +1)
  (save-excursion (insert "]"))
  (just-one-space))

(define-key objc-mode-map (kbd "C-c a") 'objc-wrap-brackets)
(define-key objc-mode-map (kbd "C-c b") 'xcode/build-compile)

(require 'c-eldoc)
(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../")
(add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)

;; Flymake

(require 'flymake)

(defun flymake-clang-init (compiler)
  (let* ((temp-file
          (flymake-init-create-temp-buffer-copy
           'flymake-create-temp-intemp))
         (local-file
          (file-relative-name
           temp-file
           (file-name-directory buffer-file-name)))
         (local-directory
          (file-name-directory buffer-file-name)))
    (list compiler
          (append (list "-fsyntax-only" ;; Don't compile my code

                        ;; Make clang output look like GCC (except for the message :)
                        "-fno-color-diagnostics"
                        "-fno-caret-diagnostics"
                        "-fno-show-column"

                        ;; Be mean
                        "-Wall"
                        "-Wextra"
                        "-pedantic"
                        "-Werror-implicit-function-declaration"

                        ;; But let me do this, still.
                        "-Wno-unused-parameter"

                        ;; Load path
                        (concat "-I" local-directory)
                        (concat "-I" local-directory "/..")
                        "-I/usr/include/freetype2"
                        "-I/usr/include/ruby-1.9.1"
                        "-I/usr/include/ruby-1.9.1/x86_64-linux"

                        ;; Input file
                        local-file)
                  flymake-clang-extra-c-flags))))

(defvar flymake-clang-extra-c-flags ()
  "C flags")

(defun flymake-c-init ()
  (flymake-clang-init "clang"))

(defun flymake-cpp-init ()
  (flymake-clang-init "clang++"))

(push '(".+\\.c$" flymake-c-init) flymake-allowed-file-name-masks)
(push '(".+\\.h$" flymake-c-init) flymake-allowed-file-name-masks)
(push '(".+\\.m$" flymake-c-init) flymake-allowed-file-name-masks)

(push '(".+\\.cpp$" flymake-cpp-init) flymake-allowed-file-name-masks)
(push '(".+\\.hpp$" flymake-cpp-init) flymake-allowed-file-name-masks)
