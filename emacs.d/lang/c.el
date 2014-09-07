(require 'cc-mode)

(defvar c-include-dirs nil)
(make-variable-buffer-local 'c-include-dirs)

(require 'auto-complete-clang-async)

(defun set-c-include-dirs (include-dirs commands other-flags)
  (require 'ffap)  (make-variable-buffer-local 'ffap-c-path)
  (require 'eldoc) (make-variable-buffer-local 'c-eldoc-includes)
  (let ((all-flags (append include-dirs other-flags)))
    (setq c-include-dirs include-dirs)
    (dolist (command commands)
      (let* ((flags (split-string (shell-command-to-string command)))
             (includes (remove-if-not
                        (lambda (e) (string-match "\\`-I" e))
                        flags)))
        (setq c-include-dirs (append c-include-dirs includes))
        (setq all-flags (append all-flags flags))))

    (setq ffap-c-path (mapcar (lambda (s) (subseq s 2)) c-include-dirs))
    (setq ac-clang-cflags all-flags)
    (setq c-eldoc-includes (mapconcat #'identity all-flags " "))))

(setq c-default-include-dirs
  '("-I."
    "-I.."
    "-I../src"
    "-I../include"
    "-I/usr/include"
    "-I/usr/include/c++/4.7.1"
    "-I/usr/include/c++/4.7.1/x86_64-unknown-linux-gnu/"
    "-I/usr/include/c++/4.7.1/x86_64-unknown-linux-gnu/32/"
    "-I/usr/include/ruby-1.9.1"
    "-I/usr/include/ruby-1.9.1/x86_64-linux"
    "-I/usr/include/ruby-2.0.0"
    "-I/usr/include/ruby-2.0.0/x86_64-linux"
    "-I/usr/include/ruby-2.1.0"
    "-I/usr/include/ruby-2.1.0/x86_64-linux"
    "-I/usr/include/freetype2"
    "-I/usr/include/Qt"
    "-I/usr/include/Qt3Support"
    "-I/usr/include/QtCore"
    "-I/usr/include/QtCrypto"
    "-I/usr/include/QtDBus"
    "-I/usr/include/QtDeclarative"
    "-I/usr/include/QtDesigner"
    "-I/usr/include/QtGui"
    "-I/usr/include/QtHelp"
    "-I/usr/include/QtMultimedia"
    "-I/usr/include/QtNetwork"
    "-I/usr/include/QtOpenGL"
    "-I/usr/include/QtScript"
    "-I/usr/include/QtScriptTools"
    "-I/usr/include/QtSql"
    "-I/usr/include/QtSvg"
    "-I/usr/include/QtTest"
    "-I/usr/include/QtUiTools"
    "-I/usr/include/QtWebKit"
    "-I/usr/include/QtXml"
    "-I/usr/include/QtXmlPatterns"
    "-I/usr/lib/clang/3.1/include"
    "-I/usr/lib/clang/3.4.2/include"
    "-I../lib/"))

(setq c-default-include-commands
      '("pkg-config --cflags sdl"
        "pkg-config --cflags gtk+-2.0"))

(set (make-variable-buffer-local 'c-local-include-dirs) nil)
(set (make-variable-buffer-local 'c-local-flags) nil)
(set (make-variable-buffer-local 'c-local-commands) nil)

(defun c-check-local-flags (x) (every 'stringp x))

(put 'c-local-include-dirs 'safe-local-variable 'c-check-local-flags)
(put 'c-local-flags 'safe-local-variable 'c-check-local-flags)

(defun c-configure-include-dirs ()
  (interactive)
  (hack-local-variables)
  (let ((project-include (locate-dominating-file (buffer-file-name)
                                                 "include")))
    (when project-include
      (add-to-list 'c-local-include-dirs
                   (concat "-I" (expand-file-name project-include)
                           "include"))))
  (add-to-list 'c-local-include-dirs
               (concat "-I" (file-name-directory (buffer-file-name))))
  (set-c-include-dirs (append c-local-include-dirs c-default-include-dirs)
                      (append c-local-commands c-default-include-commands)
                      c-local-flags))

(add-hook 'c-mode-common-hook
          'c-configure-include-dirs)

(add-hook 'c-mode-common-hook 'turn-on-auto-fill)
(add-hook 'c-mode-common-hook 'rainbow-mode)

(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))
(require 'column-marker)
(add-hook 'c-mode-common-hook '(lambda ()
                                 (unless (string= major-mode "java-mode")
                                   (flymake-mode t))
                                 (column-marker-2 80)))

(setq-default c-default-style "k&r")
(setq-default c-basic-offset 2)

(c-set-offset 'inextern-lang 0)

;; Auto-completion for ObjC
(setq ac-modes (append '(objc-mode) ac-modes))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (unless (string= major-mode "java-mode")
               (c-configure-include-dirs)
               (setq ac-sources '(ac-source-clang-async))
               (ac-clang-launch-completion-process))))

(defun objc-wrap-brackets (&optional count)
  (interactive "*p")
  (backward-up-list count)
  (insert "[")
  (forward-sexp +1)
  (save-excursion (insert "]"))
  (just-one-space))

(define-key objc-mode-map (kbd "C-c a") 'objc-wrap-brackets)
(define-key objc-mode-map (kbd "C-c b") 'xcode/build-compile)

(add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)

;; Flymake

(require 'flymake)

(defun flymake-clang-init (compiler &rest args)
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
          (append args
                  `("-fsyntax-only" ;; Don't compile my code

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
                    ,@(mapcar (lambda (s)
                                (concat "-I" (expand-file-name (substring s 2)
                                                               local-directory)))
                              c-include-dirs)

                    ;; Input file
                    ,local-file)
                  flymake-clang-extra-c-flags))))

(defvar flymake-clang-extra-c-flags ()
  "C flags")

(defun flymake-c-init ()
  (flymake-clang-init "clang"))

(defun flymake-cpp-init ()
  (flymake-clang-init "clang++" "-std=c++11"))

(push '(".+\\.c$" flymake-c-init) flymake-allowed-file-name-masks)
(push '(".+\\.h$" flymake-c-init) flymake-allowed-file-name-masks)
(push '(".+\\.m$" flymake-c-init) flymake-allowed-file-name-masks)

(push '(".+\\.cpp$" flymake-cpp-init) flymake-allowed-file-name-masks)
(push '(".+\\.hpp$" flymake-cpp-init) flymake-allowed-file-name-masks)

;; CScope

(require 'xcscope)
(define-key c-mode-base-map (kbd "M-.") 'cscope-find-global-definition)

(setq gdb-many-windows t)
