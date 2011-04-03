(require 'cc-mode)

(add-hook 'c-mode-common-hook 'turn-on-auto-fill)

(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

(add-hook 'c-mode-common-hook '(lambda ()
                                 (set (make-local-variable 'tab-width) 3)))

(setq-default c-basic-offset 3)
;; Auto-completion for ObjC
(setq ac-modes (append '(objc-mode) ac-modes))

(defun objc-wrap-brackets (&optional count)
  (interactive "*p")
  (backward-up-list count)
  (insert "[")
  (forward-sexp +1)
  (save-excursion (insert "]"))
  (just-one-space))

(define-key objc-mode-map (kbd "C-c a") 'objc-wrap-brackets)
(define-key objc-mode-map (kbd "C-c b") 'xcode/build-compile)
