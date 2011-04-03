(add-to-list 'ac-dictionary-directories (config-dir "ac-dict"))

;; Completion in any buffer, aside from the minibuffer
(define-global-minor-mode ac-global-auto-complete-mode
  auto-complete-mode
  (lambda()
    (unless (minibufferp (current-buffer))
      (auto-complete-mode t))))

(ac-global-auto-complete-mode t)

(setq-default ac-sources
              '(ac-source-abbrev
                ac-source-dictionary
                ac-source-words-in-same-mode-buffers
                ac-source-words-in-buffer
                ac-source-yasnippet))

(setq ac-ignore-case t)

(setq yas/root-directory (config-file "snippets"))
(yas/reload-all)
(yas/global-mode t)
