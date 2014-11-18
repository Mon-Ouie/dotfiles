(unless (getenv "OZHOME") (setenv "OZHOME" "/usr/local"))

(when (executable-find "ozc")
  (setq auto-mode-alist
        (append '(("\\.oz\\'" . oz-mode)
                  ("\\.ozg\\'" . oz-gump-mode))
                auto-mode-alist))

  (require 'oz)

  (setq oz-change-title nil)

  (define-key oz-mode-map (kbd "M-p") nil)
  (define-key oz-mode-map (kbd "M-n") nil)

  (define-key oz-mode-map (kbd "C-c C-l") 'oz-feed-line)
  (define-key oz-mode-map (kbd "C-C C-b") 'oz-feed-buffer)
  (define-key oz-mode-map (kbd "C-c C-r") 'oz-feed-region)

  ;; Alignment rules

  (require 'align)

  (add-to-list 'align-rules-list
               '(oz-then
                 (regexp  . "\\(\\s-+\\)then")
                 (modes   . '(oz-mode))))

  (add-to-list 'align-rules-list
               '(oz-if-elseif-else
                 (regexp . "\\b\\(?:if\\|elseif\\)\\b\\(\\s-+\\)[^\\s]")
                 (modes  . '(oz-mode))))

  (add-to-list 'align-dq-string-modes    'oz-mode)
  (add-to-list 'align-sq-string-modes    'oz-mode)
  (add-to-list 'align-open-comment-modes 'oz-mode)

  (add-hook 'oz-mode-hook 'turn-on-auto-fill))
