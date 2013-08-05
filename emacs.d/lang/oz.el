(unless (getenv "OZHOME") (setenv "OZHOME" "/usr/local"))

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
