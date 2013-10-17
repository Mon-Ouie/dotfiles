;; Disable a few bad-looking features.
(tool-bar-mode -1)
(menu-bar-mode -1)
(if (boundp 'scroll-bar-mode) (scroll-bar-mode -1))

(let ((opacity 100))
  (set-frame-parameter (selected-frame) 'alpha
                       (list opacity opacity))
  (add-to-list 'default-frame-alist
               (list 'alpha opacity opacity)))

(column-number-mode t)

(setq display-time-format "%H:%M")
(display-time)

(setq visible-bell t)

(setq inhibit-startup-message t
      initial-scratch-message nil)

(require 'color-theme)
(color-theme-initialize)
(load-file (config-file "my-charcoal.el"))

(my-color-theme-charcoal-black)
;; (color-theme-clarity)

(require 'magit)
(set-face-foreground 'magit-diff-add "green")
(set-face-attribute 'highlight
                    nil
                    :background "#333333" :underline nil)
(set-face-background 'magit-diff-hunk-header "#333333")
(set-face-foreground 'magit-diff-hunk-header "#afafaf")

(set-face-background 'magit-diff-file-header "#222222")
(set-face-foreground 'magit-diff-file-header "#bfbfbf")

(set-face-background 'flymake-errline "#ff6b6b")
(set-face-foreground 'flymake-errline "white")

(set-face-background 'flymake-warnline "#cf8243")
(set-face-foreground 'flymake-warnline "white")

(require 'linum)
(set-face-foreground 'linum "#7f7f7f")

(require 'smartparens)
(set-face-background 'sp-show-pair-match-face "#434E97")
(set-face-foreground 'sp-show-pair-match-face "white")

;; Colors in compile-mode
(require 'ansi-color)

(ansi-color-map-update 'ansi-color-names-vector
                       ["#3b3b3b" "#ff6b6b" "#a3d46e" "#eaca75" "#435e87"
                        "#cf8243" "#789ec6" "#7f7f7f"])

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'helm)
(set-face-foreground 'helm-selection "white")
