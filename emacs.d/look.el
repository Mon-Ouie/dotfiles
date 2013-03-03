;; Disable a few bad-looking features.
(tool-bar-mode -1)
(menu-bar-mode -1)
(if (boundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Enable transparency.
(set-frame-parameter (selected-frame) 'alpha '(70 70))
(add-to-list 'default-frame-alist '(alpha 70 70))

(show-paren-mode t)
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
                    :background "#333333" :underline t)

(require 'linum)
(set-face-foreground 'linum "#FFFFFF")

;; Colors in compile-mode
(require 'ansi-color)

(ansi-color-map-update 'ansi-color-names-vector
                       ["#3b3b3b" "#ff6b6b" "#a3d46e" "#eaca75" "#435e87"
                        "#cf8243" "#789ec6" "#ffffff"])

(defun colorize-compilation-buffer()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
