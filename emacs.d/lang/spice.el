(require 'spice-mode)

(add-hook 'spice-mode-hook 'turn-on-eldoc-mode)

(define-key calc-mode-map (kbd "G") nil)
(define-key calc-mode-map (kbd "G t") 'spice-graph-trans)
(define-key calc-mode-map (kbd "G d") 'spice-graph-dc)
(define-key calc-mode-map (kbd "G a") 'spice-graph-ac)

(setq calc-gnuplot-default-device "qt")
