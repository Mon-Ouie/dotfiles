(require 'deft)
(require 'poem-mode)

(add-hook 'poem-mode-hook 'turn-off-auto-fill)

(defun poem ()
  (interactive)
  (setq deft-extension "poem")
  (setq deft-directory "~/doc/poems/")
  (setq deft-text-mode 'poem-mode)
  (deft))
