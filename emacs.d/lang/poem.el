(require 'deft)

(defun poem()
  (interactive)
  (setq deft-extension "poem")
  (setq deft-directory "~/doc/poems/")
  (setq deft-text-mode 'text-mode)
  (deft))
