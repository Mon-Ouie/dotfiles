(require 'deft)
(require 'org)

(defun doc()
  (interactive)
  (setq deft-extension "org")
  (setq deft-directory "~/doc/cours/")
  (setq deft-text-mode 'org-mode)
  (deft))
