(require 'eclim)
(require 'eclimd)

(setq eclim-auto-save t)
(global-eclim-mode)

(require 'ac-emacs-eclim-source)
(add-hook 'eclim-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-emacs-eclim)
            (add-to-list 'ac-sources 'ac-source-emacs-eclim-c-dot)))

(defun start-eclim ()
  (interactive)
  (start-eclimd "~/code/eclipse/"))
