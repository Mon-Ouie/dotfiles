(require 'eclim)
(require 'eclimd)

(setq eclim-auto-save t)
(global-eclim-mode)

(require 'ac-emacs-eclim-source)
(add-hook 'eclim-mode-hook
          (lambda ()
            (flymake-mode -1)

            (add-to-list 'ac-sources 'ac-source-emacs-eclim)
            (add-to-list 'ac-sources 'ac-source-emacs-eclim-c-dot)))

(defun start-eclim ()
  (interactive)
  (start-eclimd "~/code/eclipse/"))
