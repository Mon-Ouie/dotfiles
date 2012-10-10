(require 'eclim)
(require 'eclimd)

(defun eclim-package-and-class ()
  (let ((package-name (eclim--java-current-package))
        (class-name   (eclim--java-current-class-name)))
    (if package-name (concat package-name "." class-name)
      class-name)))

(global-eclim-mode)
(setq eclim-auto-save nil)

(require 'bluej)

(require 'ac-emacs-eclim-source)
(add-hook 'eclim-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-emacs-eclim)
            (add-to-list 'ac-sources 'ac-source-emacs-eclim-c-dot)))

(defun start-eclim ()
  (interactive)
  (start-eclimd "~/code/eclipse/"))
