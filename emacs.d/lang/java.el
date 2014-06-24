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
            (add-to-list 'ac-sources 'ac-source-emacs-eclim)))
(add-hook 'java-mode-hook (lambda ()
                            (add-to-list 'ac-sources 'ac-source-gtags)
                            (ggtags-mode)))
(setq android-mode-sdk-dir "/opt/android-sdk")

(let ((eclim-path "/usr/share/eclipse/plugins/org.eclim_2.3.2.63-ge44e750/bin"))
  (setq eclim-executable (concat eclim-path "/eclim")
        eclimd-executable (concat eclim-path "/eclimd")))

(defun start-eclim ()
  (interactive)
  (start-eclimd "~/code/eclipse/"))
