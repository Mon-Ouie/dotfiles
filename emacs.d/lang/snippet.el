(add-hook 'snippet-mode-hook
          '(lambda ()
             (set (make-local-variable 'require-final-newline) nil)))

