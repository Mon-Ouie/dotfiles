(require 'org)

(add-hook 'org-mode-hook '(lambda () (auto-fill-mode t)))

(setq org-export-latex-listings 'minted)

(setq org-export-latex-packages-alist ; (options package-name required-for-previews)
      '(("" "minted" nil) ;; syntax highlighting
        ("" "mhchem" t)  ;; typesetting chemistry
        ))

(setq org-file-apps
      `(("\\.pdf\\'" . ,(if on-osx "open %s" "xdg-open %s"))
        (auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)))

(setq org-format-latex-options
      '(:foreground default
        :background default
        :scale 1.3
        :html-foreground "Black"
        :html-background "Transparent"
        :html-scale 1.0
        :matchers
        ("begin" "$1" "$" "$$" "\\(" "\\[")))
