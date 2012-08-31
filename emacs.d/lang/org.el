(require 'org)
(require 'org-habit)

(add-hook 'org-mode-hook '(lambda () (auto-fill-mode t)))
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'detect-language)

(setq org-export-latex-listings 'minted)

(setq org-export-latex-packages-alist ; (options package-name required-for-previews)
      '(("" "minted" nil) ;; syntax highlighting
        ("" "mhchem" t)   ;; typesetting chemistry
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

(setq org-export-latex-minted-langs
      '((emacs-lisp "common-lisp")
        (lisp "common-lisp")
        (cc "c++")
        (cperl "perl")
        (shell-script "bash")
        (caml "ocaml")))

(setq org-export-html-postamble nil)

(setq org-format-latex-options
      '(:foreground default
        :background default
        :scale 1.3
        :html-foreground "Black"
        :html-background "Transparent"
        :html-scale 1.0
        :matchers
        ("begin" "$1" "$" "$$" "\\(" "\\[")))

(setq org-refile-use-outline-path t)
(setq org-refile-targets '((nil . (:maxlevel . 3))))

(setq org-directory "~/doc")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(require 'org-contacts)
(setq org-contacts-files '("~/doc/contacts.org"))

(require 'org-latex)

(setq memoir-header-code
               "\\documentclass[11pt]{memoir}
\\nouppercaseheads
\\pagestyle{Ruled}
")

(add-to-list 'org-export-latex-classes
             `("memoir" ,memoir-header-code

               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))

(add-to-list 'org-export-latex-classes
             `("memoir-with-part" ,memoir-header-code

               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
