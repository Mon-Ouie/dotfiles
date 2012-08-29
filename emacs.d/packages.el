(require 'el-get)

(require 'package)

(setq package-archives
      '(("marmelade" . "http://marmalade-repo.org/packages/")
        ("tromey" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(setq el-get-sources
      '((:name ruby-end)

        (:name yaml-mode)
        (:name tuareg-mode)
        (:name haskell-mode :type git
	       :url "https://github.com/jwiegley/haskell-mode.git"
	       :load "haskell-mode.el")
        (:name cmake-mode)
        (:name lua-mode)
        (:name markdown-mode)
        (:name rhtml-mode)
        (:name pkgbuild-mode)
        (:name zencoding-mode)
        (:name slime)

        (:name yari)
        (:name auctex)
        (:name magit)

        (:name popup :type elpa)
        (:name fringe-helper
               :type http
               :url "http://nschum.de/src/emacs/fringe-helper/fringe-helper.el"
               :features fringe-helper
               :compile "fring-helper.el")
        (:name flymake-point)
        (:name flymake-fringe-icons
               :description "Add icons in the fringe, for flymake"
               :depends fringe-helper
               :type http
               :url "https://gist.github.com/raw/759130/a85ebbc6bfc5fbab54677f4236e902b2da7bf41f/flymake-fringe-icons.el"
               :features flymake-fringe-icons)

        (:name ispell-multi :type http
               :url "http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/ispell-multi.el")
        (:name flyspell-babel :type http
               :depends ispell-multi
               :url "http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/flyspell-babel.el")

        (:name color-theme)

        (:name autopair)
        (:name yasnippet :type elpa)

        (:name auto-complete :type elpa)
        (:name auto-complete-clang)

        (:name deft :type git
               :url "git://jblevins.org/git/deft.git"
               :load "deft.el")

        (:name c-eldoc
               :type elpa
               :load "c-eldoc.el")
        (:name android-mode)
        (:name ace-jump-mode :type elpa)
        (:name expand-region :type elpa)
        (:name multiple-cursors :type git
               :url "git://github.com/magnars/multiple-cursors.el.git")
        (:name eclim :type git
               :url "git://github.com/senny/emacs-eclim.git")))

(el-get 'sync)
