(require 'el-get)

(require 'package)

(setq package-archives
      '(("marmelade" . "http://marmalade-repo.org/packages/")
        ("tromey" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(setq el-get-sources
      '((:name ruby-block)
        (:name ruby-end)
        ;(:name rvm)
        ;; (:name inf-ruby-bond :type git
        ;;        :url "git://github.com/pd/inf-ruby-bond"
        ;;        :load "inf-ruby-bond.el")

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

	;; (:name pcache :type elpa)

        ;; (:name gh :type git
        ;;        :url "git://github.com/sigma/gh.el.git"
        ;;        :load "gh.el")

        ;; (:name gist :type git
        ;;        :url "git://github.com/defunkt/gist.el"
        ;;        :load "gist.el")

        (:name yari)
        (:name auctex)
        (:name magit)
        (:name dictionary :type elpa)

        (:name popup :type elpa)
        (:name fringe-helper
               :type http
               :url "http://nschum.de/src/emacs/fringe-helper/fringe-helper.el"
               :features fringe-helper
               :compile "fring-helper.el")
        (:name flymake-extension :type emacswiki)
        (:name flymake-point)
        (:name flymake-fringe-icons
               :description "Add icons in the fringe, for flymake"
               :depends fringe-helper
               :type http
               :url "https://gist.github.com/raw/759130/a85ebbc6bfc5fbab54677f4236e902b2da7bf41f/flymake-fringe-icons.el"
               :features flymake-fringe-icons)

        (:name pos-tip)

        (:name column-marker :type http
               :url "http://www.emacswiki.org/emacs/download/column-marker.el"
               :load "column-marker.el")

        (:name color-theme)
        ;(:name color-theme-twilight)

        (:name autopair)
        (:name yasnippet :type elpa)

        (:name auto-complete :type elpa)
        (:name auto-complete-clang)

        (:name dired+)
        (:name deft :type git
               :url "git://jblevins.org/git/deft.git"
               :load "deft.el")

        (:name c-eldoc
               :type elpa
               :load "c-eldoc.el")
        (:name android-mode)
        (:name ace-jump-mode :type elpa)
        (:name expand-region :type elpa)))

(el-get 'sync)
