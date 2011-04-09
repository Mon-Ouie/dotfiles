(require 'el-get)

(require 'package)
(setq package-archives (cons '("tromey" . "http://tromey.com/elpa/") package-archives))

(setq el-get-sources
      '((:name package)
        (:name el-get)

	(:name ruby-mode)
        (:name inf-ruby :type elpa)
        (:name ruby-block)
        (:name ruby-end)
        (:name rvm)
        (:name inf-ruby-bond :type git
               :url "git://github.com/pd/inf-ruby-bond"
               :load "inf-ruby-bond.el")

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

        (:name gist :type git
               :url "git://github.com/defunkt/gist.el"
               :load "gist.el")
        (:name yari)
        (:name auctex)
        (:name magit)
        (:name dictionary :type elpa)

        (:name fringe-helper
               :type http
               :url "http://nschum.de/src/emacs/fringe-helper/fringe-helper.el"
               :features fringe-helper
               :compile "fring-helper.el")
        (:name flymake-extension :type emacswiki)
        (:name flymake-point)
        (:name flymake-fringe-icons)

        (:name pos-tip)

        (:name color-theme)
        (:name color-theme-twilight)

        (:name textmate :type git
               :url "git://github.com/defunkt/textmate.el"
               :load "textmate.el")
        (:name autopair)
        (:name yasnippet)
        (:name auto-complete)
        (:name auto-complete-clang)))

(el-get 'sync)
