(require 'el-get)

(require 'package)

(setq package-archives
      '(("marmalade" . "http://marmalade-repo.org/packages/")
        ("tromey" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(setq el-get-sources
      `((:name ruby-dev :type git
               :url "https://github.com/Mon-Ouie/ruby-dev.el.git")

        (:name yaml-mode)
        (:name tuareg-mode)
        (:name haskell-mode :type git
	       :url "https://github.com/jwiegley/haskell-mode.git"
	       :load "haskell-mode.el")
        (:name structured-haskell-mode :type git
               :url "https://github.com/chrisdone/structured-haskell-mode.git"
               :build '(("cabal" "install"))
               :load-path ("elisp"))
        (:name cmake-mode)
        (:name lua-mode)
        (:name markdown-mode)
        (:name rhtml-mode)
        (:name pkgbuild-mode)
        (:name zencoding-mode)

	(:name slime
	       ;; :description "Superior Lisp Interaction Mode for Emacs"
	       ;; :type github
	       ;; :autoloads "slime-autoloads"
	       ;; :pkgname "nablaone/slime"
	       ;; :load-path ("." "contrib")
	       ;; :compile (".")
	       ;; :post-init (slime-setup)
	       )

        (:name clojure-mode)
        (:name queue :type elpa)
        (:name cider)
        (:name ac-cider :type git
               :url "https://github.com/clojure-emacs/ac-cider.git")

        (:name yari)
	(:name git-modes)
        (:name magit :type git
	       :url "https://github.com/magit/magit.git")

        (:name popup :type elpa)
        (:name fringe-helper
               :type http
               :url "http://nschum.de/src/emacs/fringe-helper/fringe-helper.el"
               :features fringe-helper
               :compile "fring-helper.el")
        (:name flymake)
        (:name flymake-point)
        (:name flymake-fringe-icons
               :description "Add icons in the fringe, for flymake"
               :depends fringe-helper
               :type http
               :url "https://gist.github.com/raw/759130/a85ebbc6bfc5fbab54677f4236e902b2da7bf41f/flymake-fringe-icons.el"
               :features flymake-fringe-icons)

        ;; (:name ispell-multi :type http
        ;;        :url "http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/ispell-multi.el")
        ;; (:name flyspell-babel :type http
        ;;        :depends ispell-multi
        ;;        :url "http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/flyspell-babel.el")

        (:name color-theme)

        (:name smartparens)
        (:name yasnippet :type elpa)

        (:name auto-complete :type elpa)
        (:name clang-complete-async)

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
               :url "git://github.com/senny/emacs-eclim.git")
        (:name rainbow-mode :type elpa)
        (:name gist)
        (:name helm)
        (:name helm-ls-git :type git
               :url "git://github.com/emacs-helm/helm-ls-git.git")
        (:name elfeed :type git
               :url "git://github.com/skeeto/elfeed.git")
        (:name undo-tree)
        (:name flx :type git
               :url "git://github.com/lewang/flx.git")
        (:name ido-vertical-mode :type git
               :url "git://github.com/emacsmirror/ido-vertical-mode.git")
        (:name visual-regexp :type git
               :url "git://github.com/benma/visual-regexp.el.git")
        (:name guide-key :type git
               :url "git://github.com/kbkbkbkb1/guide-key.git")
        (:name popwin)
        (:name s)
        (:name projectile :type git
               :url "git://github.com/bbatsov/projectile.git")
        (:name pkg-info :type elpa)

        (:name dash)
        (:name epl :type elpa)
        (:name htmlize)
        (:name ggtags :type elpa)
        (:name smex)))

(el-get 'sync)
