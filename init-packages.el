;;; init-packages.el - setup dependent elisp packages via el-get and elpa
;;
;; based on Steve Purcell's init-el-get.el


;; start ELPA
(require 'package)
(package-initialize)

;; set dependencies for el-get
(require 'el-get)
(setq el-get-byte-compile nil   ;; don't compile to bytecode by default
      el-get-sources
      '(
        ;; basics, editing, projects
        autopair
        undo-tree
        yasnippet
        (:name eproject
               :type git :url "http://github.com/jrockway/eproject.git")
        (:name ethan-wspace
               :type git :url "http://github.com/glasserc/ethan-wspace.git"
               :load-path ("lisp"))
        (:name sr-speedbar   :type emacswiki)
        (:name full-ack      :type elpa)
        (:name hungry-delete :type elpa)

        ;; appearance
        color-theme
        (:name color-theme-twilight
               :type git :url "http://github.com/crafterm/twilight-emacs.git")
        (:name idle-highlight :type elpa)

        ;; web, html, js, css, etc.
        (:name css-mode :type elpa)
        rainbow-mode

        ;; ruby, rails, etc.
        (:name ruby-mode :type elpa)
        (:name inf-ruby  :type elpa)
        (:name yaml-mode :type elpa)
        (:name haml-mode :type elpa)
        (:name sass-mode :type elpa)
        (:name rvm
               :type git :url "http://github.com/senny/rvm.el.git")

        ;; version control
        gist
        (:name magit  ;; custom recipe cause different emacs path on my macs
               :type git
               :url "http://github.com/philjackson/magit.git"
               :info "."
               :build ("make all")
               :build/darwin
               ("PATH=~/Applications/Emacs.app/Contents/MacOS:$PATH make all")
               :features magit)))

;; ensure packages are installed and loaded
(el-get 'sync)

;; TODO: add el-get-update-all fun from
;;   http://github.com/purcell/emacs.d/blob/master/init-el-get.el


(provide 'init-packages)
