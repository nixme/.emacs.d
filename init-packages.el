;;; init-packages.el - setup dependent elisp packages via el-get and elpa
;;
;; based on Steve Purcell's init-el-get.el


;; start ELPA
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; set dependencies for el-get
(require 'el-get)
(setq el-get-byte-compile nil   ;; don't compile to bytecode by default
      el-get-sources
      '(
        ;; basics, editing, projects
        autopair
        yasnippet
        ethan-wspace
        full-ack
        smex
        switch-window
        buffer-move
        (:name undo-tree :features undo-tree)
        (:name hungry-delete :type elpa)
        (:name sr-speedbar   :type emacswiki :features sr-speedbar)
        (:name linum-off
               :type elpa
               :after (lambda() (require 'linum-off)))
        (:name eproject
               :type git :url "http://github.com/jrockway/eproject.git"
               :features (eproject eproject-extras))

        ;; appearance
        color-theme
        theme-roller
        (:name idle-highlight :type elpa)

        ;; web, html, js, css, etc.
        (:name css-mode :type elpa)
        coffee-mode
        rainbow-mode
        markdown-mode

        ;; ruby, rails, etc.
        ruby-mode
        ruby-electric
        rvm
        rinari
        (:name inf-ruby :type elpa)
        yaml-mode
        haml-mode
        sass-mode

        ;; version control
        gist
        magit
        magithub

        ;; other
        google-maps
        google-weather))

;; ensure packages are installed and loaded
(el-get 'sync)


(provide 'init-packages)
