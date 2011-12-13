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

;; Local sources - recipes not included with el-get
(setq el-get-sources
      '(
        (:name hungry-delete :type elpa)
        (:name eproject
               :type git :url "http://github.com/jrockway/eproject.git"
               :features (eproject eproject-extras))
        (:name idle-highlight-mode :type elpa)
        (:name highlight-parentheses :type elpa)
        (:name dlacewell-minimap
               :type git :url "https://github.com/dustinlacewell/emacs-minimap.git"
               :features minimap)))

(setq packages
      '(
        ;; basics, editing, projects
        autopair
        yasnippet
        ethan-wspace
        full-ack
        smex
        switch-window
        buffer-move
        undo-tree
        hungry-delete
        sr-speedbar
        linum-off
        eproject
        fill-column-indicator
        dlacewell-minimap

        ;; appearance
        color-theme
        theme-roller
        idle-highlight-mode
        highlight-parentheses

        ;; web, html, js, css, etc.
        css-mode
        coffee-mode
        rainbow-mode
        markdown-mode
        mustache-mode

        ;; ruby, rails, etc.
        ruby-mode
        ruby-electric
        ruby-compilation
        rvm
        rinari
        inf-ruby
        yaml-mode
        haml-mode
        sass-mode

        ;; Autocomplete
        auto-complete
        auto-complete-ruby
        auto-complete-etags
        auto-complete-yasnippet

        ;; version control
        gist
        magit
        magithub

        ;; other
        google-maps
        google-weather))

;; Ensure packages are installed and loaded
(el-get 'sync packages)
(el-get 'wait)


(provide 'init-packages)
