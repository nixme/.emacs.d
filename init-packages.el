;;; init-packages.el - setup dependent elisp packages via el-get and elpa
;;
;; based on Steve Purcell's init-el-get.el


;; Start ELPA
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'el-get)

;; Local sources - recipes not included with el-get
(setq el-get-sources
      '(
        (:name hungry-delete :type elpa)
        (:name idle-highlight-mode :type elpa)
        (:name gist :type elpa)     ;; gist in Maramalade handles deps better
        (:name dlacewell-minimap
               :type git :url "https://github.com/dustinlacewell/emacs-minimap.git"
               :features minimap)))

(setq packages
      '(
        ;; Basics, editing, projects
        autopair                       ;; Automatically pair braces and quotes
        yasnippet                      ;; Template/snippet system
        ethan-wspace                   ;; Smart whitespace cleanup
        full-ack                       ;; `ack` front-end
        smex                           ;; Ido for M-x
        switch-window                  ;; Visual window switching
        buffer-move                    ;; Simple buffer swaping between windows
        undo-tree                      ;; Vim-style undo system with graph
        hungry-delete                  ;; Hungry delete (whole whitespace deletion) in all modes
        sr-speedbar                    ;; In-frame Speedbar
        linum-off                      ;; Easily disable line numbers in some modes
        eproject                       ;; Project system for grouping files/buffers
        dlacewell-minimap              ;; Sublime2's MiniMap feature

        ;; Appearance
        fill-column-indicator          ;; Draw a vertical line at fill column
        idle-highlight-mode            ;; Highlight all occurrences of word at point
        highlight-parentheses          ;; Highlight matching parentheses at point
        rainbow-mode                   ;; Colorize colors (hex, names)
        powerline                      ;; Fancy modeline

        ;; Web, HTML, JS, CSS, etc.
        css-mode                       ;; CSS minor mode
        coffee-mode                    ;; CoffeeScript major mode
        markdown-mode                  ;; Markdown major mode
        mustache-mode                  ;; Mustache templates major mode
        haml-mode                      ;; Haml templates major mode
        sass-mode                      ;; SASS major mode

        ;; Ruby, Rails, etc.
        ruby-mode                      ;; Ruby major mode
        ruby-electric                  ;; Electric commands for Ruby
        ruby-compilation               ;; Run ruby processes into compilation buffers
        rinari                         ;; Rails minor mode
        inf-ruby                       ;; Inferior Ruby mode
        yaml-mode

        ;; Autocomplete
        auto-complete                  ;; Intelligent auto-completion system
        auto-complete-etags            ;; Auto-complete from etags
        auto-complete-yasnippet        ;; Auto-complete from snippets/templates

        ;; Version control
        gist                           ;; Github Gist integration
        magit                          ;; Git interface and modes
        magithub                       ;; Magit extensions for Github

        ;; Other
        google-maps                    ;; Google Maps in Emacs :)
        google-weather))               ;; Google Weather in Emacs

;; Ensure packages are installed and loaded
(el-get 'sync packages)
(el-get 'wait)


(provide 'init-packages)
