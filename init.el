
;; turn off menu and toolbar
;; TODO: only turn off menu bar if in terminal
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; setup load path
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme-twilight"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rvm.el"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/ethan-wspace/lisp"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rainbow-mode"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/undo-tree"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/autopair"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/eproject"))

;; load ELPA
(require 'package)
(package-initialize)

;; load color theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (load "color-theme-twilight")
     (color-theme-twilight)))

;; load misc
(require 'misc)

;; load whitespace
(require 'whitespace)

;; load ethan-wspace
(require 'ethan-wspace)

;; load sr-speedbar
(require 'sr-speedbar)

;; load rvm.el
(require 'rvm)
(rvm-use-default)

;; load org-mode
(require 'org-install)

;; load rainbow-mode
(require 'rainbow-mode)
(add-to-list 'auto-mode-alist '("\\.css$"  . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . rainbow-mode))

;; load undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; load autopair
(require 'autopair)
(autopair-global-mode)

;; load yasnippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "/vendor/yasnippet/snippets"))
(yas/load-directory (concat dotfiles-dir "/snippets"))

;; load eproject
(require 'eproject)
(require 'eproject-extras)
(define-project-type ruby (generic) (look-for "Gemfile"))

;; load extra modes
(require 'haml-mode)
(require 'sass-mode)

;; load other customizations
(require 'bindings)
(require 'other)
