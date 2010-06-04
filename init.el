
;; turn off menu and toolbar
;; TODO: only turn off menu bar if in terminal
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; setup load path
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rvm.el"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/ethan-wspace/lisp"))

;; load ELPA
(require 'package)
(package-initialize)

;; load color theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))
(load (concat dotfiles-dir "vendor/color-theme-twilight.el"))
(color-theme-twilight)

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

;; load extra modes
(require 'haml-mode)
(require 'sass-mode)

;; load other customizations
(require 'bindings)
(require 'misc)
