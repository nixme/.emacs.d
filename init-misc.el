;; init-misc.el - other modes, extensions, and customizations


;; turn off menu and toolbar
;; TODO: only turn off menu bar if in terminal
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; font: Menlo 11pt
(set-face-attribute 'default nil :family "Menlo" :height 110)

;; UTF-8 baby!
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; indentation
(setq-default indent-tabs-mode nil  ;; use spaces for indentation
              tab-width        4    ;; display tabs as 4 spaces
              c-basic-offset   4)

;; modeline
(line-number-mode t)        ;; show line number in modeline
(column-number-mode t)      ;; show column number in modeline
(size-indication-mode t)    ;; show file size

;; fringe
(global-linum-mode t)                   ;; line numbers
(setq default-indicate-empty-lines t)   ;; empty line indicator

;; highlight current line
(global-hl-line-mode t)
(set-face-background 'hl-line "#000000")

;; auto-replace selection with keypress
(delete-selection-mode t)

;; smooth scrolling
(setq
  scroll-margin 0
  scroll-conservatively 100000
  scroll-up-aggressively 0
  scroll-down-aggressively 0
  scroll-preserve-screen-position t
  mouse-wheel-scroll-amount '(1 ((shift . 1))))

;; highlight matching parens
(show-paren-mode t)
(setq show-paren-delay 0)

;; transparently open compressed files
(auto-compression-mode t)

;; use unified diffs
(setq diff-switches "-u")

;; use visual bell instead of an annoying sound
(setq visible-bell t)

;; disable startup screens
(setq
  inhibit-startup-message t
  inhibit-startup-echo-area-message t)

;; color theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (load "color-theme-twilight")
     (color-theme-twilight)))

;; whitespace mode: show tabs, newlines, and highlight chars past column 80
(require 'whitespace)
(setq whitespace-style '(trailing tabs newline lines-tail tab-mark newline-mark)
      whitespace-line-column 80
      whitespace-display-mappings
      '(  ;; imitate textmate: triangle for tabs and logical-not for newlines
        (tab-mark     ?\t [?\u25B8 ?\t] [?\u00BB ?\t] [?\\ ?\t])
        (newline-mark ?\n [?\u00AC ?\n] [?$ ?\n])))
(set-face-foreground 'whitespace-newline "grey15")
(set-face-foreground 'whitespace-tab     "grey15")
(set-face-background 'whitespace-tab     nil)
(global-whitespace-mode t)   ;; enable whitespace-mode everywhere

;; ethan-wspace: cleanup whitespace on every save
(require 'ethan-wspace)
(global-ethan-wspace-mode t)

;; uniquify: better names for duplicate buffers
(require 'uniquify)

;; undo-tree: saner undo and fancy edit history visualization
(require 'undo-tree)
(global-undo-tree-mode)

;; autopair: electric punctuation insertion
(require 'autopair)
(autopair-global-mode)

;; rainbow-mode: color previews in css/html
(require 'rainbow-mode)
(add-to-list 'auto-mode-alist '("\\.css$"  . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . rainbow-mode))

;; other modes. TODO: use autoloads instead
(require 'haml-mode)
(require 'sass-mode)


(provide 'init-misc)