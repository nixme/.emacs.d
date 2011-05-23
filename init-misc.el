;; init-misc.el - other modes, extensions, and customizations


;; turn off toolbars
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; turn off menus unless on OS X with a GUI
(if (and (fboundp 'menu-bar-mode)
         (not (and (eq system-type 'darwin)
                   window-system)))
    (menu-bar-mode -1))

;; font: Menlo 11pt on OS X, Droid Sans Mono 10pt on Linux
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Menlo" :height 110)
  (set-face-attribute 'default nil :family "Droid Sans Mono" :height 100))

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

;; vertical bar cursor
(set-default 'cursor-type 'bar)

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

;; auto-reload files that have changed on disk but haven't been modified
(global-auto-revert-mode t)

;; use unified diffs
(setq diff-switches "-u")

;; use visual bell instead of an annoying sound
(setq visible-bell t)

;; let me use 'y' or 'n' to answer all questions cause i'm lazy
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable startup screens
(setq
  inhibit-startup-message t
  inhibit-startup-echo-area-message t)

;; ir-black colors via color-theme and theme-roller
(eval-after-load 'theme-roller '(color-theme-ir-black))

;; whitespace mode: show tabs, newlines, and highlight chars past column 80
(require 'whitespace)
(setq whitespace-style '(trailing tabs newline lines-tail tab-mark newline-mark)
      whitespace-line-column 80
      whitespace-display-mappings
      '(  ;; imitate textmate: triangle for tabs and logical-not for newlines
        (tab-mark     ?\t [?\u25B8 ?\t] [?\u00BB ?\t] [?\\ ?\t])
        (newline-mark ?\n [?\u00AC ?\n] [?$ ?\n])))
(set-face-foreground 'whitespace-newline "grey21")
(set-face-foreground 'whitespace-tab     "grey21")
(set-face-background 'whitespace-tab     nil)
(global-whitespace-mode t)   ;; enable whitespace-mode everywhere

;; ethan-wspace: cleanup whitespace on every save
(eval-after-load 'ethan-wspace '(global-ethan-wspace-mode t))

;; features to be enabled for any programming buffers
(defun enable-coding-mode-features ()
  (idle-highlight t)               ;; highlight instances of symbol under cursor
  (highlight-parentheses-mode t))  ;; color code nested parentheses
(add-hook 'ruby-mode-hook       'enable-coding-mode-features)
(add-hook 'js-mode-hook         'enable-coding-mode-features)
(add-hook 'emacs-lisp-mode-hook 'enable-coding-mode-features)

;; uniquify: better names for duplicate buffers
(require 'uniquify)

;; undo-tree: saner undo and fancy edit history visualization
(eval-after-load 'undo-tree '(global-undo-tree-mode))

;; autopair: electric punctuation insertion
(eval-after-load 'autopair '(autopair-global-mode))

;; rainbow-mode: color previews in css/html
(defun turn-on-rainbow-colors () (rainbow-mode t))
(add-hook 'css-mode-hook  'turn-on-rainbow-colors)
(add-hook 'sass-mode-hook 'turn-on-rainbow-colors)
(add-hook 'html-mode-hook 'turn-on-rainbow-colors)

;; coffee-mode: for coffeescript files
(add-hook 'coffee-mode-hook  ;; use two spaces for indenting
          '(lambda () (set (make-local-variable 'tab-width) 2)))

;; sh-mode for .zsh files
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

(provide 'init-misc)
