;;; init-ui.el
;;
;;

(defvar invisible-character-color "grey17")

;; Turn off toolbars and scrollbars
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(scroll-bar-mode -1)

;; Turn off menus unless on OS X with a GUI
(if (and (fboundp 'menu-bar-mode)
         (not (and (eq system-type 'darwin)
                   window-system)))
    (menu-bar-mode -1))

;; Turn off startup screens
(setq
  inhibit-startup-message t
  inhibit-startup-echo-area-message t)

;; Font: Menlo 11pt on OS X, Droid Sans Mono 10pt on Linux
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Menlo" :height 110)
  (set-face-attribute 'default nil :family "Droid Sans Mono" :height 90))

;; Soothing colors :)
(package 'noctilux-theme)
(after 'noctilux-theme-autoloads
  (load-theme 'noctilux t))

;; Smooth scrolling
(setq
  scroll-margin 0
  scroll-conservatively 100000
  scroll-up-aggressively 0
  scroll-down-aggressively 0
  scroll-preserve-screen-position t
  scroll-error-top-bottom t
  mouse-wheel-progressive-speed nil
  mouse-wheel-scroll-amount '(1 ((shift . 1))))

;; Use visual bell instead of annoying sound. Don't error on scroll for scroll errors.
(setq visible-bell t)
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command '(mwheel-scroll scroll-up-command scroll-down-command previous-line next-line))
          (ding))))

;; UTF-8 baby!
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; Indentation and fill
(setq-default indent-tabs-mode nil     ; Spaces for indentation
              tab-width        4       ; Display tabs as 4 spaces
              c-basic-offset   4
              fill-column      80)     ; Wrap (fill) text at column 80
(package 'fill-column-indicator)       ; Graphically show fill column
(setq fci-rule-color invisible-character-color)
(add-hook 'prog-mode-hook (lambda() (turn-on-fci-mode)))

;; Whitespace: Show tabs and newlines. Highlight characters past fill.
(require 'whitespace)
(setq whitespace-style '(trailing tabs newline lines-tail tab-mark newline-mark)
      whitespace-line-column 80
      whitespace-display-mappings
      '(  ; Imitate textmate: triangle for tabs and logical-not for newlines
        (tab-mark     ?\t [?\u25B8 ?\t] [?\u00BB ?\t] [?\\ ?\t])
        (newline-mark ?\n [?\u00AC ?\n] [?$ ?\n])))
(set-face-foreground 'whitespace-newline invisible-character-color)
(set-face-foreground 'whitespace-tab     invisible-character-color)
(set-face-background 'whitespace-tab     nil)
(global-whitespace-mode t)

;; Modeline
;(package 'powerline)
;(powerline-default-theme)
(line-number-mode t)                   ; Show line number in modeline
(column-number-mode t)                 ; Show column number in modeline
(size-indication-mode t)               ; Show file size

;; Reduce text in modeline
(package 'diminish)
(after 'projectile '(diminish 'projectile "Proj"))
(after 'undo-tree '(diminish 'undo-tree "Undo"))
(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "EL")))

;; Fringe
(setq default-indicate-empty-lines t)  ; empty line indicator

;; Line numbers globally except in some modes (via linum-off)
(global-linum-mode t)
(package 'linum-off)
(require 'linum-off)
(add-to-list 'linum-disabled-modes-list '(speedbar-mode))

;; Highlight current line
(global-hl-line-mode t)
(set-face-background 'hl-line "#000000")

;; Vertical bar cursor
(set-default 'cursor-type 'bar)

;; Speedbar in the same frame
(package 'sr-speedbar)
(autoload 'sr-speedbar-toggle "sr-speedbar")  ; Avoid init until used
(after 'sr-speedbar
  (setq speedbar-show-unknown-files t)        ; Show all files
  (setq speedbar-use-images nil)              ; Turn off the ugly icons
  (setq sr-speedbar-right-side nil)           ; Left-side pane
  (setq sr-speedbar-auto-refresh nil)         ; Don't refresh on buffer changes

  ;; Nicer fonts for speedbar when in GUI
  (when (window-system)
    ;; keep monospace buttons, but smaller height
    (set-face-attribute 'speedbar-button-face nil :height 100)

    ;; change to system default UI font for entries
    (dolist (face (list 'speedbar-file-face 'speedbar-directory-face
                        'speedbar-tag-face  'speedbar-selected-face
                        'speedbar-highlight-face))
      (if (eq system-type 'darwin)  ;; Lucida Grande on OS X
          (set-face-attribute face nil :family "Lucida Grande" :height 110)
        (set-face-attribute face nil :family "Droid Sans" :height 100))))

  ;; No left fringe and half-size right fringe. TODO: Doesn't work
  (add-hook 'speedbar-mode-hook (lambda()
                             (message "FROM SPEEDBAR HOOK")
                             (message "window-fringes %S" (window-fringes))
                             (set-window-fringes nil 0))))
  ;(add-to-list 'speedbar-frame-parameters '(left-fringe . 0)))

;; Uniquify: better names for duplicate buffers
(require 'uniquify)

;; Quick window switching
(package 'switch-window)
(require 'switch-window)

;; Quick window swapping
(package 'buffer-move)
;; TODO - Move to keybindings?



;; dlacewell-minimap

;; highlight matching parens
(show-paren-mode t)
(setq show-paren-delay 0)

;; Rainbow highlight parens that enclose point
(package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Highlight all instances of word at point
(package 'idle-highlight-mode)
(add-hook 'prog-mode-hook (lambda() (idle-highlight-mode t)))

;; TODO:
;; - highlight indentation level
;; - git gutter in fringe
;; - adjust fringe and linum margin (window system only)
;; - check everything from terminal
;; - autocomplete, rubocop
;; - PR for slim-mode to derive from prog-mode
;; - Turn off hungry delete everywhere. It's annoying

(provide 'init-ui)
