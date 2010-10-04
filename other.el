;;; misc.el - other custom settings
;;
;; inspired by:
;;   technomancy's emacs-starter-kit: http://github.com/technomancy/emacs-starter-kit
;;   Dirk-Jan C. Binnema's dot-emacs: http://www.djcbsoftware.nl/dot-emacs.html
;;   Steve Purcell's emacs.d: http://github.com/purcell/emacs.d
;;   others...

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

;; whitespace mode: show tabs, newlines, and highlight chars past column 80
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

;; ethan-wspace mode: clean whitespace on save
(global-ethan-wspace-mode t)

;; transparently open compressed files
(auto-compression-mode t)

;; use unified diffs
(setq diff-switches "-u")

;; use visual bell instead of an annoying sound
(setq visible-bell t)

;; interactively-do-things (ido)
(ido-mode t)
(setq
  ido-enable-flex-matching t
  ido-create-new-buffer 'always
  ido-max-prospects 8)

;; sr-speedbar
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)

;; org-mode
(setq org-log-done t)
(setq org-agenda-files (list "~/Dropbox/org/storage.org"
			     "~/Dropbox/org/sentech.org"
			     "~/Dropbox/org/hairbop.org"
			     "~/Dropbox/org/tippd.org"
			     "~/Dropbox/org/itsababy.org"))

;; ruby-mode
(eval-after-load 'ruby-mode
  '(progn
     (add-hook 'ruby-mode-hook 'inf-ruby-keys)
     (setq ruby-deep-indent-paren-style nil)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))

;; disable startup screens
(setq
  inhibit-startup-message t
  inhibit-startup-echo-area-message t)

(provide 'other)
