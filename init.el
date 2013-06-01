;;; init.el --- it starts here

;;; Commentary:
;;
;; "I'm using Linux. A library that emacs uses to communicate
;;  with Intel hardware." -- Erwin on #emacs
;;
;; configuration inspired by:
;;   technomancy: http://github.com/technomancy/emacs-starter-kit
;;   Dirk-Jan C. Binnema: http://www.djcbsoftware.nl/dot-emacs.html
;;   Steve Purcell: http://github.com/purcell/emacs.d
;;   Steve Yegge: http://sites.google.com/site/steveyegge2/effective-emacs
;;   Bozhidar Batsov's Prelude: https://github.com/bbatsov/prelude
;;   others...


;;; Code:

(defvar dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(defvar dotfiles-tmp-dir (concat dotfiles-dir "tmp/"))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/core"))
(add-to-list 'load-path (concat dotfiles-dir "/modules"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa"))

(require 'init-packages)

;; On OS X, envvars (PATH, MANPATH, etc.) are not loaded from the user's
;; profile. So extract them from a proper subshell.
(when (memq window-system '(mac ns))
  (package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))


;; Libraries
(require 'cl)
(require 'misc)
(require 'thingatpt)

;; Core customizations
(require 'init-editor)
(require 'init-ui)
(require 'init-misc)
(require 'init-keybindings)

;; Modules
(require 'init-ruby)
(require 'init-haml)
(require 'init-css)
(require 'init-handlebars)
(require 'init-js)
(require 'init-markdown)
(require 'init-shell)
(require 'init-git)
(require 'init-org-mode)


;; Start daemon for emacsclient
(server-start)
