;;; init.el - it starts here
;;
;; "I'm using Linux. A library that emacs uses to communicate
;;  with Intel hardware." -- Erwin on #emacs
;;
;; configuration inspired by:
;;   technomancy: http://github.com/technomancy/emacs-starter-kit
;;   Dirk-Jan C. Binnema: http://www.djcbsoftware.nl/dot-emacs.html
;;   Steve Purcell: http://github.com/purcell/emacs.d
;;   Steve Yegge: http://sites.google.com/site/steveyegge2/effective-emacs
;;   others...


;; setup load path
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(let ((default-directory (concat dotfiles-dir "/site-lisp/")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; load ELPA
(require 'package)
(package-initialize)

;; common libraries
(require 'cl)
(require 'misc)

;; load all customizations
(require 'init-ido)
(require 'init-speedbar)
(require 'init-eproject)
(require 'init-yasnippet)
(require 'init-ruby)
(require 'init-org-mode)
(require 'init-misc)
(require 'init-keybindings)
