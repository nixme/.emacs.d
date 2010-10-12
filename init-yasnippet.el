;; init-yasnippet.el


(require 'yasnippet)

(yas/initialize)

;; load included snippets
(yas/load-directory (concat dotfiles-dir "/site-lisp/yasnippet/snippets"))

;; load personal snippets
(yas/load-directory (concat dotfiles-dir "/snippets"))


(provide 'init-yasnippet)
