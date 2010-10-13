;; init-yasnippet.el


(yas/initialize)

;; load included snippets
(yas/load-directory (concat el-get-dir "/yasnippet/snippets"))

;; load personal snippets
(yas/load-directory (concat dotfiles-dir "/snippets"))


(provide 'init-yasnippet)
