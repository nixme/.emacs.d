;;; init-haml.el --- Haml

;;; Code:

(package 'haml-mode)

(package 'slim-mode)

(add-to-list 'auto-mode-alist '("\\.emblem\\'" . slim-mode))


(provide 'init-haml)
