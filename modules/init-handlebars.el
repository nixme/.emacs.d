;;; init-handlebars.el --- Handlebars

;;; Code:

(package 'handlebars-mode)
(add-to-list 'auto-mode-alist '("\\.handlebars$" . handlebars-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . handlebars-mode))


(provide 'init-handlebars)
