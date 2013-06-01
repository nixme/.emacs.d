;;; init-js.el

;;; Code:

;; coffee-mode: for coffeescript files
(package 'coffee-mode)
(add-hook 'coffee-mode-hook  ;; use two spaces for indenting
          '(lambda () (set (make-local-variable 'tab-width) 2)))


(provide 'init-js)
