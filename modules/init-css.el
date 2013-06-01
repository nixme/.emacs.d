;;; init-css.el --- CSS, Sass, SCSS


;;; Code:

(package 'sass-mode)

(package 'rainbow-mode)
(defun css-mode-defaults ()

  ;; Indent at 2 spaces
  (setq css-indent-offset 2)

  ;; rainbow-mode: Color previews in css/html
  (rainbow-mode t))

(add-hook 'css-mode-hook  'css-mode-defaults)
(add-hook 'sass-mode-hook 'css-mode-defaults)
(add-hook 'html-mode-hook 'css-mode-defaults)

;; SCSS files should use css-mode
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))


(provide 'init-css)
