;; init-ruby.el - customizations for ruby, rails, etc.


(eval-after-load 'ruby-mode
  '(progn
     (add-hook 'ruby-mode-hook 'inf-ruby-keys)

     ;; stop the crazy indentation
     (setq ruby-deep-indent-paren-style nil)

     ;; always indent on newline
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

;; files that should trigger ruby-mode (besides *.rb)
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))

;; rvm.el
(require 'rvm)
(rvm-use-default)


(provide 'init-ruby)
