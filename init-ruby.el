;; init-ruby.el - customizations for ruby, rails, etc.


(eval-after-load 'ruby-mode
  '(progn
     ;; stop the crazy indentation
     (setq ruby-deep-indent-paren-style nil)

     ;; always indent on newline
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)

     ;; ruby-compilation minor-mode, adds the following functions:
     ;;   ruby-compilation-run
     ;;   ruby-compilation-rake
     ;;   ruby-compilation-this-buffer (C-x t)
     ;;   ruby-compilation-this-buffer (C-x C-t)
     (require 'ruby-compilation)))

;; files that should trigger ruby-mode (besides *.rb)
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '(".metrics\\'" . ruby-mode))

;; unset ruby-electric keybindings for certain paired characters
;;   since autopair is much superior for them
(defun remove-some-electric-bindings ()
  (dolist (char '("\"" "\'" "(" ")" "{" "}" "[" "]"))
    (local-unset-key char)))
(add-hook 'ruby-mode-hook 'remove-some-electric-bindings 'append)


(provide 'init-ruby)
