;;; init-ruby.el --- Customizations for Ruby, Rails, etc.

;;; Code:

;; Enhanced Ruby Mode - better syntax highlighting using MRI 1.9+ Ripper
(package 'enh-ruby-mode)
(after 'enh-ruby-mode
  ;; Always indent on newline
  (define-key enh-ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)

  ;; Use wavy underlines for syntax errors and warnings, not a box.
  (custom-set-faces
   '(erm-syn-warnline ((t (:underline (:style wave :color "orange")))))
   '(erm-syn-errline ((t (:underline (:style wave :color "red")))))))

;; Files that are also ruby files
(add-to-list 'auto-mode-alist '("\\.rake\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '(".metrics\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '(".irbrc\\'" . enh-ruby-mode))

;; Electric keys for pairing blocks and delimiters. Better heredoc behavior.
;; TODO necessary?: unset ruby-electric keybindings for certain paired
;; characters since autopair is much superior for them
(package 'ruby-electric)
(add-hook 'enh-ruby-mode-hook
          (lambda()
            (ruby-electric-mode)
            (dolist (char '("\"" "\'" "(" ")" "{" "}" "[" "]"))
              (local-unset-key char))))

;; Miscellaneous
(package 'ruby-tools)
(add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)


(package 'yaml-mode)


(provide 'init-ruby)
