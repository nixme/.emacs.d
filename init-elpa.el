;;; init-elpa.el - setup emacs packages via elpa


;; start ELPA
(require 'package)
(package-initialize)

(defvar required-packages
  (list 'idle-highlight
        'ruby-mode
        'inf-ruby
        'css-mode
        'yaml-mode
        'magit
        'gist
        'full-ack
        'hungry-delete)
  "Required packages for this emacs configuration.")

(defun elpa-install-required-packages ()
  "Install all required packages that aren't installed"
  (interactive)
  (dolist (package required-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))


(provide 'init-elpa)
