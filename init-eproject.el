;; init-eproject.el - customizations for eproject


(require 'eproject)
(require 'eproject-extras)

;; define other project types
(define-project-type ruby (generic) (look-for "Gemfile"))

;; use ido for completion
(setq eproject-completing-read-function 'eproject--ido-completing-read)


(provide 'init-eproject)
