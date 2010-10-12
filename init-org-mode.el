;; init-org-mode.el - customizations for org-mode


(require 'org-install)

;; track todo items
(setq org-log-done t)

;; my standard agenda files
(setq org-agenda-files
      (list
       "~/Dropbox/org/storage.org"
       "~/Dropbox/org/sentech.org"
       "~/Dropbox/org/hairbop.org"
       "~/Dropbox/org/tippd.org"
       "~/Dropbox/org/itsababy.org"))


(provide 'init-org-mode)
