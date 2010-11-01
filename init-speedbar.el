;; init-speedbar.el - customizations for speedbar and sr-speedbar


;; show all files
(setq speedbar-show-unknown-files t)

;; turn off the ugly icons
(setq speedbar-use-images nil)

;; left-side pane
(setq sr-speedbar-right-side nil)

;; don't refresh on buffer changes
(setq sr-speedbar-auto-refresh nil)

;; disable line numbers in the speedbar frame
(add-to-list 'linum-disabled-modes-list '(speedbar-mode))

(provide 'init-speedbar)
