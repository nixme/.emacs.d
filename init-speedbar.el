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

;; nicer fonts for speedbar when in GUI
(when (window-system)
  ;; keep monospace buttons, but smaller height
  (set-face-attribute 'speedbar-button-face nil :height 100)

  ;; change to system default UI font for entries
  (dolist (face (list 'speedbar-file-face 'speedbar-directory-face
                      'speedbar-tag-face  'speedbar-selected-face
                      'speedbar-highlight-face))
    (if (eq system-type 'darwin)  ;; Lucida Grande on OS X
        (set-face-attribute face nil :family "Lucida Grande" :height 110)
      (set-face-attribute face nil :family "Droid Sans" :height 100))))

;; no left fringe and half-size right fringe. TODO: doesn't work
(add-to-list 'speedbar-frame-parameters '(left-fringe . 0))


(provide 'init-speedbar)
