;;; bindings.el - my custom keybindings
;;
;; inspired by:
;;   technomancy's emacs-starter-kit: http://github.com/technomancy/emacs-starter-kit
;;   Steve Yegge's recommendations: http://sites.google.com/site/steveyegge2/effective-emacs


;; make Cmd key act as Meta
(setq ns-command-modifier 'meta)

;; auto-indent on new line
(global-set-key (kbd "RET") 'newline-and-indent)

;; easy kill word, also behaves same as shell, but need to remap kill-region too
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

;; alias C-h to backspace and redefine help key to C-x ? or C-c ?
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-c ?") 'help-command)

;; alternatives to M-x, eval command (also based on Yegge)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; align code
(global-set-key (kbd "C-x \\") 'align-regexp)

;; completion that uses many different methods to find options
(global-set-key (kbd "M-/") 'hippie-expand)

;; cleanup
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; switch to regexp as default search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; files and buffers
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)           ;; use ibuffer instead of buffer-list
(global-set-key (kbd "C-c t") 'sr-speedbar-toggle)

;; Window switching
(windmove-default-keybindings)  ;; shift + arrow keys


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode-specific bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(provide 'bindings)
