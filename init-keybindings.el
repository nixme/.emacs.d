;;; init-keybindings.el - all keybindings in one place for easy reference


;; make Cmd key act as Meta
(setq ns-command-modifier 'meta)

;; auto-indent on new line
(global-set-key (kbd "RET") 'newline-and-indent)

;; more familiar forward and backward word
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-b") 'backward-to-word)

;; easy kill word, also behaves same as shell, but need to remap kill-region too
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

;; kill line, same as shell
(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))
(global-set-key (kbd "C-u") 'backward-kill-line)

;; alias C-h to backspace and redefine help key to C-x ? or C-c ?
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-c ?") 'help-command)

;; find file
(global-set-key (kbd "C-x f")   'ido-find-file)  ;; I mistakenly type this often
(global-set-key (kbd "C-x C-f") 'ido-find-file)  ;; implicit when loading ido

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

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;; eproject
;;   expanded from http://github.com/jrockway/eproject/wiki/InstallingEproject
;;   for readability
(global-set-key (kbd "C-x p v") (lambda nil (interactive)
                                  (eproject-revisit-project 4)))
(global-set-key (kbd "C-x p V") (lambda nil (interactive)
                                  (eproject-revisit-project 1)))
(define-key eproject-mode-map (kbd "C-x p k")
  (lambda nil (interactive) (eproject-kill-project-buffers 4)))
(define-key eproject-mode-map (kbd "C-x p K")
  (lambda nil (interactive) (eproject-kill-project-buffers 1)))
(define-key eproject-mode-map (kbd "C-x p b")
  (lambda nil (interactive) (eproject-ibuffer 4)))
(define-key eproject-mode-map (kbd "C-x p B")
  (lambda nil (interactive) (eproject-ibuffer 1)))
(define-key eproject-mode-map (kbd "C-x p o")
  (lambda nil (interactive) (eproject-open-all-project-files 4)))
(define-key eproject-mode-map (kbd "C-x p O")
  (lambda nil (interactive) (eproject-open-all-project-files 1)))
(define-key eproject-mode-map (kbd "C-x p c") 'eproject-eshell-cd-here)


(provide 'init-keybindings)
