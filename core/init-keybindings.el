;;; init-keybindings.el - all keybindings in one place for easy reference


;; make Cmd key act as Meta
(setq ns-command-modifier 'meta)

;; auto-indent on new line
(global-set-key (kbd "RET") 'newline-and-indent)

;; more familiar forward and backward word
(global-set-key (kbd "M-f") 'forward-same-syntax)
(global-set-key (kbd "M-b") (lambda () (interactive)
                              (forward-same-syntax -1)))

;; dwim C-a: move to indentation or beginning of line if already there
(defun beginning-of-indentation-or-line ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key (kbd "C-a") 'beginning-of-indentation-or-line)

;; saner forward and backward kill-word using thingatpt
(defun kill-syntax (&optional arg)
  (interactive "p")
  (let ((opoint (point)))
    (forward-same-syntax arg)
    (kill-region opoint (point))))
(defun backward-kill-syntax (&optional arg)
  (interactive)
  (kill-syntax -1))
(global-set-key (kbd "M-d") 'kill-syntax)
(global-set-key (kbd "C-w") 'backward-kill-syntax) ;; same as shell keybinding
(global-set-key (kbd "C-<backspace>") 'backward-kill-syntax)

;; remap kill-region (cut) since we're using C-w above
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

;; kill line, same as shell
(defun backward-kill-line (arg)
  (interactive "p")
  (kill-line 0))
(global-set-key (kbd "M-k") 'backward-kill-line)

;; alias C-h to backspace and redefine help key to C-x ? or C-c ?
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-c ?") 'help-command)

;; find file
(global-set-key (kbd "C-x f")   'ido-find-file)  ;; I mistakenly type this often
(global-set-key (kbd "C-x C-f") 'ido-find-file)  ;; implicit when loading ido

;; find recent file
(defun ido-find-file-recent ()
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x C-r") 'ido-find-file-recent)

;; ido-completing M-x using smex + easier keybindings based on Yegge rec
(global-set-key (kbd "M-x")     'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "M-X")     'smex-major-mode-commands)

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
(global-set-key (kbd "C-c m") 'minimap-toggle)

;; window management
(windmove-default-keybindings)  ;; shift + arrow keys to switch windows
;; control + shift + arrow keys to move windows
(global-set-key (kbd "C-S-<left>")  'buf-move-left)
(global-set-key (kbd "C-S-<right>") 'buf-move-right)
(global-set-key (kbd "C-S-<down>")  'buf-move-down)
(global-set-key (kbd "C-S-<up>")    'buf-move-up)
;; meta + shift + arrow keys to resize windows
(global-set-key (kbd "S-M-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-M-<down>")  'shrink-window)
(global-set-key (kbd "S-M-<up>")    'enlarge-window)

;; magit
(global-set-key (kbd "C-c g") 'magit-status)

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "M-o") 'projectile-find-file)


(provide 'init-keybindings)
