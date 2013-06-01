;;; init-editor.el
;;
;;


(setq sentence-end-double-space nil)  ;; only 1 space after sentence period

;; auto-replace selection with keypress
(delete-selection-mode t)

;; Echo multi-key commands immediately in the echo area
(setq echo-keystrokes 0.01)

;; transparently open compressed files
(auto-compression-mode t)

;; Automatically pair braces and quotes
(package 'autopair)
(autopair-global-mode)

;; Snippet template system
(package 'yasnippet)
(require 'yasnippet)
(yas-load-directory (concat dotfiles-dir "/snippets"))  ; Personal snippets
(yas-global-mode 1)

;; OCD whitespace cleanup
(package 'ethan-wspace)
(global-ethan-wspace-mode t)
;; TODO: You should also remove any customizations you have made to turn on
;; either show-trailing-whitespace or require-final-newline; we handle those for
;; you. (But note that require-final-newline is turned on by some modes based on
;; the value of mode-require-final-newline.)

;; Semantically increase selection region
(package 'expand-region)
;; TODO: set keybinding

;; Edit with multiple cursors
(package 'multiple-cursors)

;; Move line or selection around with M-up, M-down, M-up, M-right
(package 'drag-stuff)

;; Autocomplete as you type
(package 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; Spell checking with aspell
(setq
 ispell-program-name "aspell"
 ispell-list-command "list"
 ispell-extra-args '("--sug-mode=ultra"))
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(custom-set-faces
 '(flyspell-incorrect ((t (:underline (:style wave :color "red"))))))

;; Flymake done right
(package 'flycheck)
(global-flycheck-mode)
;; TODO: further customization?

;; Treat undo history as a tree
(package 'undo-tree)
(global-undo-tree-mode)

;; Hungry deletion for all modes
(package 'hungry-delete)
(require 'hungry-delete)
(global-hungry-delete-mode)

;; ido - interactively do things
(package 'ido-ubiquitous)                 ; Use ido almost everywhere
(package 'ido-vertical-mode)              ; Vertical list of completions
(package 'ido-complete-space-or-hyphen)   ; Space will match - or _
(package 'smex)                           ; M-x replacement with MRU
(ido-mode t)
(ido-ubiquitous-mode)
(ido-vertical-mode)
(ido-complete-space-or-hyphen-enable)

;; auto-reload files that have changed on disk but haven't been modified
(global-auto-revert-mode t)

;; save autosave and backup files centrally instead of clutering projects
(setq
  backup-directory-alist         `(("." . ,(concat dotfiles-tmp-dir "backups")))
  auto-save-list-file-prefix     (concat dotfiles-tmp-dir "auto-save-sessions/")
  auto-save-file-name-transforms `((".*" ,(concat dotfiles-tmp-dir "auto-save-files/") t))
  auto-save-interval             500)

;; Projectile - Project management
(package 'projectile)
(setq
  projectile-cache-file (expand-file-name "projectile.cache" dotfiles-tmp-dir)
  projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" dotfiles-tmp-dir))
(projectile-global-mode)

;; saveplace: remember last position of opened files
(setq save-place-file (concat dotfiles-dir "saveplace"))
(setq-default save-place t)
(require 'saveplace)

;; recentf: quickly open recently used files
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

;; Automatically wrap comments at fill while typing
(add-hook 'prog-mode-hook
          (lambda() (auto-fill-mode 1) (setq comment-auto-fill-only-comments t)))

;; Automatically make scripts executable (hashbang as canary)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)



(package 'ack-and-a-half)


(provide 'init-editor)
