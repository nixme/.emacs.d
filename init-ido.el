;; init-ido.el - interactively do things (ido) customizations


(ido-mode t)

(setq
  ido-enable-flex-matching t
  ido-create-new-buffer 'always
  ido-max-prospects 8)
;; display ido results vertically, rather than horizontally
;;   from tipcharper, jpkotta: http://emacswiki.org/emacs/InteractivelyDoThings
(setq ido-decorations
      (quote
       ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]"
        " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)


(provide 'init-ido)
