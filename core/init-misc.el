;; init-misc.el --- other modes, extensions, and customizations








;; use unified diffs
(setq diff-switches "-u")

;; let me use 'y' or 'n' to answer all questions cause i'm lazy
(defalias 'yes-or-no-p 'y-or-n-p)







(provide 'init-misc)
