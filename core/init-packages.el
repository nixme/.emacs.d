;;; init-packages.el --- Setup package management system via ELPA

;;; Commentary:
;; Based on code and ideas from Steve Purcell's init-elpa.el and milkypostman's
;; http://milkbox.net/note/single-file-master-emacs-configuration/
;;
;; See http://www.emacswiki.org/emacs/ELPA for additional information.


;;; Code:

(require 'package)

;; Marmalade and Melpa additional package repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; On-demand installation of packages
(defun package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (package package min-version t)))))

;; Run code after a file has loaded. (Wrapper for `eval-after-load' and `progn')
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))


(provide 'init-packages)

;;; init-packages.el ends here
