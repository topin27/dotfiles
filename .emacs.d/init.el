;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))    ;; Or "http://elpa.emacs-china.org/melpa/"
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
		      company
		      smex
		      ;; yasnippet
		      clean-aindent-mode
		      undo-tree
		      anaconda-mode
		      company-anaconda
		      ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Find Executable Path on OS X
;;(when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize))

;; ;; 判断某个包是否已经安装，如果没有则自动从ELPA中安装
;; (defun require-package (package &optional min-version no-refresh)
;;   "Install given PACKAGE, optionally requiring MIN-VERSION.
;; If NO-REFRESH is non-nil, the available package lists will not be
;; re-downloaded in order to locate PACKAGE."
;;   (if (package-installed-p package min-version)
;;       t
;;     (if (or (assoc package package-archive-contents) no-refresh)
;;         (package-install package)
;;       (progn
;;         (package-refresh-contents)
;;         (require-package package min-version t)))))


(add-to-list
 'load-path
 (expand-file-name "lisp" user-emacs-directory))


(require 'init-dev)
(require 'init-basic)

(provide 'init)
