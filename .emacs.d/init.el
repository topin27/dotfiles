(add-to-list
 'load-path
 (expand-file-name "lisp" "~/.emacs.d/"))

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages")
			 ("gnu-china" . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("melpa-china" . "http://elpa.emacs-china.org/melpa/")
 			 ("marmalade-china" . "http://elpa.emacs-china.org/marmalade/")
			 ("marmalade" . "http://elpa.emacs-china.org/marmalade/")))

(package-initialize)

(require 'cl)

(defvar my/packages '(
		      material-theme
		      autopair
		      ace-jump-mode
		      undo-tree
		      multi-term
		      pyvenv
		      helm
		      helm-ag
		      company
		      company-jedi
		      flycheck
		      clean-aindent-mode
		      magit
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

(setq user-full-name "Yang Tianping")
(setq user-mail-address "yangtianpingytp@163.com")

(require 'init-ui)
(require 'init-basic)
(require 'init-dev)

(provide 'init)
