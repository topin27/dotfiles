(require 'package)
(setq package-enable-at-startup nil)

;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(setq package-archives '(("gnu-china"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("marmalade-china" . "http://elpa.emacs-china.org/marmalade/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package diminish)
(use-package bind-key)

(require 'diminish)
(require 'bind-key)

(global-font-lock-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

(show-paren-mode t)
(setq show-paren-style 'parentheses)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(if (not (display-graphic-p))
    (progn
      (xterm-mouse-mode t)
      (menu-bar-mode -1))
  (progn
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command))
  :config (smex-initialize))

(setq make-backup-files nil)
(setq auto-save-default nil)

(winner-mode 1)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(global-set-key (kbd "RET") 'newline-and-indent)

(use-package ido
  :config
  (ido-mode 1)
  :init
  (progn
    (setq ido-enable-flex-matching t)
    ;; (setq ido-use-filename-at-point 'guess)
    (setq ido-everywhere t)
    (setq ido-auto-merge-work-directories-length -1))
  :bind (("C-x b" . ido-switch-buffer)
	 ("C-x B" . ido-switch-buffer-other-window)))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  :init
  (setq ido-vertical-show-count t))

(use-package electric-indent-mode
  :disabled t)

(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode))

(use-package projectile
  :config
  (projectile-global-mode)
  :diminish
  :bind-keymap ("C-c p" . projectile-command-map))

(global-set-key (kbd "<f10>") 'rename-buffer)

(line-number-mode t)
(column-number-mode t)

(add-hook 'org-mode-hook
	  (lambda ()
	    (flyspell-mode 1)
	    (toggle-truncate-lines -1)))
(use-package org
  :config
  (add-hook 'org-mode-hook
	    (lambda ()
	      (flyspell-mode 1)
	      (toggle-truncate-lines -1)))
  :bind (("C-c o c" . org-capture)
	 ("C-c o a" . org-agenda)
	 ("C-c o l" . org-store-link))
  :init
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)"))
	org-todo-keyword-faces '(("DOING" . (:foreground "cyan" :weight bold)))))
