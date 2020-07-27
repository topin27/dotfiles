;;; init --- Initialize.

;;; Commentary:
;;

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" "~/.emacs.d/"))

(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize)

(require 'cl)

(defvar my/packages '(
		      better-defaults
		      diminish
		      ace-jump-mode
		      undo-tree
		      ztree
		      imenu-list
		      wgrep
		      clean-aindent-mode
		      yasnippet
		      yasnippet-snippets
		      markdown-mode
		      company
		      projectile
                      ido-vertical-mode
                      smex
		      magit
		      neotree
		      js2-mode
		      dracula-theme
                      evil
                      evil-surround
                      icomplete-vertical
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-font-lock-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)

(show-paren-mode t)
(setq show-paren-style 'parentheses)

(delete-selection-mode t) ;; inserting text while the mark is active causes the selected text to be deleted first
(transient-mark-mode 1)
(setq select-enable-clipboard t)
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(setq cursor-type 'bar)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(if (not (display-graphic-p))
    (progn
      ;; (xterm-mouse-mode t)
      (menu-bar-mode -1))
  (progn
    (load-theme 'dracula t)
    ;; (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(which-function-mode -1)
(linum-mode -1)
(column-number-mode -1)
(line-number-mode -1)
(setq mac-command-modifier 'control)

(require 'diminish)
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (string-equal system-type "darwin")
    (if (display-graphic-p)
	(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))))
(if (string-equal system-type "darwin")
    (if (display-graphic-p)
	(add-to-list 'exec-path "/usr/local/bin/")))

(setq make-backup-files nil)
(setq auto-save-default nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(winner-mode 1)

;; (setq default-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-auto-revert-mode 1)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "RET") 'newline-and-indent)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'dired)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)) ;; lazy-load
(setq dired-dwim-target t)

(require 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(require 'ztree)

(require 'imenu-list)
(setq imenu-list-auto-resize t)
(setq imenu-list-focus-after-activation t)
(global-set-key (kbd "C-c t i") 'imenu-list-smart-toggle)

(require 'wgrep)
(setq wgrep-auto-save-buffer t)

(require 'neotree)
(global-set-key (kbd "C-c t n") 'neotree-toggle)

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; (setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(add-to-list 'projectile-globally-ignored-files "tags")
(add-to-list 'projectile-globally-ignored-files "*.pyc")
(add-to-list 'projectile-globally-ignored-files "*.class")
(add-to-list 'projectile-globally-ignored-files "*.o")
(setq projectile-globally-ignored-directories
      (append '("__pycache__") projectile-globally-ignored-directories))
(setq projectile-globally-ignored-file-suffixes '("pyc" "class" "o"))

(require 'windmove)
(windmove-default-keybindings)
(global-set-key (kbd "C-c w h") 'windmove-left)
(global-set-key (kbd "C-c w l") 'windmove-right)
(global-set-key (kbd "C-c w j") 'windmove-down)
(global-set-key (kbd "C-c w k") 'windmove-up)

(require 'better-defaults)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-auto-merge-work-directories-length -1)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ido-switch-buffer-other-window)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-show-count t)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(require 'icomplete-vertical)
(icomplete-vertical-mode)
(setq completion-styles '(partial-completion substring))
(setq completion-category-overrides '((file (styles basic substring))))
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)
(define-key icomplete-minibuffer-map (kbd "<down>") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-n") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<up>") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-v") 'icomplete-vertical-toggle)

(setq evil-want-C-i-jump nil)
(modify-syntax-entry ?_ "w")
(require 'evil)
(setq evil-default-state 'normal)
(evil-mode 1)
(loop for (mode . state) in '((xref--xref-buffer-mode . emacs)
			      (special-mode . emacs)
			      (ztree-mode . emacs)
			      (term-mode . emacs))
      do (evil-set-initial-state mode state))
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "<f10>") 'rename-buffer)
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "C-c <tab>") 'other-window)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-normal-state-map (kbd "M-.") 'find-tag)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'electric)
(electric-pair-mode t)
(electric-indent-mode t)
(defun my/toggle-paste ()
  (interactive)
  (message "Toggle paste")
  (setq electric-pair-mode (if (eq electric-pair-mode t) nil t))
  (setq electric-indent-mode (if (eq electric-indent-mode t) nil t)))
(global-set-key (kbd "C-c t p") 'my/toggle-paste)

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)


(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-show-numbers t)
(global-set-key (kbd "C-c y") 'company-yasnippet)
(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         company-yasnippet
         )
        (company-abbrev company-dabbrev)
        ))
;; (setq company-backends '(company-dabbrev-code company-keywords company-semantic company-capf company-files (company-dabbrev company-yasnippet)))
(setq company-dabbrev-downcase nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))

(defun my/prog-mode ()
  (setq display-line-numbers 'relative)
  (column-number-mode t)
  (line-number-mode t))
(add-hook 'prog-mode-hook 'my/prog-mode)

(defun my/c-mode ()
  (setq indent-tabs-mode t)
  (setq tab-width 8))
(add-hook 'c-mode-hook 'my/c-mode)

(defun my/c++-mode ()
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2))
(add-hook 'c++-mode-hook 'my/c++-mode)

(defun my/java-mode ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4))
(add-hook 'java-mode-hook 'my/java-mode)

(defun my/python-mode ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4))
(add-hook 'python-mode-hook 'my/python-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(defun my/js-mode ()
  (setq indent-tabs-mode nil)
  (js2-minor-mode)
  (setq tab-width 4))
(add-hook 'js-mode-hook 'my/js-mode)

(defun my/shell-mode ()
  (setq company-mode -1)
  (setq line-number-mode -1)
  (setq column-number-mode -1))
(add-hook 'shell-mode-hook 'my/shell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(setq markdown-command "pandoc --toc -N --mathml")

(require 'org)
(setq org-html-postamble nil)
;; (setq org-startup-indented t)
(setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)" "CANCELLED(c)"))
      org-todo-keyword-faces '(("DOING" . (:foreground "cyan" :weight bold))
			       ("CANCELLED" . (:foreground "yellow" :weight bold))))
(setq org-src-fontify-natively t)
(setq org-directory "~/Workspace/notes/")
(setq org-agenda-files (list org-directory))
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/gtd.org") "Tasks")
	 "* TODO %?\n %i\n")
	("n" "Note" plain (file (concat org-directory "/notes.org"))
	 "* %?\n %x\n")
	("l" "Link" plain (file (concat org-directory "/links.org"))
	 "- %?\n %x\n")))
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o s") 'org-store-link)

(defun my/org-mode ()
  ;; (linum-mode t)
  (setq display-line-numbers 'relative)
  (column-number-mode t)
  (line-number-mode t)
  (define-key evil-motion-state-map (kbd "C-i") 'org-cycle)
  (flyspell-mode -1)
  (toggle-truncate-lines -1))
(add-hook 'org-mode-hook 'my/org-mode)

(defun my/markdown-mode ()
  (markdown-toggle-math)
  ;; (linum-mode t)
  (setq display-line-numbers 'relative)
  (define-key evil-motion-state-map (kbd "C-i") 'org-cycle)
  (column-number-mode t)
  (line-number-mode t))
  ;; (markdown-toggle-fontify-code-blocks-natively)
(add-hook 'markdown-mode-hook 'my/markdown-mode)

(provide 'init)

;; custome below
