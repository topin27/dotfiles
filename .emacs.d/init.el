;;; init --- Initialize.

;;; Commentary:
;; 

;;; Code:

;; (add-to-list 'load-path (expand-file-name "lisp" "~/.emacs.d/"))

(require 'package)
(setq package-archives '(("gnu-china" . "http://elpa.emacs-china.org/gnu/")
			 ("melpa-china" . "http://elpa.emacs-china.org/melpa/")
			 ("marmalade" . "http://elpa.emacs-china.org/marmalade/")))

(package-initialize)

(require 'cl)

(defvar my/packages '(
		      diminish
		      ace-jump-mode
		      undo-tree
		      projectile
		      ztree
		      smex
		      ido-vertical-mode
		      idomenu
		      imenu-list
		      evil
		      evil-leader
		      evil-surround
		      wgrep
		      hungry-delete
		      rainbow-delimiters
		      clean-aindent-mode
		      yasnippet
		      yasnippet-snippets
		      cython-mode
		      scala-mode
		      markdown-mode
		      magit
		      company
		      neotree
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

(show-paren-mode t)
(setq show-paren-style 'parentheses)

(delete-selection-mode t) ;; inserting text while the mark is active causes the selected text to be deleted first
(transient-mark-mode 1)
(setq select-enable-clipboard t)
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; (setq cursor-type 'bar)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(if (not (display-graphic-p))
    (progn
      ;; (xterm-mouse-mode t)
      (menu-bar-mode -1))
  (progn
    ;; (load-theme 'deeper-blue)
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

(set-face-foreground 'linum "#362E2E")
(setq linum-format "%d ")

(require 'diminish)
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "hungry-delete" '(diminish 'hungry-delete-mode))
(eval-after-load "company" '(diminish 'company-mode))
;; (diminish 'projectile-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (cond
;;  ((string-equal system-type "gnu/linux")
;;   (setenv "PATH" (concat "~/.local/bin" ":" (getenv "PATH"))))
;;  ((string-equal system-type "darwin")
;;   (setenv "PATH" (concat "~/Library/Python/2.7/bin" ":" "/usr/local/bin" ":" (getenv "PATH")))))
(if (string-equal system-type "darwin")
    (if (display-graphic-p)
	(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))))

;; (cond
;;  ((string-equal system-type "gnu/linux")
;;   (add-to-list 'exec-path "~/.local/bin"))
;;  ((string-equal system-type "darwin")
;;   (add-to-list 'exec-path "~/Library/Python/2.7/bin/")
;;   (add-to-list 'exec-path "/usr/local/bin/")))
(if (string-equal system-type "darwin")
    (if (display-graphic-p)
	(add-to-list 'exec-path "/usr/local/bin/")))

(setq make-backup-files nil)
(setq auto-save-default nil)

(winner-mode 1)

(windmove-default-keybindings)

;; (setq default-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-auto-revert-mode 1)

(setq evil-want-C-i-jump nil)
(require 'evil)
(setq evil-default-state 'emacs)
(evil-mode 1)
(modify-syntax-entry ?_ "w")
(loop for (mode . state) in '((org-mode . normal)
			      (markdown-mode . normal)
			      (prog-mode . normal)
			      (xref--xref-buffer-mode . emacs)
			      (shell-mode . normal)
			      (term-mode . emacs))
      do (evil-set-initial-state mode state))
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(require 'evil-leader)
(evil-leader/set-leader ",")
(global-evil-leader-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f10>") 'rename-buffer)
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)

(require 'dired)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'alway)
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)) ;; lazy-load
(setq dired-dwim-target t)

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'projectile)
(projectile-mode +1)
(setq projectile-enable-caching t)
(setq-default projectile-globally-ignored-files
	      (append '(".pyc" ".class" "~" ".cache") projectile-globally-ignored-files))
(setq-default projectile-globally-ignored-directories
	      (append '("__pycache__") projectile-globally-ignored-directories))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(setq smex-save-file (expand-file-name "smex-items" user-emacs-directory))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-everywhere t)
;; (setq ido-auto-merge-work-directories-length -1)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-show-count t)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

(require 'idomenu)
(global-set-key (kbd "C-c j i") 'idomenu)

(require 'ztree)

(require 'imenu-list)
(setq imenu-list-auto-resize t)
(global-set-key (kbd "C-\\") 'imenu-list-smart-toggle)

(require 'wgrep)
(setq wgrep-auto-save-buffer t)

(require 'hungry-delete)
(global-hungry-delete-mode)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(require 'neotree)
(global-set-key (kbd "C-c t n") 'neotree-toggle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'electric)
(electric-pair-mode t)
(electric-indent-mode t)
(defun my/toggle-paste ()
  (interactive)
  (setq electric-pair-mode (if (eq electric-pair-mode t) nil t))
  (setq electric-indent-mode (if (eq electric-indent-mode t) nil t)))
(global-set-key (kbd "C-c t p") 'my/toggle-paste)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

(require 'yasnippet)
;; (setq yas-snippet-dirs
;;       '(;; "~/.emacs.d/snippets"                 ;; personal snippets
;;         ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
;; 	;; "~/.emacs.d/site-lisp/yasnippet-snippets/snippets" ;; the yasnippet-snippets collection
;;         ))
(yas-global-mode 1)
;; (yas-reload-all)
;; (add-hook 'prog-mode-hook #'yas-minor-mode)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(global-set-key (kbd "C-c y") 'company-yasnippet)
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

(require 'cython-mode)
(setq auto-mode-alist
      (cons '(".pyx" . cython-mode) auto-mode-alist))

(require 'scala-mode)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)


(defun my/prog-mode ()
  (linum-mode t)
  (column-number-mode t)
  (line-number-mode t)
  (evil-leader/set-key "g g" 'xref-find-definitions)
  (evil-leader/set-key "g b" 'xref-pop-marker-stack)
  (evil-leader/set-key "g c" 'xref-find-references))
(add-hook 'prog-mode-hook 'my/prog-mode)

(defun my/c-mode ()
  (setq indent-tabs-mode t)
  (setq tab-width 8))
(add-hook 'c-mode-hook 'my/c-mode)

(defun my/java-mode ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4))
(add-hook 'java-mode-hook 'my/java-mode)

(defun my/python-mode ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (evil-leader/set-key "g g" 'xref-find-definitions)
  (evil-leader/set-key "g b" 'xref-pop-marker-stack)
  (evil-leader/set-key "g c" 'xref-find-references))
(add-hook 'python-mode-hook 'my/python-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(add-hook 'org-mode-hook
    (lambda ()
      (flyspell-mode -1)
      (toggle-truncate-lines -1)))
(setq org-html-postamble nil)
;; (setq org-startup-indented t)
(setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)" "CANCELLED(c)"))
      org-todo-keyword-faces '(("DOING" . (:foreground "cyan" :weight bold))
			       ("CANCELLED" . (:foreground "yellow" :weight bold))))
(setq org-src-fontify-natively t)
(setq org-directory "~/Workspace/notes/")
(setq org-agenda-files (list org-directory))
(setq org-default-notes-file (concat org-directory "/notes.org"))
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

;; (require 'ox-publish)
;; (setq org-publish-project-alist
;;       '(
;;         ("org-notes"
;;          :base-directory "~/Workspace/wiki/"
;;          :base-extension "org"
;;          :publishing-directory "~/Workspace/src/topin27.github.io/"
;;          :recursive t
;;          ;; :publishing-function org-publish-org-to-html
;; 	 :publishing-function org-html-publish-to-html
;;          :headline-levels 4             ; Just the default for this project.
;;          :auto-preamble t
;;          )
;;         ("org-static"
;;          :base-directory "~/Workspace/wiki/static/"
;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
;;          :publishing-directory "~/Workspace/src/topin27.github.io/static/"
;;          :recursive t
;;          :publishing-function org-publish-attachment
;;          )
;;         ("org" :components ("org-notes" "org-static"))
;; 	))

(defun my/org-mode ()
  (linum-mode t)
  (column-number-mode t)
  (line-number-mode t)
  (define-key evil-motion-state-map (kbd "C-i") 'org-cycle))
(add-hook 'org-mode-hook 'my/org-mode)

(defun my/markdown-mode ()
  (markdown-toggle-math)
  (linum-mode t)
  (column-number-mode t)
  (line-number-mode t))
(add-hook 'markdown-mode-hook 'my/markdown-mode)

(provide 'init)

;;; Auto-generated code below
