;;; init --- Initialize.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Yang Tianping")
(setq user-mail-address "yangtianpingytp@163.com")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
;;                          ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)

(require 'cl)

(defvar my/packages '(
		      better-defaults
		      ace-jump-mode
		      undo-tree
		      imenu-list
		      yasnippet
		      yasnippet-snippets
		      markdown-mode
		      company
		      projectile
                      ag
		      js2-mode
                      evil
                      helm
                      evil-surround
                      magit
                      diminish
                      org-download
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm)
(require 'helm-config)
(helm-mode 1)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source    -1 ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)
(set-face-attribute 'helm-selection nil
		    :background "purple"
		    :foreground "black")
(defun my/helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook
          'my/helm-hide-minibuffer-maybe)
;; (setq helm-autoresize-max-height 0)
;; (setq helm-autoresize-min-height 20)
;; (helm-autoresize-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h i") 'helm-semantic-or-imenu)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h b") 'helm-resume)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h /") 'helm-do-ag)
(global-set-key (kbd "C-c h m") 'helm-man-woman)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(global-set-key (kbd "C-c h C-r") 'helm-recentf)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

(show-paren-mode t)
(setq show-paren-style 'parentheses)

(delete-selection-mode t) ;; inserting text while the mark is active causes the
                          ;; selected text to be deleted first
;; (transient-mark-mode 1)
(setq select-enable-clipboard t)

;; maximized window when startup
;; (setq initial-frame-alist (quote ((fullscreen . maximized))))

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq cursor-type 'bar)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(if (not (display-graphic-p))
    (progn
      (xterm-mouse-mode t)
      (menu-bar-mode -1))
  (progn
    ;; (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(which-function-mode -1)
;; (setq mac-command-modifier 'control)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
;; (setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Wrap lines at 80 characters
(setq-default fill-column 80)

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; ;; hippie expand is dabbrev expand on steroids
;; (setq hippie-expand-try-functions-list '(try-expand-dabbrev
;;                                          try-expand-dabbrev-all-buffers
;;                                          try-expand-dabbrev-from-kill
;;                                          try-complete-file-name-partially
;;                                          try-complete-file-name
;;                                          try-expand-all-abbrevs
;;                                          try-expand-list
;;                                          try-expand-line
;;                                          try-complete-lisp-symbol-partially
;;                                          try-complete-lisp-symbol))
;; ;; use hippie-expand instead of dabbrev
;; (global-set-key (kbd "M-/") #'hippie-expand)
;; (global-set-key (kbd "s-/") #'hippie-expand)

(if (string-equal system-type "darwin")
    (if (display-graphic-p)
	(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))))
(if (string-equal system-type "darwin")
    (if (display-graphic-p)
	(add-to-list 'exec-path "/usr/local/bin/")))

(setq make-backup-files nil)
(setq auto-save-default nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
;; (setq-default-coding-systems 'utf-8)
;; (setq-terminal-coding-systems 'utf-8)
;; (setq-keyboard-coding-systems 'utf-8)

(global-set-key (kbd "RET") 'newline-and-indent)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 20)
(setq recentf-max-saved-items 100)
(setq recentf-auto-cleanup 'never)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'winner)
(winner-mode 1)

(require 'undo-tree)
(global-undo-tree-mode)
(with-eval-after-load 'undo-tree
  (setq undo-tree-auto-save-history nil))

(require 'paren)
(show-paren-mode +1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
;; rename after killing uniquified
(setq uniquify-after-kill-buffer-p t)
;; don't muck with special buffers
(setq uniquify-ignore-buffers-re "^\\*")

(require 'windmove)
(windmove-default-keybindings)

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

(require 'imenu-list)
(setq imenu-list-auto-resize t)
(setq imenu-list-focus-after-activation t)
(global-set-key (kbd "C-c t i") 'imenu-list-smart-toggle)

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
(setq projectile-completion-system 'ivy)

(require 'better-defaults)

(require 'evil)
(setq evil-want-C-i-jump nil)
(setq evil-default-state 'normal)
(evil-mode 1)
(modify-syntax-entry ?_ "w")
(loop for (mode . state) in '((xref--xref-buffer-mode . emacs)
			      (special-mode . emacs)
			      (shell-mode . emacs)
			      (ztree-mode . emacs)
			      (term-mode . emacs))
      do (evil-set-initial-state mode state))
(define-key evil-normal-state-map (kbd "\\") 'ace-jump-mode)
(define-key evil-visual-state-map (kbd "\\") 'ace-jump-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-normal-state-map (kbd "s") 'save-buffer)
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
;; (define-key evil-normal-state-map (kbd "TAB") 'other-window)

;; (require 'pyim)
;; (require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
;; (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
;; (setq default-input-method "pyim")
;; (setq pyim-default-scheme 'microsoft-shuangpin)
;; (setq-default pyim-punctuation-half-width-functions
;;               '(pyim-probe-punctuation-line-beginning
;;                 pyim-probe-punctuation-after-punctuation))
;; ;; 开启拼音搜索功能
;; (pyim-isearch-mode 1)
;; (global-set-key (kbd "C-\\") 'toggle-input-method)
;; (define-key pyim-mode-map "." 'pyim-page-next-page)
;; (define-key pyim-mode-map "," 'pyim-page-previous-page)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'electric)
(electric-pair-mode t)
(electric-indent-mode t)

(defun my/toggle-paste ()
  (interactive)
  (message "Toggle paste")
  (setq electric-pair-mode (if (eq electric-pair-mode t) nil t))
  (setq electric-indent-mode (if (eq electric-indent-mode t) nil t)))
(global-set-key (kbd "C-c t p") 'my/toggle-paste)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(custom-set-faces
 ;; other faces
 '(magit-diff-added ((((type tty)) (:foreground "green"))))
 '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
 '(magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
 '(magit-section-highlight ((((type tty)) nil))))


(require 'yasnippet)
(yas-global-mode 1)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-show-numbers t)
(global-set-key (kbd "C-c y") 'company-yasnippet)
(setq company-backends
      '((company-abbrev company-dabbrev)
        (company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         company-yasnippet)))
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
  (column-number-mode t)
  (setq display-line-numbers-current-absolute nil)
  (setq display-line-numbers 'relative)
  (line-number-mode t))
(add-hook 'prog-mode-hook 'my/prog-mode)

(defun my/c-mode ()
  (setq indent-tabs-mode t)
  (setq tab-width 8)
  (setq c-basic-offset 8))
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
  (setq line-number-mode -1)
  (setq column-number-mode -1)
  (setq comint-prompt-read-only t))
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
(setq markdown-command "pandoc --toc -N --mathjax -s")

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

(require 'org-download)
(setq org-download-method 'directory)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(defun my/org-mode ()
  (column-number-mode t)
  (line-number-mode t)
  ;; (define-key evil-motion-state-map (kbd "C-i") 'org-cycle)
  (flyspell-mode -1)
  (toggle-truncate-lines -1))
(add-hook 'org-mode-hook 'my/org-mode)

(defun my/markdown-mode ()
  (markdown-toggle-math)
  (column-number-mode t)
  (line-number-mode t))
  ;; (markdown-toggle-fontify-code-blocks-natively)
(add-hook 'markdown-mode-hook 'my/markdown-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finial init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'projectile-mode)
(diminish 'undo-tree-mode)
(diminish 'company-mode)
(diminish 'ivy-mode)
(diminish 'eldoc-mode)
(diminish 'yas-minor-mode)
;; (diminish 'pyim-isearch-mode)


(provide 'init)

;; custome below
