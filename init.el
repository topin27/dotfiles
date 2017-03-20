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
		      monokai-theme
		      yasnippet
		      ggtags
		      clean-aindent-mode
		      undo-tree
		      ;; helm
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

;; 判断某个包是否已经安装，如果没有则自动从ELPA中安装它（需要联网）
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 基本配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list
 'load-path
 (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode 1)

(global-font-lock-mode t)  ;; 开启语法高亮
(setq default-directory "~/Workspace") ;; 默认工作目录
(fset 'yes-or-no-p 'y-or-n-p)
;; (which-function-mode t)   ;; 实时显示当前所在的函数
;; (setq modelinepos-column-limit 80)

(global-set-key (kbd "C-x C-b") 'ibuffer)
;; (setq ibuffer-use-other-window t)
(setq-default tab-width 8)

(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))

(show-paren-mode t)
(setq show-paren-style 'parentheses)

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(delete-selection-mode t)
(transient-mark-mode t)  ;; 高亮显示区域选择
(setq x-select-enable-clipboard t)
(setq fram-title-format "zhj@%b")
(setq column-number-mode t)
(setq line-number-mode t)
(global-linum-mode t)

(setq-default cursor-type 'bar)
(setq make-backup-files nil)
(setq auto-save-default nil)

(delete-selection-mode 1)
;; (global-hl-line-mode 1)
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(setq user-full-name "Yang Tianping")
(setq user-mail-address "yangtianpingytp@163.com")

(setq inhibit-splash-screen t
      initial-scratch-message nil
      ;; initial-major-mode 'org-mode  ;; 进入后直接跳到org-mode
      )

;; This will put empty line markers into the left hand side.
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq scroll-margin 3
      scroll-conservatively 10000)

(global-set-key (kbd "RET") 'newline-and-indent)

(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))

(defun kill-default-buffer ()
  "Kill the currently active buffer -- set to C-x k so that users are not asked which buffer they want to kill."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))
(global-set-key (kbd "C-x k") 'kill-default-buffer)

;; smart openline
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq c-default-style "linux" ; set style to "linux"
      c-basic-offset 8)

(setq gdb-many-windows t        ; use gdb-many-windows by default
      gdb-show-main t)          ; Non-nil means display source file containing the main routine at startup

;; GROUP: Programming -> Tools -> Ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(global-set-key (kbd "M-o") 'prelude-smart-open-line)
(global-set-key (kbd "M-O") 'prelude-smart-open-line-above)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 主题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'monokai t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'alway)

(put 'dired-find-alternate-file 'disabled nil)

;; 主动加载 Dired Mode
;; (require 'dired)
;; (defined-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; 延迟加载
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'global-company-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'smex)
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'recentf)
(setq recentf-mode 1)
(setq recentf-max-menu-items 20)
(add-to-list 'recentf-keep 'file-remote-p)  ;; 不要去检查远程文件
(global-set-key (kbd "C-x C-r") 'recentf-open-files)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ggtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook
    (lambda ()
      (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
	(ggtags-mode 1))))
(add-hook 'dired-mode-hook 'ggtags-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clean-aindent-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'undo-tree)
(global-undo-tree-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'yasnippet)
(yas-global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'ido)
;; (setq ido-enable-prefix nil
;;       ido-enable-flex-matching t
;;       ido-create-new-buffer 'always
;;       ido-use-filename-at-point 'guess
;;       ido-max-prospects 10
;;       ido-default-file-method 'selected-window
;;       ido-auto-merge-work-directories-length -1)
;; (ido-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)

(helm-mode 1)


(provide 'init)
