;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 主题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'monokai t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 补全
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-company-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'recentf)
(setq recentf-mode 1)
(setq recentf-max-saved-items 20)
(add-to-list 'recentf-keep 'file-remote-p)  ;; 不要去检查远程文件


(provide 'init)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (yasnippet smex monokai-theme company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
