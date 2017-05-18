(setq user-full-name "Yang Tianping")
(setq user-mail-address "yangtianpingytp@163.com")

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode 1)

(global-font-lock-mode t)  ;; 开启语法高亮
(setq default-directory "~/Workspace") ;; 默认工作目录
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x C-b") 'ibuffer)
;; (setq ibuffer-use-other-window t)
(setq-default tab-width 8)

(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))

(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; (display-time-mode 1)
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)

(delete-selection-mode t)  ;; 选中区域时输入将覆盖选中区域
(transient-mark-mode t)  ;; 高亮显示区域选择
(setq x-select-enable-clipboard t)
(setq fram-title-format "zhj@%b")

;; (setq column-number-mode t)
;; (setq line-number-mode t)
;; (global-linum-mode -1)

;; (setq-default cursor-type 'bar)
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; (setq inhibit-splash-screen t
;;       initial-scratch-message nil
;;       ;; initial-major-mode 'org-mode  ;; 进入后直接跳到org-mode
;;       )

;; This will put empty line markers into the left hand side.
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq scroll-margin 3
      scroll-conservatively 10000)

(global-set-key (kbd "RET") 'newline-and-indent)

(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)
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

;; (defun kill-default-buffer ()
;;   "Kill the currently active buffer -- set to C-x k so that users are not asked which buffer they want to kill."
;;   (interactive)
;;   (let (kill-buffer-query-functions) (kill-buffer)))
;; (global-set-key (kbd "C-x k") 'kill-default-buffer)

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
(global-set-key (kbd "M-o") 'prelude-smart-open-line)

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "M-O") 'prelude-smart-open-line-above)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(load-theme 'misterioso)
(set-cursor-color "#ffffff")

(setq debug-on-error nil)

(defun my/set-point-to-register()
  "临时设置记号点，由my/jump-back-to-point跳回"
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))
(global-set-key (kbd "C-.") 'my/set-point-to-register)
(defun my/jump-back-to-point()
  "跳回由my/set-point-to-register设置的记号点"
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))
(global-set-key (kbd "C-,") 'my/jump-back-to-point)

;; (defun my/go-to-char (n char)
;;   "跳至一行的某一个字符"
;;   (interactive "p\ncGo to char: ")
;;   (search-forward (string char) nil nil n)
;;   (while (char-equal (read-char) char)
;;     (search-forward (string char) nil nil n))
;;   (setq unread-command-events (list last-input-event)))
;; (define-key global-map (kbd "C-c a") 'my/go-to-char)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hippie-expand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-daabrev
	try-expand-daabrev-visible
	try-expand-daabrev-all-buffers
	try-expand-daabrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	;; try-complete-lisp-symbol-partially
	;; try-complete-lisp-symbol
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; speedbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(setq speedbar-use-images nil)
(setq speedbar-tag-hierarchy-method nil)

(global-set-key (kbd "<f5>") (lambda()
                               (interactive)
                               (speedbar)))


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
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)

(helm-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-jump-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'autopair)
(autopair-global-mode)


(provide 'init-basic)
