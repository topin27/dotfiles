(setq user-full-name "Yang Tianping")
(setq user-mail-address "yangtianpingytp@163.com")

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode 1)
(ido-mode 1)

(global-font-lock-mode t)  ;; 开启语法高亮
(setq default-directory "~/Workspace") ;; 默认工作目录
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-o") 'ido-find-file)
(global-set-key (kbd "s-O") 'find-file-other-window)
(global-set-key (kbd "s-b") 'ido-switch-buffer)
(global-set-key (kbd "s-B") 'ibuffer)
(cond
 ((string-equal system-type "darwin")
  (global-set-key (kbd "s-g") 'keyboard-quit)))
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
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
;; (setq fram-title-format "zhj@%b")

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

;; (setq scroll-margin 3
;;       scroll-conservatively 10000)

(global-set-key (kbd "RET") 'newline-and-indent)

(setq ediff-diff-options "-w"
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

;; (load-theme 'misterioso)
;; (if (not (display-graphic-p))
;;     (xterm-mouse-mode t)
;;   (set-cursor-color "#ffffff"))
(load-theme 'manoj-dark)
(if (not (display-graphic-p))
    (xterm-mouse-mode t))

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
   
;; (setq speedbar-use-images nil)
;; (setq speedbar-tag-hierarchy-method nil)

;; (global-set-key (kbd "<f5>") (lambda()
;;                                (interactive)
;;                                (speedbar)))


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
;; ace-jump-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'autopair)
(autopair-global-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-vertical-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'evil)
(add-hook 'prog-mode-hook 'evil-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-term
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (custom-set-variables
;;  '(term-default-bg-color "#000000")        ;; background color (black)
;;  '(term-default-fg-color "#dddd00"))       ;; foreground color (yellow)

(setq multi-term-program "/usr/bin/zsh")

(add-hook 'term-mode-hook
	  (lambda ()
	    (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 2048)))
(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            (autopair-mode -1)))
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)))


(provide 'init-basic)
