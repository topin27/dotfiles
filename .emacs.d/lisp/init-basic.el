(setq user-full-name "Yang Tianping")
(setq user-mail-address "yangtianpingytp@163.com")

(menu-bar-mode 1)
(tool-bar-mode 1)
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

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(delete-selection-mode t)
(transient-mark-mode t)  ;; 高亮显示区域选择
(setq x-select-enable-clipboard t)
(setq fram-title-format "zhj@%b")

;; (setq column-number-mode t)
;; (setq line-number-mode t)
;; (global-linum-mode -1)

(setq-default cursor-type 'bar)
(setq make-backup-files nil)
(setq auto-save-default nil)

(global-hl-line-mode 1)
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
;; smex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)
(ido-mode +1)


(provide 'init-basic)
