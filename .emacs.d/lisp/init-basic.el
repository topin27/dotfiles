;; 这里放置除了界面之外的全局都需要用到的包或者配置

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
 ((string-equal system-type "gnu/linux")
  (setenv "PATH" (concat "~/.local/bin" ":" (getenv "PATH")))
  )
 ((string-equal system-type "darwin")
  (setenv "PATH" (concat "~/Library/Python/2.7/bin" ":" (getenv "PATH")))
  )
 )

(setq make-backup-files nil)
(setq auto-save-default nil)

(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "RET") 'newline-and-indent)

;; (defun prelude-smart-open-line (arg)
;;   "Insert an empty line after the current line.
;; Position the cursor at its beginning, according to the current mode.
;; With a prefix ARG open line above the current line."
;;   (interactive "P")
;;   (if arg
;;       (prelude-smart-open-line-above)
;;     (progn
;;       (move-end-of-line nil)
;;       (newline-and-indent))))
;; (global-set-key (kbd "M-o") 'prelude-smart-open-line)

;; (defun prelude-smart-open-line-above ()
;;   "Insert an empty line above the current line.
;; Position the cursor at it's beginning, according to the current mode."
;;   (interactive)
;;   (move-beginning-of-line nil)
;;   (newline-and-indent)
;;   (forward-line -1)
;;   (indent-according-to-mode))
;; (global-set-key (kbd "M-O") 'prelude-smart-open-line-above)

;; (defun my/set-point-to-register()
;;   "临时设置记号点，由my/jump-back-to-point跳回"
;;   (interactive)
;;   (setq zmacs-region-stays t)
;;   (point-to-register 8))
;; (global-set-key (kbd "C-.") 'my/set-point-to-register)

;; (defun my/jump-back-to-point()
;;   "跳回由my/set-point-to-register设置的记号点"
;;   (interactive)
;;   (setq zmacs-region-stays t)
;;   (let ((tmp (point-marker)))
;;     (jump-to-register 8)
;;     (set-register 8 tmp)))
;; (global-set-key (kbd "C-,") 'my/jump-back-to-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dired

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'alway)
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)) ;; lazy-load
(setq dired-dwim-target t)

;; ace-jump-mode

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; undo-tree

(require 'undo-tree)
(global-undo-tree-mode)

;; ido & ido-vertical-mode

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-auto-merge-work-directories-length -1) ;; 禁用ido的自动查找功能（在创建新文件时特烦）

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
; (setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)

;; smex

(require 'smex) ; Not needed if you use package.el
; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
;                   ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; projectile

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

(provide 'init-basic)
