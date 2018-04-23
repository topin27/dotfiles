;; 和界面显示相关的在这里定义

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
    (scroll-bar-mode -1)
    (global-linum-mode -1)
    (line-number-mode -1)
    (column-number-mode -1)))

(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "autopair" '(diminish 'autopair-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "meghanada" '(diminish 'meghanada-mode))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (linum-mode t)
	    (which-function-mode t)))

(provide 'init-ui)
