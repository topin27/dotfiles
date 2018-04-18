;; 和界面显示相关的在这里定义

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode -1)
(line-number-mode t)
(column-number-mode t)

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
    (xterm-mouse-mode t))

(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "autopair" '(diminish 'autopair-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))

(provide 'init-ui)
