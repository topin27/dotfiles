(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-font-lock-mode t)
;; (setq default-directory "~/Workspace")
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

(setq-default tab-width 8)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq column-number-mode t)))
(add-hook 'prog-mode-hook 'which-function-mode)

(provide 'init-ui)
