;; basic dev settings

(setq-default tab-width 8)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq column-number-mode t)))
(add-hook 'prog-mode-hook 'which-function-mode)

;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

;; clean-aindent-mode

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; company

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'c-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends) '((company-semantic company-dabbrev-code)
							   company-capf company-dabbrev))))

;; company-jedi

;; (setq company-backends '((company-dabbrev-code company-jedi)))
(add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends) '((company-jedi company-dabbrev-code)
							   company-capf company-dabbrev))))

;; magit

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'init-dev)
