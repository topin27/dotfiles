;; 这里放置和编码相关的配置或者包

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default tab-width 8)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq linum-mode 1)
	    (setq line-number-mode 1)
	    (setq semantic-mode 1)
	    (setq which-function-mode 1)
	    (local-set-key (kbd "C-c C-j") 'imenu)
	    ))

(setq
 gdb-many-windows t
 gdb-show-main t
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clean-aindent-mode

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; autopair

(require 'autopair)
(add-hook 'prog-mode-hook 'autopair-mode)

;; company

(add-hook 'prog-mode-hook 'global-company-mode)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends) '((company-semantic company-dabbrev-code)
							   company-capf company-dabbrev))))

;; company-anaconda

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends) '((company-anaconda company-dabbrev-code)
							   company-capf company-dabbrev))))

;; xcscope

(require 'xcscope)
(cscope-setup)

;; flycheck

;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; imenu-list

(global-set-key (kbd "C-\\") #'imenu-list-smart-toggle)


(provide 'init-dev)
