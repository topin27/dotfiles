;; 这里放置和编码相关的配置或者包

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default tab-width 8)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq linum-mode 1)
	    (setq which-function-mode 1)))

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

;; xcscope & helm-cscope

(add-hook 'c-mode-common-hook 'helm-cscope-mode)
(add-hook 'helm-cscope-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'helm-cscope-find-global-definition)
            (local-set-key (kbd "M-@") 'helm-cscope-find-calling-this-function)
            (local-set-key (kbd "M-s") 'helm-cscope-find-this-symbol)
	    ;; (local-set-key (kbd "M-,") 'helm-cscope-pop-mark)))
            (local-set-key (kbd "M-*") 'helm-cscope-pop-mark)))

;; flycheck

;; (add-hook 'after-init-hook #'global-flycheck-mode)


(provide 'init-dev)
