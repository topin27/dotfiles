;; 这里放置和编码相关的配置或者包

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default tab-width 8)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq semantic-mode 1)
	    (local-set-key (kbd "C-c C-j") 'idomenu)
	    ))

(setq
 gdb-many-windows t
 gdb-show-main t
 )

(electric-indent-mode -1)


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

;; xcscope

(require 'xcscope)
(cscope-setup)

;; company

(require 'company)

;; (setq company-dabbrev-downcase 0)
;; ;; (setq company-idle-delay 0)  # waste of CPU
;; (define-key company-mode-map (kbd "TAB") 'company-complete)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load "company"
  '(setq company-backends '((company-dabbrev company-dabbrev-code company-yasnippet company-capf) company-files)))
(add-hook 'c-mode-hook
	  (lambda ()
	    (add-to-list 'company-backends '(company-semantic company-clang))
	    (local-set-key (kbd "<f5>") 'cscope-find-global-definition)
	    (local-set-key (kbd "<f6>") 'cscope-pop-mark)))
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (add-to-list 'company-backends '(company-elisp company-etags))))

;; anaconda

;; (add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook
	  (lambda ()
	    (add-to-list 'company-backends 'company-anaconda)
	    ;; (push 'company-anaconda company-backends)
	    (local-set-key (kbd "<f5>") 'anaconda-mode-find-definitions)
	    (local-set-key (kbd "<f6>") 'anaconda-mode-go-back)))

;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

;; sr-speedbar

(global-set-key (kbd "C-\\") 'sr-speedbar-toggle)

(provide 'init-dev)
