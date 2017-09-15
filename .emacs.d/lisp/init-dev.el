;; auto-complete

(ac-config-default)

;; flycheck

;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; clean-aindent-mode

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

(provide 'init-dev)
