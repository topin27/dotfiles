;; here is for org-mode

(add-hook 'org-mode-hook
	  (lambda ()
	    (flyspell-mode 1)  ;; turn on flyspell-mode by default
	    (linum-mode t)
	    (line-number-mode t)
	    (column-number-mode t)
	    (which-function-mode -1)
	    ;; C-TAB for expanding
	    ;; (local-set-key (kbd "C-<tab>")
	    ;;                'yas/expand-from-trigger-key)
	    (local-set-key (kbd "C-c s e")
			   'org-edit-src-code)
	    (local-set-key (kbd "C-c s i")
			   'org-insert-src-block)))

(setq org-html-postamble nil)

(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o l") 'org-store-link)

;; (setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)")))

(provide 'init-org)
