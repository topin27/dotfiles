;; here is for org-mode

(add-hook 'org-mode-hook '(lambda ()
                            (flyspell-mode 1)  ;; turn on flyspell-mode by default
			    (setq truncate-lines nil)
			    (setq linum-mode 1)
                            ;; C-TAB for expanding
                            ;; (local-set-key (kbd "C-<tab>")
                            ;;                'yas/expand-from-trigger-key)
                            (local-set-key (kbd "C-c s e")
                                           'org-edit-src-code)
                            (local-set-key (kbd "C-c s i")
                                           'org-insert-src-block)
                            ))

(setq org-html-postamble nil)
