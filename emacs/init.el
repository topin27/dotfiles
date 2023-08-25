;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))
(setq use-package-always-ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

(delete-selection-mode t) ;; inserting text while the mark is active causes the
                          ;; selected text to be deleted first
;; (transient-mark-mode 1)
(setq select-enable-clipboard t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(menu-bar-mode -1)

;; (setq cursor-type 'bar)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(which-function-mode -1)
(setq mac-command-modifier 'control)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(defun my/prog-mode ()
  (column-number-mode t)
  (setq display-line-numbers-current-absolute nil)
  (setq display-line-numbers 'relative)
  (line-number-mode t))
(add-hook 'prog-mode-hook 'my/prog-mode)
(add-hook 'text-mode-hook 'my/prog-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(setq make-backup-files nil)
(setq auto-save-default nil)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
;; (setq-default-coding-systems 'utf-8)
;; (setq-terminal-coding-systems 'utf-8)
;; (setq-keyboard-coding-systems 'utf-8)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(if (string-equal system-type "darwin")
    (if (display-graphic-p)
	(setenv "PATH" (concat "~/bins/default/bin" ":" "/usr/local/bin" ":" (getenv "PATH")))))
(if (string-equal system-type "darwin")
    (if (display-graphic-p)
	(add-to-list 'exec-path "~/bins/default/bin")))

(use-package recentf
  :init
  (setq recentf-max-menu-items 20)
  (setq recentf-max-saved-items 100)
  (setq recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

(use-package ibuffer
  :bind ("C-x C-b" . 'ibuffer))

(use-package winner
  :config
  (winner-mode 1))

(use-package undo-tree
  :diminish
  :init
  (setq undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package dirvish
  :config
  (dirvish-override-dired-mode))

(use-package ace-jump-mode
  :bind ("C-c SPC" . 'ace-jump-mode))

(use-package evil
  :init
  (setq evil-want-C-i-jump nil)
  (setq evil-default-state 'normal)
  (modify-syntax-entry ?_ "w")
  :config
  (evil-mode 1)
  :custom
  (loop for (mode . state) in '((prog-mode . normal)
				(c++-mode . normal))
	do (evil-set-initial-state mode state))
  :bind (:map evil-normal-state-map
	      ("\\" . 'ace-jump-mode)
	      ("s" . 'save-buffer)
	      ("M-." . 'xref-find-definitions)
	      ("g ]" . 'xref-find-definitions)
	      ("C-t" . 'xref-go-back)
	 :map evil-visual-state-map
	      ("\\" . 'ace-jump-mode)))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package ivy
  :diminish
  :hook (after-init . ivy-mode)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  :bind
  ("C-c u" . ivy-resume))

(use-package counsel
  :after ivy
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c r f". counsel-recentf)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-h o" . counsel-describe-symbol)
  ("C-h l" . counsel-find-library)
  ("C-c h i" . counsel-semantic-or-imenu)
  (:map minibuffer-local-map
	("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-file-preview
  :after ivy
  :config
  (ivy-file-preview-mode))

(use-package ivy-rich
  :after ivy
  :init
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (ivy-rich-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package citre
;;   :defer t
;;   :init
;;   ;; This is needed in `:init' block for lazy load to work.
;;   (require 'citre-config)
;;   ;; Bind your frequently used commands.  Alternatively, you can define them
;;   ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
;;   (global-set-key (kbd "C-x c j") 'citre-jump)
;;   (global-set-key (kbd "C-x c J") 'citre-jump-back)
;;   (global-set-key (kbd "C-x c p") 'citre-ace-peek)
;;   (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
;;   :config
;;   (setq
;;    ;; Set this if you want to always use one location to create a tags file.
;;    citre-default-create-tags-file-location 'global-cache
;;    ;; See the "Create tags file" section above to know these options
;;    citre-use-project-root-when-creating-tags t
;;    citre-prompt-language-for-ctags-command t
;;    ;; By default, when you open any file, and a tags file can be found for it,
;;    ;; `citre-mode' is automatically enabled.  If you only want this to work for
;;    ;; certain modes (like `prog-mode'), set it like this.
;;    citre-auto-enable-citre-mode-modes '(prog-mode)))

(use-package imenu-list
  :init
  (setq imenu-list-auto-resize t)
  (setq imenu-list-focus-after-activation t)
  :bind
  ("C-c t i" . imenu-list-smart-toggle))

(use-package company
  :diminish
  :init
  (setq company-idle-delay 0.1)
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase nil)
  (setq company-backends
	'((company-abbrev company-dabbrev)
	  (company-files       ; files & directories
	   company-keywords    ; keywords
	   company-capf
	   company-yasnippet)))
  :hook ((prog-mode . company-mode)
	 (text-mode . company-mode))
  :custom
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(company-preview ((t (:foreground "darkgray" :underline t))))
   '(company-preview-common ((t (:inherit company-preview))))
   '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
   '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
   '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))))

(use-package yasnippet
  :diminish
  :after company
  :init
  (yas-global-mode 1))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(markdown-mode ag yasnippet company magit imenu-list ivy-rich ivy-file-preview ivy-xref counsel ivy evil-surround evil ace-jump-mode dirvish undo-tree)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
