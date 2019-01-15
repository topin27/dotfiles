;;; init --- Initialize.

;;; Commentary:
;; 

;;; Code:

;; (add-to-list 'load-path (expand-file-name "lisp" "~/.emacs.d/"))

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages")
			 ("gnu-china" . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("melpa-china" . "http://elpa.emacs-china.org/melpa/")
 			 ("marmalade-china" . "http://elpa.emacs-china.org/marmalade/")
			 ("marmalade" . "http://elpa.emacs-china.org/marmalade/")))

(package-initialize)

(require 'cl)

(defvar my/packages '(
		      diminish
		      ace-jump-mode
		      undo-tree
		      projectile
		      ztree
		      smex
		      ido-vertical-mode
		      idomenu
		      imenu-list
		      evil
		      evil-surround
		      general
		      wgrep
		      hungry-delete
		      rainbow-delimiters
		      clean-aindent-mode
		      yasnippet
		      company
		      cython-mode
		      scala-mode
		      markdown-mode
		      ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(setq user-full-name "Yang Tianping")
(setq user-mail-address "yangtianpingytp@163.com")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-font-lock-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode t)
(line-number-mode t)

(show-paren-mode t)
(setq show-paren-style 'parentheses)

(delete-selection-mode t) ;; inserting text while the mark is active causes the selected text to be deleted first
(transient-mark-mode 1)
(setq select-enable-clipboard t)
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; (setq cursor-type 'bar)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(if (not (display-graphic-p))
    (progn
      ;; (xterm-mouse-mode t)
      (menu-bar-mode -1))
  (progn
    (load-theme 'deeper-blue)
    ;; (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(which-function-mode -1)
(add-hook 'prog-mode-hook 'linum-mode)

(require 'diminish)
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "hungry-delete" '(diminish 'hungry-delete-mode))
;; (diminish 'projectile-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (cond
;;  ((string-equal system-type "gnu/linux")
;;   (setenv "PATH" (concat "~/.local/bin" ":" (getenv "PATH"))))
;;  ((string-equal system-type "darwin")
;;   (setenv "PATH" (concat "~/Library/Python/2.7/bin" ":" "/usr/local/bin" ":" (getenv "PATH")))))

;; (cond
;;  ((string-equal system-type "gnu/linux")
;;   (add-to-list 'exec-path "~/.local/bin"))
;;  ((string-equal system-type "darwin")
;;   (add-to-list 'exec-path "~/Library/Python/2.7/bin/")
;;   (add-to-list 'exec-path "/usr/local/bin/")))

(setq make-backup-files nil)
(setq auto-save-default nil)

(winner-mode 1)

;; (setq default-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-auto-revert-mode 1)

(require 'dired)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'alway)
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)) ;; lazy-load
(setq dired-dwim-target t)

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'projectile)
(projectile-mode +1)
(setq projectile-enable-caching t)
;; (add-to-list 'projectile-globally-ignored-directories "venv")
;; (add-to-list 'projectile-globally-ignored-files ".pyc")
(setq projectile-globally-ignored-files (append '(".pyc" ".class" "~" ".cache") projectile-globally-ignored-files))

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(setq smex-save-file (expand-file-name "smex-items" user-emacs-directory))

(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-everywhere t)
;; (setq ido-auto-merge-work-directories-length -1)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
; (setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)

(require 'idomenu)

(require 'ztree)

(require 'imenu-list)
(setq imenu-list-auto-resize t)

(setq evil-want-C-i-jump nil)
(require 'evil)
(setq evil-default-state 'normal)
(evil-mode 1)
(modify-syntax-entry ?_ "w")
(loop for (mode . state) in '((org-mode . normal)
			      (prog-mode . normal)
			      (shell-mode . emacs)
			      (term-mode . emacs))
      do (evil-set-initial-state mode state))
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'wgrep)
(setq wgrep-auto-save-buffer t)

(require 'hungry-delete)
(global-hungry-delete-mode)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'electric)
(electric-pair-mode t)
(electric-indent-mode t)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

(require 'yasnippet)
(setq yas-snippet-dirs
      '(;; "~/.emacs.d/snippets"                 ;; personal snippets
        ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
	"~/.emacs.d/site-lisp/yasnippet-snippets/snippets" ;; the yasnippet-snippets collection
        ))
(yas-global-mode 1)
(yas-minor-mode-on)
;; (add-hook 'prog-mode-hook #'yas-minor-mode)

(require 'company)
(setq company-show-numbers t
      company-idle-delay 0.1)
(eval-after-load "company"
  '(setq company-backends (delete 'company-eclim (delete 'company-xcode company-backends))))
;; (eval-after-load "company"
(add-hook 'after-init-hook 'global-company-mode)
;;   '(setq company-backends
;; 	 '(company-dabbrev
;; 	    company-dabbrev-code)
;; 	   (company-files
;; 	    company-keywords
;; 	    company-capf
;; 	    company-etags))))
(require 'company-yasnippet)

(require 'cython-mode)
(setq auto-mode-alist
      (cons '(".pyx" . cython-mode) auto-mode-alist))

(require 'scala-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(windmove-default-keybindings)

(require 'general)
(general-create-definer my-leader-def
			:prefix "\\")
(my-leader-def 'normal
	       "f" 'find-file
	       "b" 'ido-switch-buffer
	       "d" 'ido-dired
	       "k" 'ido-kill-buffer
	       "r" 'recentf-open-files
	       "p" 'projectile-switch-project
	       "o" 'find-file-other-window
	       "g g" 'xref-find-definitions
	       "g b" 'xref-pop-marker-stack
	       "g c" 'xref-find-references)

(general-create-definer my-local-leader-def
			;; :prefix my-local-leader
			  :prefix ",")
(my-local-leader-def 'normal
		     "f" 'projectile-find-file
		     "b" 'projectile-switch-to-buffer
		     "p" 'projectile-switch-open-project
		     "o" 'projectile-find-file-other-window
		     "k" 'projectile-kill-buffers
		     "j i" 'idomenu)
(general-define-key
 :prefix "C-c"
 "SPC" 'ace-jump-mode
 "p" 'projectile-command-map
 "j i" 'idomenu
 "o c" 'org-capture
 "o a" 'org-agenda
 "o l" 'org-store-link
 "C-c M-x" 'execute-extended-command  ;; old M-x
 "y" 'company-yasnippet)

(general-define-key
 "M-x" 'smex
 "M-X" 'smex-major-mode-commands
 "M-/" 'hippie-expand
 "C-x C-b" 'ibuffer
 "<f10>" 'rename-buffer
 "<escape>" 'keyboard-escape-quit
 "C-x C-r" 'recentf-open-files
 "C-\\" 'imenu-list-smart-toggle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(add-hook 'org-mode-hook
    (lambda ()
      (flyspell-mode -1)
      (toggle-truncate-lines -1)))
(setq org-html-postamble nil)
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)"))
      org-todo-keyword-faces '(("DOING" . (:foreground "cyan" :weight bold))))
(setq org-src-fontify-natively t)
(setq org-agenda-files '("~/notes"))

;; (require 'ox-publish)
;; (setq org-publish-project-alist
;;       '(
;;         ("org-notes"
;;          :base-directory "~/Workspace/wiki/"
;;          :base-extension "org"
;;          :publishing-directory "~/Workspace/src/topin27.github.io/"
;;          :recursive t
;;          ;; :publishing-function org-publish-org-to-html
;; 	 :publishing-function org-html-publish-to-html
;;          :headline-levels 4             ; Just the default for this project.
;;          :auto-preamble t
;;          )
;;         ("org-static"
;;          :base-directory "~/Workspace/wiki/static/"
;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
;;          :publishing-directory "~/Workspace/src/topin27.github.io/static/"
;;          :recursive t
;;          :publishing-function org-publish-attachment
;;          )
;;         ("org" :components ("org-notes" "org-static"))
;; 	))

(provide 'init)
