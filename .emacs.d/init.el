;;; init --- Initialize.

;;; Commentary:
;; 

;;; Code:

;; (add-to-list 'load-path (expand-file-name "lisp" "~/.emacs.d/"))

(require 'package)
(setq package-archives '(("gnu-china" . "http://elpa.emacs-china.org/gnu/")
			 ("melpa-china" . "http://elpa.emacs-china.org/melpa/")
			 ("marmalade" . "http://elpa.emacs-china.org/marmalade/")))

(package-initialize)

(require 'cl)

(defvar my/packages '(
		      diminish
		      ace-jump-mode
		      undo-tree
		      projectile
		      ztree
		      imenu-list
		      evil
		      evil-leader
		      evil-surround
		      wgrep
		      rainbow-delimiters
		      clean-aindent-mode
		      yasnippet
		      yasnippet-snippets
		      cython-mode
		      markdown-mode
		      magit
		      company
		      neotree
		      helm
		      helm-projectile
		      helm-cscope
		      helm-ag
		      pyim
		      elfeed
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
    ;; (load-theme 'deeper-blue)
    ;; (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(which-function-mode -1)
(linum-mode -1)
(column-number-mode -1)
(line-number-mode -1)

(set-face-foreground 'linum "#362E2E")
;; (setq linum-format "%d ")

(require 'diminish)
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "company" '(diminish 'company-mode))
(diminish 'helm-mode)
(eval-after-load "helm-cscope" '(diminish 'helm-cscope-mode))
;; (diminish 'projectile-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (cond
;;  ((string-equal system-type "gnu/linux")
;;   (setenv "PATH" (concat "~/.local/bin" ":" (getenv "PATH"))))
;;  ((string-equal system-type "darwin")
;;   (setenv "PATH" (concat "~/Library/Python/2.7/bin" ":" "/usr/local/bin" ":" (getenv "PATH")))))
(if (string-equal system-type "darwin")
    (if (display-graphic-p)
	(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))))

;; (cond
;;  ((string-equal system-type "gnu/linux")
;;   (add-to-list 'exec-path "~/.local/bin"))
;;  ((string-equal system-type "darwin")
;;   (add-to-list 'exec-path "~/Library/Python/2.7/bin/")
;;   (add-to-list 'exec-path "/usr/local/bin/")))
(if (string-equal system-type "darwin")
    (if (display-graphic-p)
	(add-to-list 'exec-path "/usr/local/bin/")))

(setq make-backup-files nil)
(setq auto-save-default nil)

(winner-mode 1)

(windmove-default-keybindings)

;; (setq default-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-auto-revert-mode 1)

(setq evil-want-C-i-jump nil)
(require 'evil)
(setq evil-default-state 'emacs)
(evil-mode 1)
(modify-syntax-entry ?_ "w")
(loop for (mode . state) in '((org-mode . normal)
			      (markdown-mode . normal)
			      (prog-mode . normal)
			      (xref--xref-buffer-mode . emacs)
			      (shell-mode . normal)
			      (term-mode . emacs))
      do (evil-set-initial-state mode state))
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(require 'evil-leader)
(evil-leader/set-leader ",")
(global-evil-leader-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f10>") 'rename-buffer)
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)

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
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'ztree)

(require 'imenu-list)
(setq imenu-list-auto-resize t)
(setq imenu-list-focus-after-activation t)
(global-set-key (kbd "C-c t i") 'imenu-list-smart-toggle)

(require 'wgrep)
(setq wgrep-auto-save-buffer t)

(require 'neotree)
(setq neo-smart-open t)
(global-set-key (kbd "C-c t n") 'neotree-toggle)

(require 'helm)
(require 'helm-config)
(helm-mode 1)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source    -1 ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)
(set-face-attribute 'helm-selection nil
		    :background "purple"
		    :foreground "black")
(defun my/helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook
          'my/helm-hide-minibuffer-maybe)
;; (setq helm-autoresize-max-height 0)
;; (setq helm-autoresize-min-height 20)
;; (helm-autoresize-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h i") 'helm-semantic-or-imenu)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h b") 'helm-resume)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h /") 'helm-do-ag)
(global-set-key (kbd "C-c h m") 'helm-man-woman)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

(require 'projectile)
(projectile-mode +1)
(setq projectile-enable-caching t)
(setq-default projectile-globally-ignored-files
	      (append '(".pyc" ".class" "~" ".cache") projectile-globally-ignored-files))
(setq-default projectile-globally-ignored-directories
	      (append '("__pycache__") projectile-globally-ignored-directories))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(require 'pyim)
(require 'pyim-basedict)
(pyim-basedict-enable)
(setq default-input-method "pyim")
(setq pyim-default-scheme 'quanpin)
(setq pyim-page-tooltip 'popup)
(setq pyim-page-length 9)
(global-set-key (kbd "C-\\") 'toggle-input-method)
(global-set-key (kbd "M-c") 'toggle-input-method)

(require 'elfeed)
(global-set-key (kbd "C-x w") 'elfeed)
(setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "http://planet.emacsen.org/atom.xml"
	"feed://www.ruanyifeng.com/blog/atom.xml"
	"https://www.byvoid.com/zhs/feed"
	"https://coolshell.cn/feed"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'electric)
(electric-pair-mode t)
(electric-indent-mode t)
(defun my/toggle-paste ()
  (interactive)
  (setq electric-pair-mode (if (eq electric-pair-mode t) nil t))
  (setq electric-indent-mode (if (eq electric-indent-mode t) nil t)))
(global-set-key (kbd "C-c t p") 'my/toggle-paste)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

(require 'yasnippet)
;; (setq yas-snippet-dirs
;;       '(;; "~/.emacs.d/snippets"                 ;; personal snippets
;;         ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
;; 	;; "~/.emacs.d/site-lisp/yasnippet-snippets/snippets" ;; the yasnippet-snippets collection
;;         ))
(yas-global-mode 1)
;; (yas-reload-all)
;; (add-hook 'prog-mode-hook #'yas-minor-mode)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
;; (global-set-key (kbd "C-c y") 'company-yasnippet)
;; (setq company-backends '(company-dabbrev-code company-keywords company-semantic company-capf company-files (company-dabbrev company-yasnippet)))
(setq company-dabbrev-downcase nil)
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
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))

(require 'cython-mode)
(setq auto-mode-alist
      (cons '(".pyx" . cython-mode) auto-mode-alist))

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'helm-cscope)


(defun my/prog-mode ()
  (linum-mode t)
  (column-number-mode t)
  (line-number-mode t)
  (evil-leader/set-key "g g" 'xref-find-definitions)
  (evil-leader/set-key "g b" 'xref-pop-marker-stack)
  (evil-leader/set-key "g c" 'xref-find-references))
(add-hook 'prog-mode-hook 'my/prog-mode)

(defun my/c-common-mode ()
  (setq indent-tabs-mode t)
  (setq tab-width 8)
  (setq c-basic-offset 8)
  (evil-leader/set-key "g g" 'helm-cscope-find-global-definition-no-prompt)
  (evil-leader/set-key "g b" 'helm-cscope-pop-mark)
  (evil-leader/set-key "g c" 'helm-cscope-find-calling-this-function))
(add-hook 'c-mode-common-hook 'my/c-common-mode)

(defun my/java-mode ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (evil-leader/set-key "g g" 'helm-cscope-find-global-definition-no-prompt)
  (evil-leader/set-key "g b" 'helm-cscope-pop-mark)
  (evil-leader/set-key "g c" 'helm-cscope-find-calling-this-function))
(add-hook 'java-mode-hook 'my/java-mode)

(defun my/python-mode ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4))
(add-hook 'python-mode-hook 'my/python-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(setq org-html-postamble nil)
;; (setq org-startup-indented t)
(setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)" "CANCELLED(c)"))
      org-todo-keyword-faces '(("DOING" . (:foreground "cyan" :weight bold))
			       ("CANCELLED" . (:foreground "yellow" :weight bold))))
(setq org-src-fontify-natively t)
(setq org-directory "~/Workspace/notes/")
(setq org-agenda-files (list org-directory))
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/gtd.org") "Tasks")
	 "* TODO %?\n %i\n")
	("n" "Note" plain (file (concat org-directory "/notes.org"))
	 "* %?\n %x\n")
	("l" "Link" plain (file (concat org-directory "/links.org"))
	 "- %?\n %x\n")))
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o s") 'org-store-link)

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

(defun my/org-mode ()
  (linum-mode t)
  (column-number-mode t)
  (line-number-mode t)
  (define-key evil-motion-state-map (kbd "C-i") 'org-cycle)
  (flyspell-mode -1)
  (toggle-truncate-lines -1))
(add-hook 'org-mode-hook 'my/org-mode)

(defun my/markdown-mode ()
  (markdown-toggle-math)
  (linum-mode t)
  (column-number-mode t)
  (line-number-mode t)
  (markdown-toggle-url-hiding)
  (markdown-toggle-fontify-code-blocks-natively)
  (define-key evil-motion-state-map (kbd "C-i") 'markdown-cycle))
(add-hook 'markdown-mode-hook 'my/markdown-mode)

(provide 'init)

;;; Auto-generated code below
