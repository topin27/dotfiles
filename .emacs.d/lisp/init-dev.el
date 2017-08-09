(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq column-number-mode t)))
(add-hook 'prog-mode-hook 'which-function-mode)
;; (add-hook 'prog-mode-hook 'hl-line-mode)

;; 从第81列开始高亮
;; (setq-default
;;  whitespace-line-column 81
;;  whitespace-style       '(face lines-tail))
;; (add-hook 'prog-mode-hook #'whitespace-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'after-init-hook 'global-company-mode)
(add-hook 'prog-mode-hook 'company-mode)
(setq company-idle-delay 0.2)
(setq company-show-numbers t)
(setq company-minimum-prefix-length 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'after-init-hook #'global-flycheck-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clean-aindent-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'undo-tree)
(global-undo-tree-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'projectile)
;; (projectile-global-mode)
(add-hook 'prog-mode-hook 'projectile-mode)
(add-hook 'org-mode-hook 'projectile-mode)


;;=============================
;; For C dev
;;=============================

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq c-default-style "linux" ; set style to "linux"
      c-basic-offset 8)

(setq gdb-many-windows t        ; use gdb-many-windows by default
      gdb-show-main t)          ; Non-nil means display source file containing the main routine at startup

(add-hook 'c-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends) '((company-clang company-semantic company-dabbrev-code)
							   company-dabbrev)))
	  )


;;=============================
;; For Python dev
;;=============================

(add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends) '((company-anaconda company-dabbrev-code)
							   company-dabbrev)))
	  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anaconda and anaconda-company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)


;;=============================
;; For Rust dev
;;=============================

(unless (getenv "RUST_SRC_PATH")
  (setenv "RUST_SRC_PATH" (expand-file-name "/Users/yangtianping/Workspace/src/rust-master/src")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(autoload 'rust-mode "rust-mode" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck-rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; racer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(define-key rust-mode-map (kbd "M-*") 'pop-tag-mark)
(define-key rust-mode-map (kbd "M-?") 'racer-describe)
(setq company-tooltip-align-annotations t)


(provide 'init-dev)
