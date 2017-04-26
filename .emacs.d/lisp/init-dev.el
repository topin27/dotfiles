(which-function-mode t) ;; 实时显示当前所在的函数

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

(add-hook 'prog-mode-hook
	  'linum-mode
	   'hl-line-mode)

(add-hook 'prog-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "M-*") 'pop-tag-mark)
	     (local-set-key (kbd "M-.") 'find-tag)))

(setq-default
 whitespace-line-column 81
 whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'global-company-mode)


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
;; yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'yasnippet)
(yas-global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anaconda and anaconda-company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xcscope
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (require 'xcscope)
	     (cscope-setup)
	     (cscope-minor-mode t)))
;; (add-hook 'c-mode-common-hook
;; 	  '(lambda ()
;; 	     (local-set-key (kbd "M-r") 'cscope-find-functions-calling-this-function)))


(provide 'init-dev)
