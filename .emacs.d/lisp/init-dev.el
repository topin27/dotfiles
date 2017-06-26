;; (which-function-mode t) ;; 实时显示当前所在的函数

(add-hook 'prog-mode-hook 'linum-mode)
;; (add-hook 'prog-mode-hook 'hl-line-mode)

;; 从第81列开始高亮
(setq-default
 whitespace-line-column 81
 whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)


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

;; ;; TODO
;; (add-hook 'c-mode-hook
;; 	  (lambda ()
;; 	     (local-set-key (kbd "M-*") 'pop-tag-mark)
;; 	     (local-set-key (kbd "M-.") 'find-tag))
;; 	  )


;;=============================
;; For Python dev
;;=============================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anaconda and anaconda-company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'global-company-mode)
;; TODO
(add-hook 'c-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends) '((company-clang company-semantic company-dabbrev-code)
							   company-dabbrev)))
	  )
(add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends) '((company-anaconda company-dabbrev-code)
							   company-dabbrev)))
	  )


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


(provide 'init-dev)
