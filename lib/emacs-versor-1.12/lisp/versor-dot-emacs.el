;;;; versor-dot-emacs.el -- example setup file for versor
;;; Time-stamp: <2006-03-09 14:52:36 john>

(add-to-list 'load-path "/home/john/common/open-projects/emacs-versor/lisp/" t)

(require 'versor)

(versor-setup 'arrows 'arrows-misc 'modal 'text-in-code 'menu)

(setq versor-mode-current-levels 
      (mapcar 'versor-mode-levels-triplet
	      '((emacs-lisp-mode "structural" "exprs")
		(lisp-interaction-mode "structural" "exprs")
		(c-mode "program" "statement-parts")
		(text-mode "cartesian" "lines")
		(html-helper-mode "text" "words"))))

;;; end of versor-dot-emacs.el
