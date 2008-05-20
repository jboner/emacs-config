;;; VERSATILE CURSORS  ------------------------------
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/emacs-versor-1.12/lisp"))

(require 'versor)
(require 'languide)
(require 'flexi-choose)
(require 'versor-voice)
;(require 'pedals)
;(require 'handsfree)
(require 'languide-keymap)
(require 'languide-c-like)
(require 'languide-functional)
(require 'languide-html-like)

;; preset the dimensions for some modes
(setq versor-mode-current-levels
    (mapcar 'versor-mode-levels-triplet
	    '(
	      (scala-mode "structural" "exprs")
	      (emacs-lisp-mode "structural" "exprs")
	      (lisp-interaction-mode "structural" "exprs")
	      (c-mode "program" "statement-parts")
	      (text-mode "cartesian" "lines")
	      (html-helper-mode "text" "words")
	      )))

(versor-setup
  'arrows 'arrows-misc  ;; this makes versor use the arrow keys, rather
                        ;; than the keyboard cluster
  'modal                ;; this keeps separate dimensions for each mode
  'text-in-code         ;; this keeps a different movement system for
                        ;; within comments / string literals, from
                        ;; that for within actual code
  'menu                ;; make a menu available
)
(setq versor-reformat-automatically nil)
(versor-mode true)
