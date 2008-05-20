;;; versor-joystick.el --- joystick interface for Versor

;; Copyright (C) 2007, 2008  John Sturdy

;; Author: John Sturdy <jcgs@hosea>
;; Keywords: convenience, hardware

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Bindings for versor for the gamepad interface.  A few functions are
;; also defined here, some of them for the bulk definition of
;; commands, and some for use from those bulk definitions.

;;; Code:

(require 'joystick)
(require 'versor-tracking)

(defvar versor-joystick-process nil
  "The process versor is using to communicate with a joystick.")

(defun versor-joystick-bindings (device)
  "Make the bindings for the versor joystick setup for DEVICE."
  (versor-joystick-make-wrappers)
  (versor-joystick-make-wrappers "versor-joystick-" "-other-window"
				 'in-other-window 1
				 'other-window-versor-copy
				 'other-window-versor-yank)
  (versor-joystick-make-wrappers "versor-joystick-" "-other-other-window"
				 'in-other-window 2
				 'other-other-window-versor-copy
				 'other-other-window-versor-yank)

  ;; Hat switch to be normal versor movements
  (global-set-key [ Hat0X-previous ] 'versor-prev)
  (global-set-key [ Hat0X-next ] 'versor-next)
  (global-set-key [ Hat0Y-previous ] 'versor-over-prev)
  (global-set-key [ Hat0Y-next ] 'versor-over-next)

  (put 'versor-prev 'joystick-help "<-- v")
  (put 'versor-next 'joystick-help "v -->")
  (put 'versor-over-prev 'joystick-help "v--")
  (put 'versor-over-next 'joystick-help "v++")

  (put 'backward-char 'joystick-help "char--")
  (put 'forward-char 'joystick-help "char++")
  (put 'previous-line 'joystick-help "line--")
  (put 'next-line 'joystick-help "line++")

  ;; left-hand joystick to be normal non-versor movements, but with
  ;; versor-tracking
  (global-set-key [ X-previous ] 'versor-tracking-backward-char)
  (global-set-key [ X-next ] 'versor-tracking-forward-char)
  (global-set-key [ Y-previous ] 'versor-tracking-previous-line)
  (global-set-key [ Y-next ] 'versor-tracking-next-line)

  (put 'versor-tracking-backward-char 'joystick-help "<-- vt")
  (put 'versor-tracking-forward-char 'joystick-help "vt -->")
  (put 'versor-tracking-previous-line 'joystick-help "vt--")
  (put 'versor-tracking-next-line 'joystick-help "vt++")

  (set-other-window-axes "" "versor-tracking-" "-other-window"
			 'versor-other-window-function-setup
			 'backward-char
			 'forward-char
			 'previous-line
			 'next-line)

  (put 'versor-tracking-backward-char-other-window 'joystick-help "<-- vto")
  (put 'versor-tracking-forward-char-other-window 'joystick-help "vto -->")
  (put 'versor-tracking-previous-line-other-window 'joystick-help "vto--")
  (put 'versor-tracking-next-line-other-window 'joystick-help "vto++")

  (set-other-window-axes "BaBt6-" "versor-tracking-" "-other-other-window"
			 'versor-other-other-window-function-setup
			 'backward-char
			 'forward-char
			 'previous-line
			 'next-line)

  (put 'versor-tracking-backward-char-other-other-window
       'joystick-help "<-- vtO")
  (put 'versor-tracking-forward-char-other-other-window
       'joystick-help "vtO -->")
  (put 'versor-tracking-previous-line-other-other-window
       'joystick-help "vtO--")
  (put 'versor-tracking-next-line-other-other-window
       'joystick-help "vtO++")

  (set-other-window-axes "ToBt2-PiBt-"
			 "versor-joystick-" "-other-window"
			 'versor-other-window-function-setup
			 'backward-word
			 'forward-word
			 'backward-sentence
			 'forward-sentence)
  (set-other-window-axes "ToBt2-PiBt-BaBt6-"
			 "versor-joystick-" "-other-other-window"
			 'versor-other-other-window-function-setup
			 'backward-word
			 'forward-word
			 'backward-sentence
			 'forward-sentence)
  (set-other-window-axes "BaBt-BaBt2-"
			 "versor-joystick-" "-other-window"
			 'versor-other-window-function-setup
			 'previous-sexp
			 'next-sexp
			 'backward-up-list
			 'down-list)
  (set-other-window-axes "BaBt-BaBt2-BaBt6-"
			 "versor-joystick-" "-other-other-window"
			 'versor-other-other-window-function-setup
			 'previous-sexp
			 'next-sexp
			 'backward-up-list
			 'down-list)
  (set-other-window-axes "PiBt-BaBt2-"
			 "versor-joystick-" "-other-window"
			 'versor-other-window-function-setup
			 'c-beginning-of-statement
			 'c-end-of-statement
			 'beginning-of-defun
			 'end-of-defun)
  (set-other-window-axes "PiBt-BaBt2-BaBt6-"
			 "versor-joystick-" "-other-other-window"
			 'versor-other-other-window-function-setup
			 'versor-joystick-statement-navigate-parts-previous
			 'versor-joystick-statement-navigate-parts-next
			 'versor-joystick-previous-statement
			 'versor-joystick-next-statement)
  ;; use the lower right button for dimension changes
  (global-set-key [ BaBt2-Hat0X-previous ] 'versor-out)
  (global-set-key [ BaBt2-Hat0X-next ] 'versor-in)
  (global-set-key [ BaBt2-Hat0Y-previous ] 'versor-prev-meta-level)
  (global-set-key [ BaBt2-Hat0Y-next ] 'versor-next-meta-level)
  (global-set-key [ BaseBtn2-up ] 'versor-mark)

  (put 'BaseBtn2-up 'joystick-modifier-help "dimension")
  (put 'BaseBtn2-up 'joystick-help "v-mark")
  (put 'versor-out 'joystick-help "zoom out")
  (put 'versor-in 'joystick-help "zoom in")
  (put 'versor-prev-meta-level 'joystick-help "level--")
  (put 'versor-next-meta-level 'joystick-help "level++")
  (joystick-label-modified "BaBt2" "Dimensions" device)

  ;; use the upper right button for "extend selection"
  (global-set-key [ BaBt-Hat0X-previous ] 'versor-extend-item-backwards)
  (global-set-key [ BaBt-Hat0X-next ] 'versor-extend-item-forwards)
  (global-set-key [ BaBt-Hat0Y-previous ] 'versor-extend-item-backwards-regardless)
  (global-set-key [ BaBt-Hat0Y-next ] 'versor-extend-item-forwards-regardless)
  (global-set-key [ BaseBtn-up ] 'versor-select-surrounding)
  ;; (global-set-key [ BaBt-Hat0Y-previous ] 'versor-other-end-of-item) ; todo: find new home
  ;; (global-set-key [ BaBt-Hat0Y-next ] 'versor-dwim) ; todo: find new home

  (put 'BaseBtn-up 'joystick-modifier-help "extend")
  (put 'versor-extend-item-backwards 'joystick-help "<--extend")
  (put 'versor-extend-item-forwards 'joystick-help "extend-->")
  (put 'versor-other-end-of-item 'joystick-help "other-end")
  (put 'versor-dwim 'joystick-help "dwim")
  (put 'versor-select-surrounding 'joystick-help "surround")
  (joystick-label-modified "BaBt" "Extend" device)

  ;; Now go over some of the ordinary gamepad bindings, but put a
  ;; layer onto them that handles the versor selection drawing
  (global-set-key [ ToBt2-BaBt-Hat0X-previous ]
		  'versor-tracking-backward-char)
  (global-set-key [ ToBt2-BaBt-Hat0X-next ]
		  'versor-tracking-forward-char)
  (global-set-key [ ToBt2-BaBt-Hat0Y-previous ]
		  'versor-tracking-previous-line)
  (global-set-key [ ToBt2-BaBt-Hat0Y-next ]
		  'versor-tracking-next-line)

  (global-set-key [ ToBt2-PiBt-Hat0X-previous ]
		  'versor-joystick-previous-word)
  (global-set-key [ ToBt2-PiBt-Hat0X-next ]
		  'versor-joystick-next-word)
  (global-set-key [ ToBt2-PiBt-Hat0Y-previous ]
		  'versor-joystick-backward-sentence)
  (global-set-key [ ToBt2-PiBt-Hat0Y-next ]
		  'versor-joystick-forward-sentence)

  (global-set-key [ BaBt-BaBt2-Hat0X-previous ]
		  'versor-joystick-previous-sexp)
  (global-set-key [ BaBt-BaBt2-Hat0X-next ]
		  'versor-joystick-next-sexp)
  (global-set-key [ BaBt-BaBt2-Hat0Y-previous ]
		  'versor-joystick-backward-up-list)
  (global-set-key [ BaBt-BaBt2-Hat0Y-next ]
		  'versor-joystick-down-list)

  (global-set-key [ PiBt-BaBt2-Hat0X-previous ]
		  'versor-joystick-statement-navigate-parts-previous)
  (global-set-key [ PiBt-BaBt2-Hat0X-next ]
		  'versor-joystick-statement-navigate-parts-next)
  (global-set-key [ PiBt-BaBt2-Hat0Y-previous ]
		  'versor-joystick-previous-statement)
  (global-set-key [ PiBt-BaBt2-Hat0Y-next ]
		  'versor-joystick-next-statement)

  ;; also, use the yank button as a modifier, for insertions
  (global-set-key [ ToBt-Hat0X-previous ] 'versor-insert-before)
  (global-set-key [ ToBt-Hat0X-next ] 'versor-insert-after)
  (global-set-key [ ToBt-Hat0Y-previous ] 'versor-insert-around)
  (global-set-key [ ToBt-Hat0Y-next ] 'versor-replace)

  (put 'versor-insert-before 'joystick-help "<--ins")
  (put 'versor-insert-after 'joystick-help "ins-->")
  (put 'versor-insert-around 'joystick-help "<--ins-->")
  (put 'versor-replace 'joystick-help "replace")
  (joystick-label-modified "ToBt-" "Insert" device)

  (versor-define-insertion
   [ ThumbBtn-up ] 'versor-top-n-kills)
  (versor-define-insertion
   [ BaBt2-BaseBtn-up ] (lambda (n) "parentheses" (list "(" ")")))
  (versor-define-insertion
   [ BaBt-BaseBtn2-up ] (lambda (n) "brackets" (list "[" "]")))
  (versor-define-insertion
   [ PiBt-BaBt-BaseBtn2-up ] (lambda (n) "braces" (list "{" "}")))
  (versor-define-insertion
   [ ThumbBtn2-up] 'versor-top-n-searches)
  (versor-define-insertion
   [ Trigger-up ] (lambda (n) "line end" (list "\n")))
  ;; PiBt insertions [- ]
  ;; [- ]    if-then
  ;;       let    while
  ;;           call
  ;;
  (versor-define-insertion
   [ PiBt-Trigger-up ]
   (lambda (n)
     "variable declaration in programming language of current mode"
     (versor-statement-insertion-strings 'variable-declaration)))
  (versor-define-insertion
   [ PiBt-ThumbBtn-up ]
   (lambda (n)
     "if-then statement in programming language of current mode"
     (versor-statement-insertion-strings 'if-then)))
  (versor-define-insertion
   [ PiBt-ThumbBtn2-up ]
   (lambda (n)
     "funcall statement in programming language of current mode"
     (versor-statement-insertion-strings 'function-call)))
  (versor-define-insertion
   [ PiBt-TopBtn-up ]
   (lambda (n)
     "while statement in programming language of current mode"
     (versor-statement-insertion-strings 'while-do)))
  ;; ToBt2-PiBt insertions [= ]
  ;; [= ]    if-then-else
  ;;       setq       for
  ;;           defun
  (versor-define-insertion
   [ ToBt2-PiBt-Trigger-up ]
   (lambda (n)
     "assignment statement in programming language of current mode"
     (versor-statement-insertion-strings 'assignment)))
  (versor-define-insertion
   [ ToBt2-PiBt-ThumbBtn-up ]
   (lambda (n)
     "if-then-else statement in programming language of current mode"
     (versor-statement-insertion-strings 'if-then-else)))
  (versor-define-insertion
   [ ToBt2-PiBt-TopBtn-up ]
   (lambda (n)
     "for statement in programming language of current mode"
     (versor-statement-insertion-strings 'for)))
  (versor-define-insertion
   [ ToBt2-PiBt-ThumbBtn2-up ]
   (lambda (n)
     "defun statement in programming language of current mode"
     (versor-statement-insertion-strings 'defun)))
  ;; ToBt2 insertions [_ ]
  ;; [_ ]      or
  ;;        and
  ;;           not
  (versor-define-insertion
   [ ToBt2-Trigger-up ]
   (lambda (n)
     "AND operator in programming language of current mode"
     (versor-statement-insertion-strings 'and)))
  (versor-define-insertion
   [ ToBt2-ThumbBtn-up ]
   (lambda (n)
     "OR operator in programming language of current mode"
     (versor-statement-insertion-strings 'or)))
  (versor-define-insertion
   [ ToBt2-ThumbBtn2-up ]
   (lambda (n)
     "NOT operator in programming language of current mode"
     (versor-statement-insertion-strings 'not)))

  ;; Now some bindings similar to the insertions, but for languide
  ;; high-level commands.  These try to follow the same conventions as
  ;; the versor insertions:
  ;;         if-then
  ;;       let    while
  ;;           call
  ;; with the addition of hat movement up being "group" and hat left
  ;; being "up to scope"; hat down being "remove control" and hat
  ;; right being "comment selection".
  (global-set-key [ PiBt-Trigger-up] 'languide-map)
  (define-key languide-map [ PiBt-Trigger-up ] 'versor-languide-convert-selection-to-variable)
  (define-key languide-map [ ToBt2-Trigger-up ] 'versor-languide-convert-selection-to-global-variable)
  (define-key languide-map [ ThumbBtn2-up ] 'versor-languide-convert-selection-to-function)
  (define-key languide-map [ PiBt-ThumbBtn2-up ] 'versor-languide-create-function-for-call)
  (define-key languide-map [ ToBt2-ThumbBtn2-up ] 'versor-languide-surround-selection-with-call)
  (define-key languide-map [ ToBt2-PiBt-ThumbBtn2-up ] 'versor-languide-remove-function-call)
  (define-key languide-map [ PiBt-ThumbBtn-up ] 'versor-languide-make-conditional)
  (define-key languide-map [ ToBt2-PiBt-ThumbBtn-up ] 'versor-languide-make-conditional-else)
  (define-key languide-map [ PiBt-TopBtn-up] 'versor-languide-make-iterative)
  (define-key languide-map [ ToBt2-PiBt-TopBtn-up ] 'versor-languide-make-iterative-indexed)
  (define-key languide-map [ Hat0Y-previous ] 'versor-languide-unify-statements)
  (define-key languide-map [ Hat0X-previous ] 'versor-languide-enclosing-scoping-point)
  (define-key languide-map [ Hat0Y-next ] 'versor-languide-remove-control)
  (define-key languide-map [ Hat0X-next ] 'versor-languide-comment-selection)
  (define-key languide-map [ BaseBtn3up ] 'versor-describe-selection)

  ;; [upper left, lower right] for begin altering
  (global-set-key [ ToBt2-BaseBtn2-down ] 'versor-begin-altering-item)

  (put 'versor-begin-altering-item 'joystick-help "alter")

  (define-key versor-altering-mode-map [ ToBt2-BaBt2-Hat0X-next ] 'versor-alter-item-next-value)
  (define-key versor-altering-mode-map [ ToBt2-BaBt2-Hat0X-previous ] 'versor-alter-item-previous-value)
  (define-key versor-altering-mode-map [ ToBt2-BaBt2-Hat0Y-next ] 'versor-alter-item-next-type)
  (define-key versor-altering-mode-map [ ToBt2-BaBt2-Hat0Y-previous ] 'versor-alter-item-previous-type)
  (define-key versor-altering-mode-map [ ToBt2-BaseBtn2-up ] 'versor-end-altering-item)
  (joystick-label-modified "ToBt2-BaBt2-" "Altering" device)

  (global-set-key [ Trigger-up ] 'possibly-other-window-versor-copy)
  (global-set-key [ ThumbBtn-up ] 'versor-kill)
  (global-set-key [ TopBtn-up ] 'possibly-other-window-yank)
  (global-set-key [ ThumbBtn2-up ] 'find-this-tag)
  (define-key emacs-lisp-mode-map
    [ ThumbBtn2-up ] 'versor-find-this-function)
  (define-key lisp-interaction-mode-map
    [ ThumbBtn2-up ] 'versor-find-this-function)

  (global-set-key [ ThBt2-Hat0X-next ] 'versor-search-forward)
  (global-set-key [ ThBt2-Hat0X-previous ] 'versor-search-backward)
  (define-key isearch-mode-map [ ThBt2-Hat0X-next ] 'isearch-repeat-forward)
  (define-key isearch-mode-map [ ThBt2-TopBtn-up ] 'isearch-yank-word)
  (define-key isearch-mode-map [ ThBt2-Hat0X-previous ]'isearch-repeat-backward)

  (put 'versor-copy 'joystick-help "v-copy")
  (put 'versor-kill 'joystick-help "v-kill")
  (put 'yank 'joystick-help "yank")
  (put 'find-tag 'joystick-help "find tag")
  (put 'find-function 'joystick-help "find fn")

  (joystick-label-modified "" "Versor" device))

(defun versor-joystick-setup ()
  "Configure the joystick for versor."
  (interactive)
  (add-hook 'joystick-bindings-hook 'versor-joystick-bindings)
  (setq versor-joystick-process (joystick-start)))

(defvar versor-joystick-wrappable-commands
  '(versor-previous-word
    versor-next-word
    versor-backward-sentence
    versor-forward-sentence
    previous-sexp
    next-sexp
    versor-backward-up-list
    versor-down-list
    statement-navigate-parts-previous
    statement-navigate-parts-next
    previous-statement
    next-statement)
  "Commands for which joystick wrappers should be made.")

(defun versor-joystick-make-wrappers (&optional prefix suffix
						sequencer sequencer-arg
						following-copy-function following-yank-function)
  "Make the versor-joystick wrappers for various forms of movement.
Optional PREFIX and SUFFIX are the prefix and suffix to use for each name.
Optional SEQUENCER is the name of a progn-like form to use in each function.
Optional SEQUENCER-ARG is the first argument of SEQUENCER.
If FOLLOWING-COPY-FUNCTION and FOLLOWING-YANK-FUNCTION are given, set the
`following-copy-function' and `following-yank-function' properties of the function
created to those functions."
  (or prefix
      (setq prefix "versor-joystick-"))
  (or suffix
      (setq suffix ""))
  (or sequencer
      (setq sequencer 'progn))
  (dolist (base versor-joystick-wrappable-commands)
    (let* ((base-string (symbol-name base))
	   (base-base-string (and (string-match "^\\(versor-\\)?\\(.+\\)$"
						base-string)
				  (match-string 2 base-string)))
	   (name (intern (concat prefix
				 base-base-string
				 suffix)))
	   (body `(lambda (arg)
		    ,(format "Like %s but for driving from analog joystick."
			     base)
		    (interactive "p")
		    (,sequencer ,sequencer-arg
				(versor-as-motion-command item
				  (,base arg))))))
      (message "Defining %S to do %S" name body)
      (fset name body)
      (if following-copy-function
	  (put name 'following-copy-function following-copy-function))
      (if following-yank-function
	  (put name 'following-yank-function following-yank-function)))))

(defun possibly-other-window-versor-copy ()
  "If the most recent movement was in another window, copy from that window.
Otherwise, do an ordinary copy."
  (interactive)
  (message "Last command was %S, its copy function is %S" last-command (get last-command 'following-copy-function))
  (funcall (or (get last-command 'following-copy-function)
	       'versor-copy)))

(defun possibly-other-window-yank (&optional arg)
  "If the most recent movement was in another window, yank from that window.
Otherwise, do an ordinary yank.
Optional ARG is as for 'yank'."
  (interactive)
  (funcall (or (get last-command 'following-yank-function)
	       'yank)
	   arg))

(defun versor-other-window-function-setup (fname)
  "Set up function FNAME as a movement command for the other window."
  (put fname 'following-copy-function 'other-window-versor-copy)
  (put fname 'following-yank-function 'other-window-versor-yank))

(defun versor-other-other-window-function-setup (fname)
  "Set up function FNAME as a movement command for the other ther window."
  (put fname 'following-copy-function 'other-other-window-versor-copy)
  (put fname 'following-yank-function 'other-other-window-versor-yank))

(defun other-window-versor-copy ()
  "Do versor-copy in other-window-."
  (interactive)
  (in-other-window 1
		   (versor-copy)))

(defun other-window-versor-yank (&optional arg)
  "Do yank in other-window-.
Optional ARG is passed on."
  (interactive)
  (in-other-window 1
		   (yank arg)))

(defun other-other-window-versor-copy ()
  "Do versor-copy in other-other-window-."
  (interactive)
  (in-other-window 2
    (versor-copy)))

(defun other-other-window-versor-yank (&optional arg)
  "Do yank in other-other-window-.
Optional ARG is passed on."
  (interactive)
  (in-other-window 2
    (yank arg)))

(defun versor-find-this-function ()
  "Find the function at point, without prompting."
  (interactive)
  (versor-as-motion-command item
      (find-function (function-called-at-point))))

(provide 'versor-joystick)

;;; versor-joystick.el ends here
