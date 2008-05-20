;;;; versor-voice.el
;;; Time-stamp: <2007-06-20 19:57:01 jcgs>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2005, 2006  John C. G. Sturdy
;;
;; This file is part of emacs-versor.
;;
;; emacs-versor is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; emacs-versor is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with emacs-versor; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; This module provides spoken command definitions for versor; see
;; versor.el itself for spoken output

(defvar vr-versor-dimension-command-list nil
  "Vocal movement commands for versor.
Generated automatically from the movement dimensions table, moves-moves.")

(defun singular (plural)
  "Try to make a plausible singular form of PLURAL."
  (cond
   ((string-match "\\([a-z]+\\)s" plural)
    (match-string 1 plural))
   ((string-match "\\([a-z]+\\)ren" plural)
    (match-string 1 plural))
   ((string-match "\\([a-z]+\\)en" plural)
    (match-string 1 plural))
   (t plural)))

(defun define-versor-vocal ()
  "Define the vocal commands."
  (interactive)
  (dolist (level (versor-all-level-names))
    ;; define a command to select this level
    (let* ((selector-function-name (intern (concat "versor-select-" level)))
	   (selector-function-body
	    `(lambda ()
	       ,(format "Set the versor dimension to %s.\n\nThis function was generated automatically by define-versor-vocal." level)
	       (interactive)
	       (versor-select-named-level ,level)))
	   )
      ;; (message "Defining %S to be %S" selector-function-name selector-function-body)
      (fset selector-function-name selector-function-body)
      (pushnew (cons level selector-function-name)
	       vr-versor-dimension-command-list
	       :test 'equal)
      (pushnew (cons (format "move by %s" level) selector-function-name)
	       vr-versor-dimension-command-list
	       :test 'equal))

    ;; for this level, define the four versor movements
    (dolist (action '("first" "previous" "next" "last"))
      (let* ((unit (singular level))
	     (action-function-name (intern (format "versor-voice-%s-%s" action unit)))
	     (action-voice-command (format "%s %s" action unit))
	     (action-function-body
	      `(lambda (n)
		 ,(format "Move to the %s %s\n\nThis function was generated automatically by define-versor-vocal."
			  action level)
		 (interactive "p")
		 (let* ((level-pair (versor-find-level-by-single-name ,level))
			(versor-meta-level-shadow (car level-pair))
			(versor-level-shadow (cdr level-pair))
			(action (versor-get-action ',(intern action))))
		   (funcall action n)))))
	(unless (boundp action-function-name)
	  (fset action-function-name action-function-body))
	(pushnew (cons action-voice-command action-function-name)
		 vr-versor-dimension-command-list
		 :test 'equal)))

    ;; (dolist (action '("start of" "end of"))
;;       (let* ((unit (singular level))
;; 	     (action-function-name (intern (format "versor-voice-%s-%s" action unit)))
;; 	     (action-voice-command (format "%s %s" action unit))
;; 	     (action-function-body
;; 	      `(lambda (n)
;; 		 ,(format "Move to the %s the %s\n\nThis function was generated automatically by define-versor-vocal."
;; 			  action level)
;; 		 (interactive "p")
;; 		 (let* ((level-pair (versor-find-level-by-single-name ,level))
;; 			(versor-meta-level-shadow (car level-pair))
;; 			(versor-level-shadow (cdr level-pair))
;; 			(action (versor-get-action ',(intern action))))
;; 		   (funcall action n)))))
;; 	(unless (boundp action-function-name)
;; 	  (fset action-function-name action-function-body))
;; 	(pushnew (cons action-voice-command action-function-name)
;; 		 vr-versor-dimension-command-list
;; 		 :test 'equal)))
    )

  (dolist (meta-level (versor-meta-level-names))
    (let* ((name (car meta-level))
	   (symbol (intern (concat "versor-select-meta-" name)))
	   (body `(lambda ()
		    ,(format "Set the versor meta-dimension to %s.\n\nThis function was generated automatically by define-versor-vocal." name)
		    (interactive)
		    (versor-select-named-meta-level ,name))))
      (fset symbol body)
      (pushnew (cons (format "%s movements" name) symbol)
	       vr-versor-dimension-command-list
	       :test 'equal))))

(if (null vr-versor-dimension-command-list) (define-versor-vocal))

(defun versor-surround-with-binding ()
  "Surround the selection with a binding."
  (interactive)
  (versor-insert-around
   (versor-statement-insertion-strings 'variable-declaration)))

(defun versor-surround-with-and ()
  "Surround the selection with an AND."
  (interactive)
  (versor-insert-around
   (versor-statement-insertion-strings 'and)))

(defun versor-surround-with-or ()
  "Surround the selection with an OR."
  (interactive)
  (versor-insert-around
   (versor-statement-insertion-strings 'or)))

(defun versor-surround-with-not ()
  "Surround the selection with a not."
  (interactive)
  (versor-insert-around
   (versor-statement-insertion-strings 'not)))

(defvar vr-versor-command-list
  '(("next one" . versor-next)
    ("onwards" . versor-next)
    ("backwards" . versor-prev)
    ("back one" . versor-prev)
    ("previous one" . versor-prev)
    ("first one" . versor-start)
    ("initial one" . versor-start)
    ("last one" . versor-end)
    ("final one" . versor-end)
    ("extend" . versor-extend-item-forwards)
    ("retract" . versor-extend-item-backwards)
    ;; meta-movements
    ("dimension in" . versor-in)
    ("dimension out" . versor-out)
    ("zoom movements in" . versor-in)
    ("zoom movements out" . versor-out)
    ("meta dimension in" . versor-prev-meta-level)
    ("meta dimension out" . versor-next-meta-level)
    ("over" . versor-out-briefly)
    ("end" . versor-end-of-item)
    ("reverse" . versor-reverse)
    ("beginning" . versor-start-of-item)
    ("other end" . versor-other-end-of-item)
    ;; aliases for dimensions
    ("expressions" . versor-select-exprs)
    ("characters" . versor-select-chars)
    ;; basic editing commands
    ("copy this" . versor-copy)
    ("cut this" . versor-kill)
    ("mark this" . versor-mark)
    ("transpose" . versor-transpose)
    ("valof" . wander-yank)
    ("result is" . pick-up-sexp-at-point)
    ("return" . exit-recursive-edit)
    ;; alterations
    ("alter this" . versor-begin-altering-item)
    ("that's it" . versor-end-altering-item)
    ("bingo" . versor-end-altering-item)
    ("accept" . versor-end-altering-item)
    ("revert to original" . versor-abandon-altering-item)
    ;; higher-level movements
    ("container" . versor-select-container-contents)
    ;; language-guided edits
    ("extract variable" . versor-languide-convert-selection-to-variable)
    ("extract global variable" . versor-languide-convert-selection-to-global-variable)
    ("extract function" . versor-languide-convert-selection-to-function)
    ("surround with call" . versor-languide-surround-selection-with-call)
    ("remove function call" . versor-languide-remove-function-call)
    ("create function for call" . versor-languide-create-function-for-call)
    ("unify statements" . versor-languide-unify-statements)
    ("comment selection" . versor-languide-comment-selection)
    ("enclosing scoping point" . versor-languide-enclosing-scoping-point)
    ("enclosing decision point" . versor-languide-enclosing-decision-point)
    ("employ variable" . versor-languide-employ-variable)
    ("make conditional" . versor-languide-make-conditional)
    ("make iterative" . versor-languide-make-iterative)
    ("remove control" . versor-languide-remove-control)
    ;; versor insertions
    ("surround with binding" . versor-surround-with-binding)
    ("surround with and" . versor-surround-with-and)
    ("surround with or" . versor-surround-with-or)
    ("surround with not" . versor-surround-with-not))
  "Individually defined versor voice commands (and friends).")

(provide 'versor-voice)

;;; end of versor-voice.el
