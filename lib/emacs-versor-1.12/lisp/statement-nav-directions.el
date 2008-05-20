;;;; statement-nav-directions.el -- follow directions to navigate to parts of statements
;;; Time-stamp: <2007-08-21 18:19:20 john>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; This file implements the basic movements needed to navigate around
;; statements. Directions for specific statement parts in various
;; programming languages are defined in such files as
;; languide-lisp-like.el, languide-c-like.el and so forth.

;; Normally, a sequence of directions is followed, and the last one is
;; taken as the result, that is, the thing to leave selected. To allow
;; multipart selections, as versor does, you can indicate a step of
;; the directions as selecting what it moves over, by wrapping it in a
;; call to ``remember''.

(defconst languide-parts '("container" "framework" "whole" "head" "body" "tail")
  "The parts we can navigate to.")

(defun navigate-to (part)
  "Navigate to PART of the current statement."
  (add-hook 'after-change-functions 'languide-after-change-function nil t) ; to invalidate cache as needed
  (let* ((old-position (point))		; in case we give up
	 ;; see which statement we are now on; if it is the same as the last
	 ;; time we did any navigation, we can use cached navigation data
	 (prelocated (latest-move-was-languide))
	 (statement-start (if prelocated
			      (progn
				(languide-debug-message 'navigate-to "re-using statement position from latest-statement-known=%S" latest-statement-known)
				(car latest-statement-known))
			    (progn
			      (beginning-of-statement-internal)
			      (languide-debug-message 'navigate-to "navigate-to %S found statement begins here"
						      part)
			      (point))))
	 (remembered (statement-find-part statement-start part)))
    (if remembered
	(progn				; we have cached data for this
	  ;; (message "using cached statement position %S" remembered)
	  (versor-set-current-items remembered)
	  (versor-display-highlighted-choice (symbol-name part) (languide-parts)))
      (let* ((type (if prelocated
		       (third latest-statement-known)
		     (identify-statement nil)))
	     (directions (get-statement-part type part)))
	;; no cached data, really do the navigation
;;;;;;;;;;;;;;;; type is coming through as nil if we are prelocated but had not cached this part
	;; (message "not cached; navigate-to %S %S got %S" type part directions) (backtrace)
	(if directions
	    (if (eq (car directions) 'statement-navigate)
		(let ((selected-parts (progn
					(goto-char statement-start)
					(statement-navigate (cdr directions)))))
		  ;; cache the data in case we want it again;
		  ;; (message "caching statement position %S" selected-parts)
		  (statement-remember-part statement-start type
					   part
					   ;;(list (point) mark-candidate)
					   selected-parts
					   )
		  ;; (message "Selected-parts=%S" selected-parts)
		  ;; (versor-set-current-item (point) mark-candidate)
		  (versor-set-current-items selected-parts)
		  ;; (message "point now at %d" (point))
		  (versor-display-highlighted-choice (symbol-name part) (languide-parts)))
	      (goto-char old-position)
	      (error "Don't know how to handle directions like \"%S\" (for %S of %S)" directions part type))
	  (goto-char old-position)
	  (error "No %S defined for \"%S\" for %S" part type major-mode)))))
  (setq navigated-latest-part part
	navigated-latest-place (point)))

(defvar mark-candidate nil
  "Where the mark will be set at the end of statement-navigate.
We set this (using set-mark-candidate) instead of setting the mark,
at each stage of navigation, to avoid polluting the mark-ring.")

(defun latest-statement-navigation-end (&optional junk)
  "Return the end of the latest statement navigation result.
Optional argument for compatibility with other things that get the versor
package to the end of an item."
  mark-candidate)

;; (defvar transient-transient-mark-mode 120
;;   "*Whether to turn transient-mark-mode on briefly after each statement navigation.
;; if this is a number, is used as the number of seconds for which to do this.
;; If you make this as long as you're likely to wait, it will in effect sit there
;; until you do something else.")

(defvar statement-navigate-list-selector-functions
  '(statements
    statement
    statement-contents
    upto
    from-start-of-statement
    start-of-match
    preceding-expression
    expression
    expressions
    expression-contents
    rest-of-line
    block-at-this-depth
    python-docstring
    )
  "Functions for statement navigation, that can select what they describe.
All these functions should return a cons of the start and end
positions of what they select.")

(defun statement-navigate-list (directions &optional so-far)
  "Internal for statement-navigate.
Uses the dynamic variables of statement-navigate, so probably not suitable
for use elsewhere."
  (let ((last-part nil))
    (catch 'done
      (dolist (direction directions)
    
	;; (message "Navigating %S" direction)
	(setq last-part
	      (cond
	       ((stringp direction)
		(re-search-forward direction (point-max) t)
		(cons (match-beginning 0) (match-end 0)))
	       ((consp direction)
		(let ((functor (car direction)))
		  (cond
		   ((eq functor 'remember)
		    ;; (message "(remember %S)" (cdr direction))
		    (setq so-far (statement-navigate-list (cdr direction) so-far))
		    ;; (message "so-far=%S" so-far)
		    ;; return nil to last-part to avoid remembering it twice
		    nil)
		   ((eq functor 'if)
		    (setq so-far
			  (if (let ((condition (cadr direction)))
				(cond
				 ((stringp condition) (looking-at condition))
				 ((consp condition) (eval condition))))
			      (statement-navigate-list (caddr direction) so-far)
			    (statement-navigate-list (cdddr direction) so-far))))
		   ((memq functor statement-navigate-list-selector-functions)
		    (eval direction))
		   (t (eval direction)
		      nil))))
	       ((eq direction 'fail)
		(throw 'done nil))
	       (t (error "unknown navigation element %S" direction))))
	;; (message "end of statement-navigate-list loop, so-far=%S" so-far)
    
	))
    (when last-part
      (setq so-far (cons last-part so-far))))
  so-far)

(defun statement-navigate (directions)
  "Take DIRECTIONS for navigating around a statement.
Leave point at the start of the selected section."
  (setq mark-candidate nil)
  (let ((selected-parts (catch 'navigation-end
			  (statement-navigate-list directions)))
	)
    
    ;; (message "after statement-navigate-list(%S) selected-parts=%S" directions selected-parts)
    (set-mark mark-candidate)
    (nreverse selected-parts)))

(defun set-mark-candidate (m)
  "Set the mark candidate to M.
At the end of statement-navigate, this is used to set the mark.
This avoids polluting the mark ring with intermediate marks."
  (setq mark-candidate m))

(defun expression (&optional n)
  "Move forward over an expression.
For use in statement-navigate."
  (interactive)
  (set-mark-candidate (point))
  (let ((start (point)))
    (forward-sexp (if n n 1))
    (if (interactive-p) (set-mark mark-candidate))
    (cons start (point))))

(defun expression-contents (&optional n)
  "Select the contents of an expression (omitting initial and final whitespace).
Intended for use from statement-navigate."
  (interactive)
  (if (null n) (setq n 1))
  ;; (message "expression-contents starting from %d (\"%s...\")" (point) (buffer-substring (point) (min (point-max) (+ (point) 12))))
  (let ((start (point)))
    (forward-sexp n)
    (let ((after-end (point)))
      (forward-sexp (- n))
      (if (looking-at "\\s(")
	  (progn
	    (down-list 1)
	    (skip-syntax-forward " ")
	    (let ((start (point)))
	      (goto-char (1- after-end))
	      (skip-syntax-backward " ")
	      (set-mark-candidate (point))
	      (goto-char start)))
	(progn
	  (set-mark-candidate after-end))))
    (if (interactive-p) (set-mark mark-candidate))
    (cons (point) mark-candidate)))

(defun expressions ()
  "Select as many expressions as possible, stopping on hitting a non-balanced closing bracket."
  (interactive)
  (let ((start (point)))
    (goto-char (scan-lists start 1 1))
    (let ((after-end (point)))
      (goto-char (1- after-end))
      (skip-syntax-backward " ")
      (set-mark-candidate start)
      (if (interactive-p) (set-mark mark-candidate))
      (cons start (point)))))

(defmacro without-changing-current-statement (&rest bodyforms)
  "Execute BODYFORMS without changing the currently selected statement."
  `(let ((languide-last-statement-selector-command languide-last-statement-selector-command)
	 (statement-latest-start statement-latest-start)
	 (navigated-latest-part navigated-latest-part)
	 (items (mapcar (function
			 (lambda (item-o)
			   (if (consp item-o)
			       item-o
			     (cons (overlay-start item-o)
				   (overlay-end item-o)))))
			(versor-get-current-items))))
     (let ((result (progn ,@bodyforms)))
       (versor-set-current-items items)
       result)))

(defun statement ()
  "Select the next statement."
  (interactive)				; for testing
  (progn ; without-changing-current-statement
   (skip-to-actual-code)
   (if (looking-at (compound-statement-open))
       (progn
	 (expression))
     (let ((start (point)))
       (next-statement-internal 1)
       (skip-to-actual-code-backwards)
       (set-mark-candidate start)
       (cons start (point))))))
  
(defun statements ()
  "Select the statements up to the next closing bracket."
  (interactive)	; mostly for testing, but it might come in useful in its own right
  (let* ((start nil)
	 (selected (without-changing-current-statement
		    (skip-to-actual-code)
		    (setq start (point))
		    (while (not (or (eobp)
				    (looking-at (compound-statement-close))))
		      ;; (message "forward one statement from %d" (point))
		      (next-statement-internal 1)
		      (skip-to-actual-code))
		    (skip-to-actual-code-backwards)
		    (set-mark-candidate start)
		    (cons start (point)))))
    (when (interactive-p)
	(set-mark (car selected)))
    (goto-char (cdr selected))
    selected))

(defun statement-contents ()
  "Select the contents of next statement.
If it is a simple statement, it is selected in its entirety.
If it is a compound statement, the statements that make it up are selected,
but the compound statement delimiters are not."
  (progn ; without-changing-current-statement
   (skip-to-actual-code)
   (if (not (looking-at (compound-statement-open)))
       (statement)
     (down-list 1)
     (skip-to-actual-code)
     (let ((start (point)))
       (statements)
       ;; (backward-char 1)
       ;; (beginning-of-statement-internal)
       ;; (end-of-statement-internal)
       (cons start (point))))))

(defun upto (pattern)
  "Select up to the start of PATTERN."
  (let ((start (point)))
    (if (re-search-forward pattern (point-max) t)
	(set-mark-candidate (match-beginning 0))
      (error "No %s found" pattern))
    (goto-char start)
    (if (interactive-p) (set-mark mark-candidate))
    (cons start mark-candidate)))

(defun rest-of-line ()
  "Select the rest of the line."
  (cons (point) (line-end-position)))

(defun block-at-this-depth ()
  "Select the block at this depth."
  (back-to-indentation)
  (let ((start (point))
	(this-indent (current-indentation)))
    (while (>= (current-indentation) this-indent)
      (forward-line 1))
    (forward-line -1)
    (cons start (line-end-position))))

(defun from-start-of-statement ()
  "Move to the start of the statement."
  (set-mark-candidate statement-latest-start)
  (cons (point) statement-latest-start))

(defun start-of-match ()
  "Move point to the start of the match just done (for statement navigation)."
  (let ((where (point)))
    (goto-char (match-beginning 0))
    (cons (point) where)))

(defun preceding-expression ()
  "Select the preceding expression (for statement navigation)."
  (backward-sexp 1)
  (expression))

(defun python-docstring ()
  "Select a python docstring."
  (if (looking-at "\"\"\"")
      (let ((start (point)))
	(goto-char (match-end 0))
	(search-forward "\"\"\"")
	(cons start (point)))))

(defun continue-if (pattern)
  "Check that we are looking at PATTERN.
If not, stop scanning the navigation directions here."
  (unless (looking-at pattern)
    (throw 'navigation-end nil)))

(provide 'statement-nav-directions)

;;; end of statement-nav-directions.el
