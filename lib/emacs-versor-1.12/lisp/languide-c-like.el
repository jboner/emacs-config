;;;; languide-c-like.el -- C, java, perl definitions for language-guided editing
;;; Time-stamp: <2008-01-22 17:25:25 jcgs>

;; Copyright (C) 2004, 2005, 2006, 2007, 2008  John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2004
;; Keywords: editing

;; This file is NOT part of GNU Emacs.

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

(require 'cl)
(require 'modal-functions)

;; todo: make use of cc-mode (would have to understand it first)

(defun languide-c-back-to-possible-ender (bod)
  "Move point back to be at something that might end a C statement.
It must be outside a comment or string, relative to BOD.
BOD is Beginning Of Defun, which is taken to be not in a comment or string."
  (let ((in-comment-or-string t))
    ;; keep looking for the possible start of a statement, and checking that
    ;; it is not part of a comment or string
    (while in-comment-or-string
      (cond
       ((and nil (looking-at "[a-z_]"))	; inside a symbol -- go to start of symbol -- not sure why I wanted to do this -- anded out for now
	(skip-syntax-backward "w_")
	)
       (t
	(re-search-backward "[{;}]"
			    bod	      ; (point-min)
			    t)	      ; leaves point at start of match
	;; having found a character that can end a C statement,
	;; we now parse from the start of the defun up to the
	;; position of the character, to see whether the character
	;; is in code or in string-or-comment
	(let ((result (save-excursion (parse-partial-sexp bod (point)
							  nil ; target-depth
							  nil ; stop-before
							  nil ; state
							  nil ; stop-comment
							  ))))
	  (setq in-comment-or-string
		(or
		 ;; emacs19 doesn't give us that handy 8th element!
		 (nth 8 result)
		 ;; only want a number in in-comment-or-string if it
		 ;; tells us a character position; this one gives us a
		 ;; character code
		 (if (nth 3 result) t nil)
		 (nth 4 result)
		 (languide-c-inside-for-control)))
	  (if in-comment-or-string
	      (if (numberp in-comment-or-string)
		  (goto-char in-comment-or-string)
		(backward-char 1)))))))))

(defun languide-c-inside-for-control ()
  "Return whether we seem to be in the head of a for loop.
Useful when we've got to a semicolon and need to know whether it terminates a statement.
If not at a semicolon (e.g. deeper inside an expression inside the for loop head) this
is liable to return the wrong result."
  (save-excursion
    (condition-case error-var
	(progn
	  ;; we only need to go up one level of brackets, because the
	  ;; semicolons in a for-control occur at the top level of it
	  (backward-up-list 1)
	  (backward-sexp 1)
	  (looking-at "for"))
      (error nil))))

(defun blank-between (a b)
  "Return whether there is only whitespace between A and B."
  (save-excursion
    (goto-char a)
    (= (- b a)
       (skip-chars-forward " \t\n\r" b))))

(defmodal move-into-previous-statement (c-mode c++-mode perl-mode java-mode) ()
  "Move into the previous C (etc) statement.
Need only work if already at or just before the start of a statement."
  (interactive)
  (skip-to-actual-code-backwards)
  (if (eq (char-before (point)) ?})
      (backward-sexp 1)
    (backward-char 1)))

(defmodal move-into-next-statement (c-mode c++-mode perl-mode java-mode) ()
  "Move into the next C (etc) statement.
Need only work if already at or just beyond the end of a statement."
  (interactive)
  (skip-to-actual-code)
  (if (looking-at "{")
      (forward-sexp 1)
    (forward-char 1)))

(defun languide-previous-substatement ()
  "Go back a block, if at a block end, otherwise go back a statement."
  (skip-to-actual-code-backwards)
  (if (eq (char-syntax (char-before (point)))
	  open-bracket)
      (goto-char (safe-scan-sexps (point) -1))
    (previous-statement 1)))

(defvar c-like-function-type-modifiers
  (concat "\\(" (mapconcat 'identity
			   '("static" "struct" "extern" "public" "private")
			   "\\)\\|\\(")
	  "\\)")
  "Things that can appear before the actual types of functions.")

(defun continue-back-past-curly-ket (starting-point)
  "Continue to back to the start of a C statement, having got back to a closing curly bracket.
Return the type if we spotted it."
  ;; taken out-of-line for readability of surrounding code
  (let* ((close (point))
	 (following-code (skip-to-actual-code)))
    (cond
     ;; first two cases are those where a keyword can follow a closing brace
     ;; and still be within a statement
     ((looking-at "\\<while\\>")
      ;; might be
      ;;   do { ... } * while ( ... )
      ;; or
      ;;   { ... } * while ( ...) { ... } // with the first { ... } being a free-standing compound statement
      ;; and so must go back to check for the "do"
      (goto-char (safe-scan-sexps (point) -2))
      (if (looking-at "\\<do\\>")
	  'do-while
	;; go back to the "while"
	(goto-char following-code)
	'while-do))
     ((looking-at "\\<else\\>")
      ;; where we started was:
      ;;   if ( ...) { ... } * else { ... }
      ;; and so we must go back over the "then", the condition, and the keyword
      ;; todo: this should be a beginning-of-statement-internal and then 2 exprs
      (goto-char (safe-scan-sexps (point) -3))
      'if-then-else)

     ;; having dealt with both the "{ ... } <keyword>" cases,
     ;; now see whether we started inside a statement that follows a closing brace
     ((>= starting-point following-code)
      ;; this case looks like:
      ;;   { ... } ab*cd // where the { ... } is a free-standing compound statement, or anything else
      ;; so we want to go to:
      ;;   { ... } *abcd
      (goto-char following-code)
      ;; in this case, we don't know the type
      nil)
     ;; next, try going back a statement and a keyword
     ;; and seeing if that is an else
     ((save-excursion ; don't disturb things for the next clause's test
	;; we are looking for the cases like:
	;;   if (...) { ... } else { ... } *
	;; (languide-previous-substatement) ; we know it's a compound statement, or we wouldn't be in this function
	;; todo: these can probably all be plain scan-sexps, as we're not checking for nil result anyway
	(goto-char (safe-scan-sexps close -2))
	;; let's see if that's got us to:
	;;   if (...) { ... } *else { ... }
	(looking-at "\\<else\\>"))
      (goto-char (safe-scan-sexps close -2)) ; re-do the movement that we did inside the save-excursion above
      (languide-previous-substatement)
      (goto-char (safe-scan-sexps (point) -2))
      'if-then-else)

     ((save-excursion ; don't disturb things for the next clause's test
	(goto-char (safe-scan-sexps close -1))
	(zerop (current-column)))
      ;; top-level braces, presume a defun
      (goto-char (safe-scan-sexps close -1))
      (message "checking for a function definition around %d" (point))
      (skip-to-actual-code-backwards) (backward-char 1)
      (message "looks like a function definition around %d" (point))
      ;; but is it K&R or ANSI?
      (cond
       ((looking-at ";")
	(message "K&R arg type"))
       ((looking-at ")")
	(message "ANSI arg list")))
      (when (re-search-backward "^[a-z][a-z0-9_ ]+(" (point-min) t)
	(skip-chars-backward "a-z \t_")))

     ((save-excursion ; don't disturb things for the next clause's test
	;; todo: add handling for java's "throws", etc
	;; todo: we should look for if (...) { ... } first, though
	;; we are looking for the cases like:
	;;   void function (...) { ... } *
	(goto-char (safe-scan-sexps close -2))
	(looking-at "("))
      ;; re-do the movement that we did inside the save-excursion
      ;; above, and then carry on back past the function name and type
      (goto-char (safe-scan-sexps close -4))
      (let ((type-possible-start-start (point)))
	(while (progn
		 (backward-word 1)
		 (looking-at c-like-function-type-modifiers))
	  (setq type-possible-start-start (point)))
	(goto-char type-possible-start-start))
      'defun)


     ;; if it wasn't an else, try going back another sexp
     ((progn
	(goto-char (safe-scan-sexps close -3))
	(looking-at "\\(\\<\\(if\\)\\|\\(while\\)\\|\\(for\\)\\|\\(until\\)\\>\\)"))
      (intern (match-string-no-properties 1)))
     (t
      ;; Go back and see what was before the brace; could be
      ;;   if ( ... ) { ... }
      ;;   while ( ... ) { ... }
      ;;   until ( ... ) { ... }
      ;;   for ( ... ) { ... }
      ;; or it could just be a free-standing code block
      ;; (languide-debug-message 'continue-back-past-curly-ket "Other case of closing brace, at \"%s\"" (buffer-substring (point) (+ (point) 20)))

      ;; todo: I think some code may be missing here..........
      ;; (languide-debug-message 'continue-back-past-curly-ket "Other case; going back to look before the brace... got \"%s\"" (buffer-substring (point) (+ (point) 20)))
      ;; (languide-debug-message 'continue-back-past-curly-ket "Remaining there contentedly")
      (cond


       (t
	 ;; Before the brace was not if/while/for/until/else; assuming plain block
	(goto-char (safe-scan-sexps close -1))
	nil))))))

(defvar debug-overlays nil)

(defun debug-overlay (from to text colour)
  (when nil t
    (let ((o (make-overlay from to)))
      (overlay-put o 'face (cons 'background-color colour))
      (overlay-put o 'before-string (format "[%s:" text))
      (overlay-put o 'after-string (format ":%s]" text))
      (if nil
	  (push o debug-overlays)
	(read-char)
	(delete-overlay o)))))

(defmodal beginning-of-statement-internal (c-mode c++-mode perl-mode) ()
  "Move to the beginning of a C or Perl statement.
Return the type if obvious."
  (mapcar 'delete-overlay debug-overlays)
  (setq debug-overlays nil)
  (let ((starting-point (point))
	;; get beginning of defun, so we can use parse-partial-sexp to
	;; see whether we have landed in a string or comment
	(bod (save-excursion
	       (c-beginning-of-defun 1)
	       (point))))
    (languide-c-back-to-possible-ender bod)
    (let* ((possible-ender (point))
	   ;; We've now found a statement delimiter, and checked that it is a
	   ;; real one, and not part of a string or comment. Now we might make
	   ;; some adjustments, then finally move over any whitespace or comments
	   ;; leading in to the actual statement.
	   (possible-type (cond
			   ((looking-at "{")
			    ;; move one character forward (into the braced area) so that
			    ;; when we skip whitespace and comments, we will be at the
			    ;; start of the first statement in the braces
			    (forward-char 1)
			    nil)

			   ((looking-at "}")
			    (forward-char 1)
			    ;; this will take us back past the rest of the statement

			    ;; todo: which is wrong in some cases, so fix that! Only go
			    ;; back if what is ahead of us is something that can follow a
			    ;; closing brace within a statement, so for example we should
			    ;; go back if at "do { ... }  * while (...)", but not if at
			    ;; "while (...) do { ... } *"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

			    (continue-back-past-curly-ket
			     starting-point
			     ;; (point)
			     )

			    ;; (skip-to-actual-code)

			    )
			   ((looking-at ";")
			    (goto-char (match-end 0))
			    (prog1
				(let ((following-code (skip-to-actual-code)))
				  ;; We have looked back from point, and found that the
				  ;; nearest thing that could be the end of the previous
				  ;; statement is a semicolon. Then we've gone forward to the
				  ;; first thing that is not a space or comment, and by
				  ;; comparing that position with that of the semicolon, we
				  ;; can work out whether we have just gone straight to the
				  ;; start of the current statement, or whether it is some
				  ;; other case, such as the else part of an if-then-else
				  (if (<= following-code starting-point)
				      (progn
					;; this is the "abcd;    fg*hi" case
					(goto-char following-code)
					(if (looking-at "else")
					    (progn
					      ;; this is the "if (xyz) abcd;    el*se mnop;" case
					      (goto-char (- possible-ender 1)) ; just into the "then" statement
					      (beginning-of-statement-internal)
		      ;;;;;;;;;;;;;;;; probably more to do here?
					      )
					  nil))
				    (progn
				      ;; this is the "abcd;  *  fghi" case
				      (goto-char (- possible-ender 1))
				      (beginning-of-statement-internal))))
			      (when (save-excursion ; this is old version, is it right????????????????
				      (parse-partial-sexp (point) starting-point
							  nil ; targetdepth
							  t ; stopbefore
							  )
				      (= (point) starting-point))
				(backward-char 1)))))))

      ;; now we're at a real statement delimiter
    ;;;;;;;;;;;;;;;; why this "unless"? find out, and comment it!
      ;; might mean to go to the start of a leading comment if there is one, otherwise to the code?
      (unless (blank-between (point) (1+ starting-point))
	(skip-to-actual-code starting-point))
      ;; we don't know what type it is
      possible-type)))

(defmodal end-of-statement-internal (c-mode c++-mode perl-mode java-mode) (hint)
  "Move to the end of a C, Perl or Java statement. HINT suggests a statement type."
  (let ((old (point))
	;; get beginning of defun, so we can see whether we have landed in a
	;; string or comment
	(bod (save-excursion
	       (c-beginning-of-defun 1)
	       (point))))
    ;; (if (looking-at "{") (backward-char 1))
    (let ((in-comment-or-string t))
      ;; keep looking for the possible start of a statement, and checking that
      ;; it is not part of a comment or string
      (while in-comment-or-string
	(re-search-forward "[{;}]" (point-max) t) ; leaves point at end of match
	(let ((result (save-excursion (parse-partial-sexp bod (point)
							  0 ; target-depth
							  nil ; stop-before
							  nil ; state
							  nil ; stop-comment
							  ))))
	  (setq in-comment-or-string (or
				      ;; emacs19 doesn't give us that handy 8th element!
				      (nth 8 result)
				      (languide-c-inside-for-control)
				      (if (nth 3 result) t nil)
				      (nth 4 result)
				      ))
	  (if in-comment-or-string
	      (forward-char 1)))))

    (cond
     ((and (zerop (current-column))
	   (looking-at "\\(static\\|extern\\)? *\\(struct +\\)?\\*?\\([a-z][a-z0-9_]*\\)"))
      (end-of-defun))
     (t
      (cond
       ((= (char-after (1- (point))) ?{)
	(backward-char 1)
	(forward-sexp 1)))
      (cond
       ((save-excursion
	  (skip-to-actual-code)
	  (looking-at "else"))
	;; over the else keyword
	(forward-sexp 1)
	;; over the else clause
	(end-of-statement-internal nil))
       ((save-excursion
	  (skip-to-actual-code)
	  (looking-at "while"))
	;; now look around to see whether this is part of "do {} while ()"
	;; avert your eyes when reading this code, or re-write it for me!
	(when (save-excursion
		(skip-to-actual-code)
		(forward-sexp 2)
		(if (looking-at "[ \t\r\n]")
		    (skip-to-actual-code))
		(looking-at "[;}]"))
	  (skip-to-actual-code)
	  (forward-sexp 2)
	  (if (looking-at "[ \t\r\n]")
	      (skip-to-actual-code)))))))))

(defvar c-binding-regexp-1
  (concat  "\\([a-z][a-z0-9_]*\\s-*\\*?\\[?\\]?\\)\\s-*" ; the type
	   "\\([a-z][a-z0-9_]*\\)\\s-*"	; the name
	   "\\(\\[[0-9]*\\]\\)?\\s-*"	; array qualifier, if present
	   "\\(=[^;]*\\)?;"		; the value, if present
	   )
  "A regexp for a common kind of C binding, covering plain types.")

(defvar c-binding-regexp-2
  (concat "\\("
	  "\\(?:struct\\|unsigned\\|const\\|volatile\\)\\s-+" ; qualifiers
	  "[a-z][a-z0-9_]*\\s-*"	; the type
	  "\\*?\\[?\\]?"		; the trimmings
	  "\\)\\s-*"
	  "\\([a-z][a-z0-9_]*\\)"	; the name
	   "\\(\\[[0-9]*\\]\\)?\\s-*"	; array qualifier, if present
	  "\\s-*\\(=[^;]*\\)?;"		; the value, if present
	  )
  "A regexp for a common kind of C binding, covering ones with a qualifier before them.")

(defvar c-binding-regexp-3
  (concat "\\("
	  "\\(?:const\\|volatile\\)\\s-+" ; first qualifier
	  "\\(?:struct\\|unsigned\\)\\s-+" ; second qualifier
	  "[a-z][a-z0-9_]*\\s-*"	; the type
	  "\\*?\\[?\\]?\\)\\s-*"	; the trimmings
	  "\\([a-z][a-z0-9_]*\\)\\s-*"	; the value, if present
	  "\\(\\[[0-9]*\\]\\)?\\s-*"	; array qualifier, if present
	  "\\(=[^;]*\\)?;")
  "A regexp for a common kind of C binding, covering ones with two qualifiers before them.")

(defvar c-arg-regexp-1
  (concat "\\([a-z][a-z0-9_]*\\s-*"	; type
	  "\\(?:const\\s-*\\)?"
	  "\\*?\\[?\\]?\\)\\s-*"	; trimmings
	  "\\([a-z][a-z0-9_]*\\)\\s-*,?" ; name
	  "\\(\\[[0-9]*\\]\\)?\\s-*"	; array qualifier, if present
	  )
  "A regexp for a common kind of C arg.")

(defvar c-arg-regexp-2
  (concat "\\("
	  "\\(?:struct\\|unsigned\\|const\\|volatile\\)\\s-+" ; qualifier
	  "[a-z][a-z0-9_]*\\s-*"	; type
	  "\\(?:const\\s-*\\)?"
	  "\\*?\\[?\\]?"		; trimmings
	  "\\)"
	  "\\s-*\\([a-z][a-z0-9_]*\\)\\s-*,?" ; name
	  "\\(\\[[0-9]*\\]\\)?\\s-*"	; array qualifier, if present
	  )
  "A regexp for a common kind of C arg.")

(defvar c-arg-regexp-3
  (concat "\\("
	  "\\(?:const\\|volatile\\)\\s-+" ; first qualifier
	  "\\(?:struct\\|unsigned\\)\\s-+" ; second qualifier
	  "[a-z][a-z0-9_]*\\s-*"	; type
	  "\\(?:const\\s-*\\)?"
	  "\\*?\\[?\\]?"		; trimmings
	  "\\)"
	  "\\s-*"
	  "\\([a-z][a-z0-9_]*\\)\\s-*,?" ; name
	  "\\(\\[[0-9]*\\]\\)?\\s-*"	; array qualifier, if present
	  )
  "A regexp for a common kind of C arg.")


(defvar c-statement-keywords-regexp
  (concat
   "\\("
   (mapconcat 'symbol-name
	      '(do for while if return switch continue default case class)
	      "\\)\\|\\(")
   "\\)")
  "Regexp matching common C keywords.")

(defmodal identify-statement (c-mode c++-mode) (default)
  "Identify the current statement, or return DEFAULT.
We must be at the start of the statement already, otherwise
this does not have to work."
  (cond
   ((looking-at c-statement-keywords-regexp)
    (let ((keyword-string (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
      (cond
       ((string= keyword-string "if")
	(save-excursion
	  (if (and (safe-forward-sexp 2)
		   (statement)
		   (skip-to-actual-code)
		   (looking-at "else"))
	      'if-then-else
	    'if-then)))
       ((string= keyword-string "for") 'for)
       ((string= keyword-string "return") 'return)
       ((string= keyword-string "while") 'while-do)
       ((string= keyword-string "switch") 'switch)
       ((string= keyword-string "continue") 'continue)
       ((string= keyword-string "case") 'case)
       ((string= keyword-string "default") 'default)
       ((string= keyword-string "do") 'do-while)
       (t nil))))
   ((save-excursion
      (and (safe-forward-sexp 1)
	   (looking-at " *[-+*/]?= *")))
    'assignment)
   ((save-excursion
      (and (safe-forward-sexp 1)
	   (looking-at "\\(++\\)\\|\\(--\\)")))
    'assignment)
   ((looking-at "DEFUN")
    'defun)
   ((save-excursion
      (and (safe-forward-sexp 1)
	   (looking-at " *(")))
    'function-call)
   ;; todo: recognize variable declarations with and without initial values
   ((save-excursion
      (and (safe-forward-sexp 2)
	   (looking-at " *[=;] *")))
    ;; (message "vardef1")
    'variable-declaration)
   ((save-excursion
      (looking-at "[^;()=]+;"))
    ;; (message "vardef2")
    'variable-declaration)
   ((looking-at c-binding-regexp-1)
    ;; (message "vardef3")
    'variable-declaration)
   ((eq (char-after (point)) ?{)
    'progn)
   ((and (zerop (current-column))
	 (looking-at "\\(static\\|extern\\)? *\\(struct +\\)?\\*?\\([a-z][a-z0-9_]*\\)"))
    'defun)
   (t default)))

(defmodal insert-compound-statement-open (c-mode c++-mode java-mode perl-mode) ()
  "Insert a block start."
  (languide-insert "{\n"))

(defmodal compound-statement-open (c-mode c++-mode java-mode perl-mode) ()
  "Return a block start."
   "{")

(defmodal insert-compound-statement-close (c-mode c++-mode java-mode perl-mode) ()
  "Insert a block end."
  (languide-insert "\n}\n"))

(defmodal compound-statement-close (c-mode c++-mode java-mode perl-mode) ()
  "Return a block end."
  "}")

(defmodal languide-conditional-needs-unifying
  (c-mode c++-mode java-mode perl-mode)
  (from to)
  "Whether the conditional statement needs its dependent statements unified for it."
  ;; todo: can improve on this --- and it should probably take FROM and TO args
  t)

(defmodal statement-container (c-mode c++-mode perl-mode java-mode) ()
  "Move to the end of the container of the current statement."
  ;; needs to do the "not in string, not in comment" stuff, so we need
  ;; the Beginning Of Defun to compare against
  (let* ((bod (save-excursion
		(c-beginning-of-defun 1)
		(point)))
	 (in-comment-or-string t))
    ;; keep looking for the possible start of a statement, and checking that
    ;; it is not part of a comment or string
    (while in-comment-or-string
      ;; todo: this is wrong, it can find a preceding and closed container
      ;; (search-backward "{" (point-min) t) ; leaves point at end of match
      (if (safe-backward-up-list)
	  (let ((result (save-excursion (parse-partial-sexp bod (point)
							    0 ; target-depth
							    nil	; stop-before
							    nil	; state
							    nil	; stop-comment
							    ))))
	    (setq in-comment-or-string (or
					;; emacs19 doesn't give us that handy 8th element!
					(nth 8 result)

					;;					(languide-c-inside-for-control)
					;;					(if (nth 3 result) t nil)
					;;					(nth 4 result)

					))
	    (when in-comment-or-string (goto-char in-comment-or-string))
	    ;;	    (if in-comment-or-string
	    ;;		(forward-char 1))

	    )
	;; could not move out any more; set flag so we terminate now
	(setq in-comment-or-string nil)))
    ;; We are now at the opening brace (or have reached the outermost level, and were there anyway)
    (safe-scan-sexps (point) 1)))

(defun find-next-c-binding-outwards ()
  "Move to the next enclosing binding."
  (let ((binding-pattern "{")
	went)
    (while (and (setq went (outward-once))
		(not (looking-at binding-pattern))))
    (and went
	 (looking-at binding-pattern))))

(defun move-over-initializer (decls start)
  "Move over any initializer at the start of DECLS, looking at it from START.
Return the position at the end of the initializer."
  (if (string-match "^ *=" decls start)
      (let* ((l (length decls))
	     (next-comma (string-match "," decls start))
	     (next-paren (string-match "(" decls start)))
	(while (and (integerp next-comma)
		    (integerp next-paren)
		    (< next-paren next-comma))
	  (let ((level 1))
	    (setq start (1+ next-paren))
	    (while (and (< start l)
			(> 0 level))
	      (let ((c (aref decls start)))
		(if (= c open-bracket)
		    (setq level (1+ level))
		  (if (= c close-bracket)
		      (setq level (1- level)))))
	      (setq start (1+ start))))
	  (setq next-comma (string-match "," decls start)
		next-paren (string-match "(" decls start)))
	(if (integerp next-comma)
	    (1+ next-comma)
	  start))
    start))

(defmodal variables-in-scope (c-mode c++-mode) (whereat)
  "Return the list of variables in scope at WHEREAT."
  (save-excursion
    (goto-char whereat)
    (c-beginning-of-defun)
    (let ((bod (point))
	  (parse-sexp-ignore-comments t)
	  (variables nil))
      (goto-char whereat)
      ;; (message "Looking for bindings between %d and %d" bod whereat)
      (while (find-next-c-binding-outwards)
	(save-excursion
	  (down-list)
	  (skip-to-actual-code)
	  ;; (message "Looking at possible bindings at %d" (point))
	  (setq b1 nil b2 nil b3 nil)
	  (while (or (setq b1 (looking-at c-binding-regexp-1))
		     (setq b2 (looking-at c-binding-regexp-2))
		     (setq b3 (looking-at c-binding-regexp-3)))
	    ;; (message "matched %S as binding" (or (and b1 c-binding-regexp-1) (and b2 c-binding-regexp-2) (and b3 c-binding-regexp-3)))
	    (let ((vartype (match-string-no-properties 1))
		  (varname (match-string-no-properties 2))
		  (extra (match-string-no-properties 4)))
	      ;; (message "  At %d got %S of type %S" (point) varname vartype)
	      (push (cons varname vartype) variables)
	      (when (stringp extra)
		(save-match-data
		  ;; (message "  extras are %s" extra)
		  (let ((e (move-over-initializer extra 0)))
		    (while (string-match ", *\\([a-z][a-z0-9_]*\\)" extra e)
		      ;; (message "    Got extra var of same type: %s" (match-string-no-properties 1 extra))
		      (push (cons (match-string-no-properties 1 extra) vartype) variables)
		      (setq e (move-over-initializer extra (match-end 1))))))))
	    (goto-char (match-end 0))
	    ;; (message "end of that binding was %d" (point))
	    (skip-to-actual-code)
	    ;; (message "skipped to %d" (point))
	    ))
	(backward-char 1))
      ;; now include the funargs
      (goto-char bod)
      (when (and (safe-backward-sexp 1)
		 (safe-down-list 1))
	(c-forward-syntactic-ws)
	;; (message "bod at %d; trying to find args at %d" bod (point))
	(setq a1 nil a2 nil a3 nil)
	(while (or (setq a3 (looking-at c-arg-regexp-3))
		   (setq a2 (looking-at c-arg-regexp-2))
		   (setq a1 (looking-at c-arg-regexp-1)))
	  ;; (message "matched %S as arg" (or (and a1 c-arg-regexp-1) (and a2 c-arg-regexp-2) (and a3 c-arg-regexp-3)))
	  (let ((vartype (match-string-no-properties 1))
		(varname (match-string-no-properties 2)))
	    ;; (message "Adding arg %s of type %s" varname vartype)
	    (push (cons varname vartype) variables))
	  (goto-char (match-end 0))
	  ;; (message "end of match was %d" (point))
	  (c-forward-syntactic-ws)
	  ;; (message "possible start for next match is %d" (point))
	  ))
      variables)))

(defmodal variable-bindings-in-region (c-mode c++-mode java-mode)  (from to)
  "Return a list of the bindings between FROM and TO.
Each element is a list of:
  name
  type
  scope-begins scope-ends
  initial-value-as-string"
  (save-excursion
    (goto-char from)
    (c-beginning-of-defun)
    (let ((bod (point))
	  (parse-sexp-ignore-comments t)
	  (variables nil))
      (goto-char to)
      ;; (message "Looking for bindings between %d and %d" bod whereat)
      (while (and (find-next-c-binding-outwards)
		  (>= (point) from))
	(save-excursion
	  (let ((scope-begins (point))
		(scope-ends (save-excursion (safe-forward-sexp 1) (point))))
	    (down-list)
	    (skip-to-actual-code)
	    (while (or (looking-at c-binding-regexp-1)
		       (looking-at c-binding-regexp-2)
		       (looking-at c-binding-regexp-3))
	      (let* ((vartype (match-string-no-properties 1))
		     (varname (match-string-no-properties 2))
		     (extra (match-string-no-properties 3))
		     (initial-value-as-string (if extra
						  (substring extra 0 (string-match "," extra))
						"")))
		(push (list varname vartype
			    (point) scope-ends
			    initial-value-as-string)
		      variables)
		(when (stringp extra)
		  ;; (message "extra is %s" extra)
		  (let ((e 0))
		    (while (string-match ", *\\([a-z][a-z0-9_]*\\)" extra e)
		      (push (list (match-string-no-properties 1 extra) vartype
				  (point) scope-ends
				  ""	; todo: get this initial value
				  ) variables)
		      (setq e (match-end 1))))))
	      (goto-char (match-end 0))
	      (skip-to-actual-code))))
	(backward-char 1))
      variables)))

(defmodal variable-references-in-region (c-mode c++-mode java-mode) (from to)
  "Return a list of the variable references between FROM and TO.
Each element is a list of:
  name
  location"
  (let ((results nil)
	(bod (save-excursion (goto-char from) (c-beginning-of-defun) (point))))
    (save-excursion
      (goto-char from)
      (while (re-search-forward "[a-z][a-z0-9_]*" to t)
	(let ((start (match-beginning 0))
	      (name (match-string-no-properties 0))
	      (pps (save-excursion (parse-partial-sexp bod (point)))))
	  (unless (or
		   (string-match (cond
				  ;; todo: find separate one for java
				  ((eq major-mode 'java-mode) c-keywords-regexp)
				  (t c-keywords-regexp))
				 name)
		   ;; in comment or quoted:
		   (fourth pps) (fifth pps))
	    (skip-to-actual-code)
	    (unless (= (char-after) open-bracket)
	      (unless (save-excursion
			;; is it a structure member name?
			(goto-char start)
			(skip-to-actual-code-backwards)
			(or (= (char-before) ?.)
			    (and (= (char-before) ?>)
				 (= (char-before (1- (point))) ?-))))
		  (push (cons name (point)) results)))))))
    (nreverse results)))

(defmodal static-variable-p (c-mode c++-mode java-mode perl-mode) (name where)
  "Return whether a static variable called NAME is visible at WHERE."
  nil)

(defmodal move-to-enclosing-scope-last-variable-definition (c-mode c++-mode java-mode perl-mode)
  (&optional allow-conversions)
  "Move to the end of the nearest set of variable bindings.
This is the place at which you would naturally insert a new
variable, allowing for its initial value referring to any
variable already declared.
Optional arguments list names of variables needed in the definition of the new one.
This lets clever implementations put the definition as far out as possible."
  (if (find-next-c-binding-outwards)
      (let ((open (point)))
	(safe-down-list 1)
	(skip-to-actual-code)
	(while (or (looking-at c-binding-regexp-1)
		   (looking-at c-binding-regexp-2)
		   (looking-at c-binding-regexp-3))
	  (goto-char (match-end 0))
	  (skip-to-actual-code))
	(skip-to-actual-code-backwards)
	open)
    nil))

(defmodal variable-declaration-texts  (c-mode c++-mode java-mode) (name type initial-value)
  "Return the texts for a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
TYPE and INITIAL-VALUE may be null, but the NAME is required.
The result is a list of three strings: any preceding whitespace,
the actual declaration, and any following whitespace."
  (list (if (save-excursion
	      (let ((here (point)))
		(back-to-indentation)
		(= here (point))))
	    ""
	  (concat "\n" (make-string (- (save-excursion (back-to-indentation) (point))
				       (save-excursion (beginning-of-line 1) (point)))
				    ? )))
	(concat
	 (if type
	     (concat type " ")
	   "void ")
	 name
	 (if initial-value
	     (concat " = " initial-value ";")
	   ""))
	""))

(defmodal insert-variable-declaration (c-mode c++-mode java-mode) (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
Assumes we are at the obvious point to add a new variable.
TYPE and INITIAL-VALUE may be null, but the NAME is required."
  (unless (save-excursion
	    (let ((here (point)))
	      (back-to-indentation)
	      (= here (point))))
    (newline-and-indent))
  (if type
      (languide-insert type " ")
    (languide-insert "void "))
  (languide-insert name)
  (when initial-value
    (languide-insert " = " initial-value ";")))

(defmodal insert-variable-declaration (perl-mode) (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
Assumes we are at the obvious point to add a new variable.
TYPE and INITIAL-VALUE may be null, but the NAME is required."
  (unless (save-excursion
	    (let ((here (point)))
	      (back-to-indentation)
	      (= here (point))))
    (insert "\n"))
  (languide-insert "my " name)
  (when initial-value
    (languide-insert " = " initial-value ";")))

(defun c-arg-string (arg)
  "Return the string declaring ARG, which is in languide's format, which may be just the name, or (name . type)."
  (cond
   ((stringp arg) arg)			; we hope this one rarely occurs with C
   ((symbolp arg) (symbol-name arg))	; we hope this one rarely occurs with C
   ((consp arg)
    (let ((name (car arg))
	  (type (cdr arg)))
    (concat (if (stringp type)
		type
	      (if (symbolp type)
		  (symbol-name type)
		""))
	    " "
	    (if (stringp name)
		name
	      (if (symbol-name name)
		  (symbol-name name)
		"")))))))

(defmodal insert-function-declaration (c-mode c++-mode java-mode) (name result-type arglist body &optional docstring)
  "Insert a function definition for a function called NAME, returning RESULT-TYPE, taking ARGLIST, and implemented by BODY."
  (languide-insert "\n" result-type " " name)
  (let ((arglist-start (point)))
    (languide-insert "(")
    (languide-insert (mapconcat 'c-arg-string
				arglist
				", \n"))
    (languide-insert ")\n")
    (save-excursion
      (goto-char arglist-start)
      (c-indent-exp t)))
  (when docstring
    (languide-insert "  /* " docstring " */\n"))
  (let ((start (point)))
    (languide-insert "{\n")
    (languide-insert body)
    (languide-insert "\n}\n\n")
    (goto-char start)
    (c-indent-exp t)))

(defmodal insert-function-declaration (perl-mode) (name result-type arglist body)
  "Insert a function definition for a function called NAME, returning RESULT-TYPE, taking ARGLIST, and implemented by BODY."
  (languide-insert "sub " name "{\n"
	  "    my (")
  (let ((first t))
    (mapcar (lambda (arg)
	      (unless first (languide-insert " ") (setq first nil))
	      (cond
	       ((stringp arg)
		(languide-insert "$" arg))
	       ((symbolp arg)
		(languide-insert "$" (symbol-name arg)))
	       ((consp arg)
		;; todo: perl-specific clever stuff here
		)
	       ))
	    arglist))
  (languide-insert ") = @_;\n"
	  body
	  "}\n"))

(defmodal ambient-defun-name (c-mode c++-mode) (where)
  "Give the name of the function defined around WHERE."
  (save-excursion
    (goto-char where)
    (beginning-of-defun)
    (let ((end (scan-sexps (point) -1)))
      (buffer-substring-no-properties (scan-sexps end -1) end))))

(defmodal function-call-string (c-mode c++-mode java-mode perl-mode) (name arglist where)
  "Return a function call for a function called NAME taking ARGLIST. WHERE gives context."
  (let ((at-statement-start (save-excursion
			      ;; This is to see whether this is a procedure call statement,
			      ;; or a function call expression.
			      (goto-char where)
			      (skip-to-actual-code-backwards)
			      (memq (char-before) (list ?{ ?\; ?})))))
    (concat name "("
	    (mapconcat (function
			(lambda (arg)
			  (if (consp arg)
			      (car arg)
			    arg)))
		       arglist ", ")
	    ")"
	    (if at-statement-start
		";\n"
	      ""))))

(defmodal languide-find-surrounding-call (c-mode c++-mode java-mode perl-mode) ()
  "Return a list of the function call syntax around point.
Each entry is a cons of start and end positions. For most languages
there will be two or three entries, the function name, the
start-of-call or start-of-args (may be merged with the function name),
and the end-of-call or end-of-args. Separators between arguments could
also be included. The caller should treat these as coming in any
order, and being in any quantity; thus, if using them to modify the
buffer, it is usually necessary to sort them and deal with them in
descending order of character position."
  (save-excursion
    (let ((bod (save-excursion (c-beginning-of-defun) (point))))
      (catch 'found
	(while (and (safe-backward-up-list)
		    (>= (point) bod))
	  (let ((open-bracket (cons (point) (1+ (point)))))
	    (when (save-excursion
		    (backward-sexp)
		    (looking-at "\\(\\s_\\|\\sw\\)+"))
	      (let ((function-name (cons (match-beginning 0) (match-end 0))))
		(forward-sexp)
		(throw 'found (list function-name open-bracket (cons (1- (point)) (point))))))))
	nil))))

;; (defmodal function-arglist-boundaries (c-mode c++-mode java-mode perl-mode) (&optional where)
;;   "Return a cons of the start and end of the argument list surrounding WHERE,
;; or surrounding point if WHERE is not given.")

(defun find-tag-or-search (tag &optional function)
  "Find TAG, or if cannot be found through the tags table, try to find it anyway."
  (condition-case evar
      (progn
	(find-tag tag)
	t)
    (error
     (let ((old (point)))
       (goto-char (point-min))
       (if (re-search-forward (concat "^[a-z#]+.*\\s-\\*?"
				      tag (if function "\\s-*(" ""))
			      (point-max) t)
	   (progn
	     (beginning-of-line)
	     t)
	 (goto-char old)
	 nil)))))

(defun type-of-c-tag (tag)
  "Find TAG and return its type, or nil if not found."
  (save-window-excursion
    (save-excursion
      (and (find-tag-or-search tag)
	   (let ((start (point)))
	     (search-forward tag)
	     (goto-char (match-beginning 0))
	     (skip-syntax-backward "-")
	     (buffer-substring-no-properties start (point)))))))

(defun type-of-c-function (function)
  "Return the result type of FUNCTION."
  (save-window-excursion
    (save-excursion
      (and (find-tag-or-search function t)
	   (let ((start (point)))
	     (search-forward function)
	     (buffer-substring-no-properties start (match-beginning 0)))))))

(defun type-of-c-variable (variable where)
  "Return the type of VARIABLE."
  (let* ((locals (variables-in-scope where))
	 (as-local (cdr (assoc variable locals))))
    (or as-local
	(type-of-c-tag variable)
	"void")))

(defun remove-leading-expression (str)
  "Return STR with a bracketed expression removed from the front."
  (let ((i 1)
	(n (length str))
	(d 1))
    (while (and (< i n) (> d 0))
      (let ((c (aref str i)))
	(if (= c open-bracket)
	    (setq d (1+ d))
	  (if (= c close-bracket)
	      (setq d (1- d)))))
      (setq i (1+ i)))
    (substring str i)))

(defvar structure-types-cache nil
  "Alist of structure types we have looked at.
Cdrs are alists of member names to types.")

(defun modify-c-type (base-type modifier-op modifier-arg)
  "To BASE-TYPE, apply a MODIFIER-OP and MODIFIER-ARG.
The modifier can be structure accessors, etc."
  (let* ((members-of-type-pair (assoc base-type structure-types-cache)))
    (if members-of-type-pair
	(cdr (assoc modifier-arg (cdr members-of-type-pair)))
      (save-window-excursion
	(save-excursion
	  (find-tag-or-search base-type)
	  (when (looking-at "typedef\\s-+") (goto-char (match-end 0)))
	  (when (looking-at "struct\\s-+") (goto-char (match-end 0)))
	  (forward-sexp)
	  (let ((start (point))
		(end (save-excursion (forward-sexp) (point)))
		(members nil))
	    (while (re-search-forward "\\(\\(?:struct\\s-+\\)?[a-z_]+\\s-*\\*?\\)\\([a-z_]+\\)\\s-*;"
				      end t)
	      (push (cons (match-string-no-properties 2)
			  (match-string-no-properties 1))
		    members))
	    (push (cons base-type members)
		  structure-types-cache)
	    (cdr (assoc modifier-arg members))))))))

(defmodal deduce-expression-type (c-mode c++-mode java-mode) (value-text where)
  "Given VALUE-TEXT, try to deduce the type of it.
Second arg WHERE gives the position, for context."
  (cond
   ((string-match "[{;}]" value-text)
    ;; (message "\"%s\" appears to be a block, looking for return value" value-text)
    (cond
     ((string-match "return\\s-*;" value-text) "void")
     ((string-match "return\\s-\\([^;]+\\);" value-text)
      (deduce-expression-type (match-string 1 value-text)))
     (t "void")))
   ((string-match "^-?[0-9]+\\.[0-9]*$" value-text) "float")
   ((string-match "^-?[0-9]+$" value-text) "int")
   ((string-match "\".*\"" value-text)
    (cond
     ((eq major-mode 'java-mode) "String")
     ((eq major-mode 'c-mode c++-mode) "char *")))
   ((string-match "\\([a-z][a-z0-9_]*\\)(" value-text)
    ;; (message "\"%s\" appears to be a function call" value-text)
    (let* ((function-type (type-of-c-function (match-string-no-properties 1 value-text)))
	   (remainder (remove-leading-expression (substring value-text (match-end 1))))
	   (is-struct-member (string-match "\\s-*\\([-.>&]+\\)\\s-*\\(.+\\)" remainder))
	   (member-op (if is-struct-member (match-string-no-properties 1 remainder) nil))
	   (member-member (if is-struct-member (match-string-no-properties 2 remainder) nil)))
      (and member-op member-member
	   (modify-c-type function-type member-op member-member))))
   ((string-match "\\([a-z][a-z0-9_]*\\)\\s-*\\([-.>&]+\\)\\s-*\\(.+\\)" value-text)
    ;; (message "match-data %S" (match-data))

    (let* ((base-variable (match-string-no-properties 1 value-text))
	   (member-op (match-string-no-properties 2 value-text))
	   (member-member (match-string-no-properties 3 value-text))
	   (base-variable-base-type (type-of-c-variable base-variable where)))
      (and nil (message "\"%s\" appears to be a variable \"%s\" of base type %s with modifier-op \"%s\" and modifier-arg \"%s\""
			value-text
			base-variable base-variable-base-type
			member-op
			member-member))
      (modify-c-type base-variable-base-type member-op member-member)))
   ((string-match "\\([a-z][a-z0-9_]*\\)" value-text)
    ;; (message "match-data %S" (match-data))
    (let* ((base-variable (match-string-no-properties 1 value-text)))
      ;; (message "base-variable %S" base-variable)
      (type-of-c-variable base-variable where)))
   (t "unknown")))

(defun lisp-operator-to-c (lisp-operator)
  (or (cdr (assoc lisp-operator
		  '((and . "&&")
		    (or . "||")
		    (not . "!"))))
      (symbol-name operator)))

(defmodal add-expression-term (c-mode c++-mode java-mode perl-mode)
  (operator argument from to)
  "Wrap an expression with OPERATOR and ARGUMENT around the region between FROM and TO."
  (goto-char from)
  ;; todo: this "unitary" stuff isn't right yet. It's an attempt not to add unnecessary brackets
  (let ((unitary (eq (scan-sexps (1+ from) 1) (1- to))))
    (unless unitary
      (languide-insert "("))
    (goto-char (1+ to))
    (languide-insert " " (lisp-operator-to-c operator) " " argument)
    (unless unitary
      (languide-insert ")"))))

(defmodal move-before-defun (c-mode c++-mode java-mode perl-mode) ()
  "Move to before the current function definition."
  (c-mark-function))			; pollutes mark ring -- sorry

(defun is-under-control-statement (where)
  "Return whether WHERE is the start of the body of a control statement."
  (save-excursion
    (goto-char where)
    (or (and (safe-backward-sexp)
	     (looking-at "else")
	     'if-then-else)
	(and (safe-backward-sexp)
	     (looking-at "\\(if\\)\\|\\(while\\)\\|\\(for\\)")
	     (intern (match-string-no-properties 0))))))

(defmodal languide-region-type (c-mode c++-mode java-mode perl-mode) (from to)
  "Try to work out what type of thing the code between FROM and TO is.
Results can be things like if-then-body, if-then-else-tail, progn-whole,
while-do-head, defun-body, and so on. If one of these is returned, the
code must be exactly that (apart from leading and trailing
whitespace).
If it is not recognizable as anything in particular, but ends at the
same depth as it starts, and never goes below that depth in between,
that is, is something that could be made into a compound statement or
expression, return t.
Otherwise return nil.
May set languide-region-detail-string to a string giving the user incidental
information; otherwise should clear it to nil.
languide-region-detail-level says how much incidental information to include."
  (setq languide-region-detail-string nil)
  (save-excursion
    (let* ((ppe (parse-partial-sexp from to))
	   (depth (nth 0 ppe))
	   (min-depth (nth 6 ppe))
	   (in-string (nth 3 ppe))
	   (in-comment (nth 4 ppe)))
      ;; (message "depth=%d min-depth=%d" depth min-depth)
      (if (or (/= depth 0)
	      (< min-depth 0)
	      in-string
	      in-comment)
	  nil
	(let* ((possible-open-position nil)
	       (preceding-is-open (save-excursion
				    (goto-char from)
				    (skip-to-actual-code-backwards)
				    (backward-char 1)
				    (setq possible-open-position (point))
				    (looking-at (compound-statement-open))))
	       (following-is-close (save-excursion
				     (goto-char to)
				     (skip-to-actual-code)
				     (looking-at (compound-statement-close)))))
	  (if (and preceding-is-open
		   following-is-close)
	      (let ((is-under-control
		     (is-under-control-statement possible-open-position)))
		;; we have got a whole compound statement
		(if is-under-control
		    (intern (concat "compound-" (symbol-name is-under-control) "-body"))
		  'compound-statement))
	    ;; not a whole compound statement, first see whether it is
	    ;; one statement or several whole statements
	    (let* ((real-start (save-excursion
				 (goto-char from)
				 (skip-to-actual-code)))
		   (real-end (save-excursion
			       (goto-char to)
			       (skip-to-actual-code-backwards)))
		   )
	      (let ((is-single-statement (save-excursion
					   (goto-char real-start)
					   (next-statement-internal 1)
					   (skip-to-actual-code-backwards)
					   (= (point) real-end)))
		    (is-under-bare-control (is-under-control-statement real-start)))
		;; (message "single=%S under-bare-control=%S" is-single-statement is-under-bare-control)
		(if is-single-statement
		    (if is-under-bare-control
			(intern (concat (symbol-name is-under-bare-control) "-body"))
		      ;; now see whether this statement is the sole contents of a bracketed block
		      (let ((surrounding-open nil))
			(if (save-excursion
			      (and (progn
				     (goto-char real-start)
				     (skip-to-actual-code-backwards)
				     (backward-char 1)
				     (setq surrounding-open (point))
				     (looking-at (compound-statement-open)))
				   (progn
				     (goto-char real-end)
				     (skip-to-actual-code)
				     (looking-at (compound-statement-close)))))
			    (let ((is-under-bracketed-control (is-under-control-statement surrounding-open)))
			      ;; (message "surrounding-open=%d" surrounding-open)
			      (if is-under-bracketed-control
				  (intern (concat (symbol-name is-under-bracketed-control) "-body"))
				'sole-content-of-block))
			  (let ((type (save-excursion
					(goto-char real-start)
					(identify-statement nil))))
			    (when type
				(setq languide-region-detail-string (format "%S" type))))
			  'whole-statement)))
		  ;; not single statement
		  (if is-under-bare-control
		      ;; sequence of statements, but the first one is
		      ;; only the body of a control statement
		      nil
		    ;; now see whether the start and end are whole statements
		    (if (save-excursion
			  (let ((hint (progn
					(goto-char (1+ real-start))
					(beginning-of-statement-internal))))
			    (and (= (point) real-start)
				 (progn
				   (goto-char (1- real-end))
				   (end-of-statement-internal hint)
				   (= (point) real-end)))))
			'sequence
		      nil)))))))))))

(defstatement comment (c-mode c++-mode)
  "Comment"
  (head "/\\* *")
  (body "/\\* *" (upto " *\\*/"))
  (tail " *\\*/")
  (create (template "/* " r " */")))

(defstatement comment (java-mode)
  "Comment"
  (head "// *")
  (body "/\\* *" (upto " *$"))
  (tail "$")
  (create (template "// " r n)))

(defstatement comment (perl-mode)
  "Comment"
  (head "# *")
  (body "# *" (upto " *$"))
  (tail "$")
  (create (template "# " r n)))

(defstatement comment (c-mode c++-mode)
  "Comment"
  (head "/\\* *")
  (body "/\\* *" (upto " *\\*/"))
  (tail " *\\*/")
  (create (template "/* " r " */")))

(defstatement progn (c-mode c++-mode java-mode perl-mode)
  "Sequential execution statement."
  (head "{")
  (body "{" (statements)))

(defstatement if-then (c-mode c++-mode java-mode perl-mode)
  "If statement without else clause."
  (head "if" (expression-contents))
  (body "if" (expression) (statement-contents))
  (add-head (template & > "if (" r ")" n>))
  (framework (remember "if") (remember "(") (expressions) (remember ")")
	     (skip-to-actual-code) (continue-if "{")
	     (remember "{") (expressions) (remember "}"))
  (create (template & > "if (" p ") {" n>
		    r "}"))
  (begin-end "if () {" end "}")
  (begin-end-with-dummy "if (1) {" end "}"))

(defstatement if-then-else (c-mode c++-mode java-mode perl-mode)
  "If statement with else clause."
  (head "if" (expression-contents))
  (body "if" (expression) (statement-contents))
  (tail "if" (expression) (statement) "else" (statement-contents))
  (framework (remember "if") (remember "(") (expressions) (remember ")")
	     (skip-to-actual-code)
	     (if "{"
		 ((remember "{") (statements) (remember "}"))
	       (statement))
	     (remember "else")
	     (skip-to-actual-code)
	     (continue-if "{")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "if (" p ") {" n>
		    r "} else {"n>
		    p "}"))
  (begin-end "if () {" "} else {}")
  (begin-end-with-dummy "if (1) {" "} else {}"))

(defstatement while-do (c-mode c++-mode java-mode perl-mode)
  "While statement."
  (head "while" (expression-contents))
  (body "while" (expression) (statement-contents))
  (framework (remember "while") (remember "(") (expressions) (remember ")")
	     (skip-to-actual-code) (continue-if "{")
	     (remember "{") (expressions) (remember "}"))
  (create (template & > "while (" p ") {" n>
		    r "}"))
  (begin-end "while () {" "}")
  (begin-end-with-dummy "while (1) {" "}"))

(defstatement do-while (c-mode c++-mode java-mode perl-mode)
  "Do-While statement."
  (head "do" (statement) "while" (expression))
  (body "do" (statement-contents))
  (framework (remember "do")
	     (remember "(") (expression) (remember ")")
	     (skip-to-actual-code) (continue-if "{")
	     (remember "{") (expressions) (remember "}"))
  (create (template & > "do {" r "} while (" p ")" n>)))

(defstatement for (c-mode c++-mode java-mode perl-mode)
  "For statement."
  (head "for" (expression-contents))
  (body "for" (expression) (statement-contents))
  (framework (remember "for") (remember "(") (expressions) (remember ")")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "for (" p ";" p ";" p ") {" n>
		    r "}")))

(defstatement switch (c-mode c++-mode java-mode)
  "Switch statement"
  (head "switch" (expression-contents))
  (body "switch" (expression) (statement-contents))
  (framework (remember "switch") (remember "(") (expressions) (remember ")")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "switch (" p ";" p ";" p ") {" n>
		    r "}")))

(defstatement defun (c-mode c++-mode java-mode)
  "Function definition"
  (head (upto "{"))
  (body "{" (statements))
  (framework)
  (create (template & >  (p "Function result type: ") (p "Function name: ")
		    "("  (p "Function argument: ") ")"
		    n>
		    "{" n>
		    r
		    "}" n))
  (begin-end "(" ")\n{\n}"))

(defstatement defun (perl-mode)
  "Function definition"
  (head "sub" (expression-contents))
  (body "sub" (expression) (expression-contents))
  (framework (remember "sub") (remember "{") (statements) (remember "}"))
  (create (template & > "sub " (p "Function name: ")
		    n>
		    "{" n>
		    r
		    "}" n))
  (begin-end "sub {" "}"))

(defstatement variable-declaration (perl-mode)
  "My variables"
  (head "my (" (expressions))
  (body "my" (expression) (expressions))
  (framework (remember "my") (remember "(") (expressions) (remember ")"))
  (create (template & > "my (" p ")" n)))

(defstatement variable-declaration (c-mode c++-mode java-mode)
  "Local variable"
  ;; todo: recognize local variables with and without initial values, as separate statement types
  (head "[;=]" (start-of-match) (from-start-of-statement))
  (body "[;=]" (if (looking-at "=") (upto ";") fail))
  (framework (remember "=") (remember ";"))
  (create (template & > " " p ";" n)))

(defstatement assignment (perl-mode c-mode c++-mode java-mode)
  "Assignment"
  (head "=" (start-of-match) (from-start-of-statement))
  (body "=" (upto ";"))
  (framework (remember "=") (remember ";"))
  (create (template & > (p "Variable: ") " = " r ";")))

(defstatement function-call (perl-mode c-mode c++-mode java-mode)
  "Function call"
  (head (expression))
  (body (expression) (expression-contents))
  (framework (remember "(") (expressions) (remember ")"))
  (create (template & > (p "Function name: ") "(" r ")")))

(defstatement return (c-mode c++-mode java-mode)
  "Return, with optional result"
  (head "return" (start-of-match) (from-start-of-statement))
  (body "return" (upto ";"))
  (framework (remember "return") (expression) (remember ";"))
  (create (template & > "return" r ";")))

(defstatement and (perl-mode c-mode c++-mode java-mode)
  "And expression."
  (begin-end "( " " && )")
  (begin-end-with-dummy "( " " && 1)"))

(defstatement or (perl-mode c-mode c++-mode java-mode)
  "Or expression."
  (begin-end "(" " || )")
  (begin-end-with-dummy "(" " || 0)"))

(defstatement not (perl-mode c-mode c++-mode java-mode)
  "Not expression."
  (begin-end "(!" ")"))

(defstatement class (c++-mode
		     ;; todo: is the java syntax the same?
		     java-mode)
  "Class definition."
  (head "class" (expression))
  (body "class"
	"{" "public:" (upto "private:"))
  (tail "class" "{" "private:" (expressions)))

;; now define the whole statements, systematically
(mapcar (lambda (mode)
	  (let ((statements (get mode 'statements)))
	    (mapcar (lambda (statement)
		      (unless (assoc 'whole (cdr statement))
			(rplacd statement
				(cons '(whole statement-navigate 
					      statement ; (up-to-end-of-statement)
					      )
				      (cdr statement)))))
		    statements)))
	'(perl-mode c-mode c++-mode java-mode))

(provide 'languide-c-like)

;;; end of languide-c-like.el
