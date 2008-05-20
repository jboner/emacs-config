;;;; languide-sh-like.el -- shell etc definitions for language-guided editing
;;; Time-stamp: <2007-08-11 14:43:48 jcgs>
;;
;; Copyright (C) 2004, 2006, 2007  John C. G. Sturdy
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

(require 'cl)
(require 'modal-functions)

(defmodal move-into-previous-statement (sh-mode) ()
  "Move into the previous statement.
This need be valid only after a beginning-of-statement-internal.
It should move point back such that another beginning-of-statement-internal
will go back another statement."
)

(defmodal move-into-next-statement (sh-mode) ()
  "Move into the next statement.
This need be valid only after an end-of-statement-internal.
It should move point forward such that another end-of-statement-internal
will go forward another statement."
  (forward-char 1)			; todo: this is probably wrong
)

(defmodal beginning-of-statement-internal (sh-mode) ()
  "Move to the beginning of the statement.
Do not do auxiliary stuff that might be associated with this."
  (interactive)
  (let ((bol (line-beginning-position))
	(semicolon (if (search-backward ";" nil t)
		       (match-end 0)
		     (point-min))))
    (goto-char (max bol semicolon))
    (skip-to-actual-code)
    nil))

(defmodal end-of-statement-internal (sh-mode) (hint)
  "Move to the end of the current statement.
Do not do auxiliary stuff that might be associated with this."
  (interactive)
  (message "Starting by going to beginning from %d" (point))
  (beginning-of-statement-internal)
  (let ((type (identify-statement nil)))
    (message "Statement type at %d is %S" (point) type)
    (cond
     ((memq type '(if-then if-then-else switch for while))
      (let ((end (compound-statement-close)))
	(message "Is compound, moving over contained statements until %s" end)
	;; get onto first substatement
	(cond
	 ;; todo: these searches should be special ones that skip comments, etc
	 ((memq type '(for while)) (re-search-forward "\\<do\\>"))
	 ((eq type 'if) (re-search-forward "\\<then\\>")))
	(skip-to-actual-code)
	(while (not (looking-at end))
	  (next-statement-internal 1)
	  (message "Moved forward a statement, now looking at %s..." (buffer-substring-no-properties (point) (+ 24 (point))))
	  (skip-to-actual-code)
	  )
	(goto-char (match-end 0)))
      
      )
     ((null type)
      (let ((eol (line-end-position))
	    (semicolon (save-excursion (or (search-forward ";" nil t)
					   (point-max)))))
	(goto-char (min eol semicolon))
	)))))

(defmodal identify-statement (sh-mode) (default)
  "Note what kind of statement we are at the start of.
Need not work if not at the start of a statement.
If the statement cannot be identified, return DEFAULT."
  (cond
   ((looking-at "[a-z_]+\\s-*=") 'assignment)
   ((looking-at "case") 'switch)
   ((looking-at "if")
    'if-then				; todo: spot if-then-else
    )
   ((looking-at "while") 'while)
   ((looking-at "for") 'for)
   ((looking-at "exit") 'return)
   ((looking-at "shift") 'shift)
   ((looking-at "export") 'export)
   ((looking-at "[-a-z0-9_*.$]+).*$") 'case)
   (t default)
   )
)

(defmodal compound-statement-open (sh-mode) ()
  "Return a block start."
)

(defmodal compound-statement-close (sh-mode) ()
  "Return a block end."
  "\\(fi\\)\\|\\(done\\)\\|\\(esac\\)"
)

(defmodal insert-compound-statement-open (sh-mode) ()
  "Insert the start of a compound statement"
)

(defmodal insert-compound-statement-close (sh-mode) ()
  "Insert the end of a compound statement"
)

(defmodal binding-around (sh-mode) (whereat)
  "Return the shell variable definition around WHEREAT."
  (save-match-data
    (save-excursion
      (goto-char whereat)
      (beginning-of-line 1)
      (if (looking-at "^\\(export +\\)?\\([^=]+\\)=\\(.+\\)$")
	  (list (match-string 2)
		(match-string 3)
		(match-beginning 2) (match-end 2)
		(match-beginning 3) (match-end 3))
	nil))))

(defmodal scope-around (sh-mode) (whereat)
  "Return the scope (start and end) around WHEREAT."
  (save-excursion
    (let* ((start (safe-scan-lists whereat -1 1))
	   (end
	    (if start
		(progn
		  (goto-char start)
		  (forward-sexp 1)
		  (point))
	      (point-max))))
      (list (if start start (point-min))
	    end))))

(defmodal variable-reference (sh-mode) (varname)
  "Make a variable reference from VARNAME"
  (concat "$" varname))

(defstatement comment (c-mode)
  "Comment"
  (head "#")
  (body "#" (upto "\n"))
  (create (template "# " r "\n")))

(defstatement if-then (sh-mode)
  "If statement without else clause."
  (head "if" (upto "then"))
  (body "if" (upto "then") "then" (statements))
  (framework (remember "if") (remember "then")
	     (skip-to-actual-code) (continue-if "{")
	     (remember "{") (expressions) (remember "}"))
  (create (template & > "if " p n>
		    "then\n"
		    r n> "\nfi\n"))
  (begin-end "if \nthen\n" "\nfi\n")
  (begin-end-with-dummy "if true\nthen\n" end "\nfi\n"))

(defstatement if-then-else (sh-mode)
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

(defstatement while-do (sh-mode)
  "While statement."
  (head "while" (expression-contents))
  (body "while" (expression) (statement-contents))
  (create (template & > "while (" p ") {" n>
		    r "}"))
  (begin-end "while () {" "}")
  (begin-end-with-dummy "while (1) {" "}"))

(defstatement for (sh-mode)
  "For statement."
  (head "for" (expression-contents))
  (body "for" (expression) (statement-contents))
  (framework (remember "for") (remember "(") (expressions) (remember ")")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "for (" p ";" p ";" p ") {" n>
		    r "}")))

(defstatement switch (sh-mode)
  "Switch statement"
  (head "switch" (expression-contents))
  (body "switch" (expression) (statement-contents))
  (framework (remember "switch") (remember "(") (expressions) (remember ")")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "switch (" p ";" p ";" p ") {" n>
		    r "}")))

(defstatement variable-declaration (sh-mode)
  "Local variable"
  ;; todo: recognize local variables with and without initial values, as separate statement types
  (head "=" (start-of-match) (from-start-of-statement))
  (body "=" (upto ";"))
  (framework (remember "=") (remember ";"))
  (create (template & > " " p ";" n)))

(defstatement assignment (sh-mode)
  "Assignment"
  (head "=" (start-of-match) (from-start-of-statement))
  (body "=" (upto ";"))
  (framework (remember "=") (remember ";"))
  (create (template & > (p "Variable: ") " = " r ";")))

(provide 'languide-sh-like)

;;; end of languide-sh-like.el
