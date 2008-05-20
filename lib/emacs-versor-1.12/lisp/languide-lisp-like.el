;;;; languide-lisp-like.el -- Lisp, Elisp, Scheme definitions for language-guided editing
;;; Time-stamp: <2007-12-31 20:37:39 jcgs>
;;
;; Copyright (C) 2007, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2004
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; Copyright (C) 2004, 2005, 2006, 2007  John C. G. Sturdy
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

;;; Commentary:
;; Lisp, Elisp, Scheme definitions for language-guided editing.  This
;; makes the modal definitions for those languages' modes, as used to
;; implement the defmodel'ed functions in languide-bindings.el,
;; languide-edits.el, languide.el, modal-functions.el,
;; statement-navigation.el, versor-alter-item.el, and
;; versor-base-moves.el.

(require 'cl)
(require 'modal-functions)

;;; Code:
(defmodal beginning-of-statement-internal (lisp-mode
					   emacs-lisp-mode
					   lisp-interaction-mode
					   scheme-mode)
  ()
  "Move to the beginning of Lisp statement, which is a pretty nebulous concept."
  ;;;;;;;;;;;;;;;; wrong -- this effectively does previous-statement
  (unless (looking-at "(") (backward-up-list 1))
  (forward-sexp -1)
  nil)

(defmodal end-of-statement-internal (lisp-mode
				     emacs-lisp-mode
				     lisp-interaction-mode
				     scheme-mode)
  (hint)
  "Move to the end of Lisp statement."
  ;;;;;;;;;;;;;;;; wrong -- this effectively does next-statement
  (unless (looking-at "(") (backward-up-list 1))
  (forward-sexp 1))

(defvar lisp-mode-statement-identities
  '(("when" . if-then)
    ("if" . if-then-else)
    ("setq" . assignment)
    ("let" . variable-declaration)
    ("let*" . variable-declaration)
    ("progn" . progn)
    ("defun" . defun))
  "Alist of Lisp functors which are recognized by languide.
Maps strings to symbols.")

(defmodal identify-statement (lisp-mode emacs-lisp-mode lisp-interaction-mode) (default)
  "Identify a Lisp form or function."
  (if (looking-at "(\\([-:_a-z0-9]+\\)")
      (let ((string (match-string-no-properties 1)))
	(or (cdr (assoc string lisp-mode-statement-identities))
	    'function-call))
    default))

(defmodal insert-compound-statement-open (lisp-mode
					  emacs-lisp-mode
					  lisp-interaction-mode)
  ()
  "Insert a progn."
  (languide-insert "(progn\n "))

(defmodal insert-compound-statement-close (lisp-mode
					   emacs-lisp-mode
					   lisp-interaction-mode)
  ()
  "Insert a progn's closing bracket."
  (languide-insert ")"))

(defmodal languide-conditional-needs-unifying
  (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode) (from to)
  "Whether the conditional statement needs its dependent statements unified for it."
  nil)

(defun find-next-lisp-binding-outwards (&optional allow-conversions)
  "Move to the next enclosing binding.

With optional ALLOW-CONVERSIONS, include places that could be
converted to binding places.

Returns the position if it found one, or nil otherwise."
  (interactive)
  (let ((binding-pattern (if allow-conversions
			     "\\((let\\*?\\\\)\\|\\((progn\\)>"
			   "(let\\*?\\>")))
    (while (and (outward-once)
		(not (looking-at binding-pattern))))
    (if (looking-at binding-pattern)
	(point)
      nil)))

(defmodal variables-in-scope (lisp-mode emacs-lisp-mode lisp-interaction-mode) (whereat)
  "Return the alist list of variables in scope at WHEREAT."
  ;; todo: make this spot lambda bindings too
  ;; todo: make it not spot literals
  ;; todo: make it not spot keywords
  (save-excursion
    (goto-char whereat)
    (beginning-of-defun)
    (let ((bod (point))
	  next
	  (parse-sexp-ignore-comments t)
	  (variables nil))
      (goto-char whereat)
      ;; first, check we are not in the preamble of a let form:
      (when (or (looking-at "let\\*?")
		(save-excursion
		  (and (safe-backward-sexp)
		       (looking-at "let\\*?"))))
	(safe-backward-up-list 1))
      ;; now, see whether we are in the bindings list of a let* form:
      (let ((possible-current-binding (save-excursion
					(safe-backward-up-list 1)
					(safe-backward-sexp 1)
					(and (looking-at "let\\*?")
					     (match-string-no-properties 0)))))
	(cond
	 ((string= "let*" possible-current-binding)
	  ;; (message "Got some of our own")
	  (while (setq next (safe-scan-sexps (point) -1))
	    (goto-char next)
	    (when (looking-at "(") (forward-char) (skip-to-actual-code))
	    (let ((start (point)))
	      (forward-sexp)
	      (push (list (buffer-substring-no-properties start (point)))
		    variables))
	    (goto-char next))
	  ;; avoid getting the same lot again
	  (safe-backward-up-list 2)
	  )
	 ((string= "let" possible-current-binding)
	  ;; (message "Avoiding this bunch")
	  ;; the bindings we are among are not in scope here, so get out from among them
	  (safe-backward-up-list 2)))
	)
      ;; (message "Looking for bindings between %d and %d" bod whereat)
      (while (find-next-lisp-binding-outwards)
	(save-excursion
	  (down-list 2)
	  (skip-to-actual-code)
	  ;; (message "Entering binding list got us to %d" (point))
	  (while (setq next (safe-scan-sexps (point) 1))
	    (skip-to-actual-code)
	    ;; (message "Looking at %s as binding" (buffer-substring-no-properties (point) next))
	    (when (looking-at "(") (forward-char) (skip-to-actual-code))
	    (let ((start (point)))
	      (forward-sexp)
	      (push (list (buffer-substring-no-properties start (point)))
		    variables))
	    (goto-char next))))
      ;; got the let-forms, now get the funargs
      (goto-char bod)
      (down-list)
      (when (looking-at "def[um]")
	(when (looking-at "defmod[ae]l") (forward-sexp))
	(forward-sexp 2)
	(let ((limit (safe-scan-sexps (point) 1)))
	  (down-list)
	  (skip-to-actual-code)
	  ;; (message "Arglist starts at %d" (point))
	  (let ((old (point))
		new)
	    (while (and (setq new (safe-scan-sexps old 1))
			(< new limit))
	      (push (list (buffer-substring-no-properties old new)) variables)
	      (goto-char new)
	      (skip-to-actual-code limit)
	      (setq old (point))))))
      variables)))

(defmodal adapt-binding-point (lisp-mode emacs-lisp-mode lisp-interaction-mode) (&optional allow-conversions)
  "Make a binding point suitable to receive another binding."
  (let ((binding-place (point)))
    (save-excursion
      (outward-once)
      (if (looking-at "(defun")
	  (progn
	    (goto-char binding-place)
	    (languide-insert-before-markers "(let (")
	    (save-excursion ; so we finish at new variable insertion point
	      (languide-insert ")\n")
	      (let ((old (point))
		    new)
		(while (setq new (safe-scan-sexps old 1)) ; move over body forms
		  (setq old new))
		(goto-char old))
	      (languide-insert ")")
	      (backward-up-list)
	      (indent-sexp)))
	(outward-once)
	(cond
	 ((looking-at "\\((let\\)[^*]")
	  ;; todo: add further condition, that one of the variables needed is bound in this set?
	  (save-excursion
	    (replace-match "\\1*" t nil nil 1)))
	 ((and allow-conversions
	       (looking-at "(\\(progn))"))
	  (save-excursion
	    (replace-match "let* ()" t t nil 1))))))))

(defmodal move-to-enclosing-scope-last-variable-definition
  (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (&optional allow-conversions)
  "Move to the end of the nearest set of variable bindings.
This is the place at which you would naturally insert a new
variable, allowing for its initial value referring to any
variable already declared.

It may create a suitable place if there is none; for example, in Lisp
it could wrap the outermost forms of a \"defun\" with a \"let\".

Optional argument ALLOW-CONVERSIONS allows conversion of possible
scoping points into actual ones. For example, in lisp, this means a
\"progn\" can be changed to a \"let\".

Returns the start of the binding if it found a binding, nil if it went to the top level
of the defun."
  (let ((binding-point (find-next-lisp-binding-outwards allow-conversions)))
    (if binding-point
	(progn
	  (down-list 2)
	  (let ((old (point))
		new)
	    (while (setq new (safe-scan-sexps old 1))
	      (setq old new))
	    (goto-char old))
	  binding-point)
      ;; no let forms, so position ourselves to make a new one at the
      ;; outer level of the defun
      (down-list)
      (forward-sexp 3)			; over "defun", name, args
      (skip-to-actual-code)
      (when (= (char-after) ?\")
	(forward-sexp 1)		; over docstring
	(skip-to-actual-code))
      (when (looking-at "(interactive")
	(forward-sexp 1)
	(skip-to-actual-code))
      nil)))

(defmodal variable-last-possibly-assigned-before (lisp-mode emacs-lisp-mode lisp-interaction-mode) (variable wherebefore)
  "Return the position of the most recent assignment to VARIABLE before WHEREBEFORE."
  (save-excursion
    (goto-char wherebefore)
    (let ((bod (save-excursion (beginning-of-defun) (point))))
      (catch 'got-assignment
	(while (search-backward "(setq" bod t)
	  (let* ((first-name (match-end 0))
		 (end (progn (forward-sexp) (1- (point)))))
	    (goto-char first-name)
	    (catch 'scan-setq
	      (while t
		(skip-to-actual-code)
		(cond
		 ((>= (point) end) (throw 'scan-setq nil))
		 ((string= (buffer-substring-no-properties (point)
							   (save-excursion (forward-sexp) (point)))
			   variable)
		  (throw 'got-assignment (point)))
		 (t (forward-sexp) (forward-sexp))))
	      nil)))
	(goto-char wherebefore)
	(when (re-search-backward
	       (format "(\\(rplaca\\|rplacd\\|incf\\|decf\\) +\\(%s\\)" variable)
	       bod t)
	  (throw 'got-assignment (point)))))))

(defmodal variable-declaration-texts (lisp-mode emacs-lisp-mode lisp-interaction-mode) (name type initial-value)
  "Return the texts for a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
TYPE and INITIAL-VALUE may be null, but the NAME is required.
The result is a list of three strings: any preceding whitespace,
the actual declaration, and any following whitespace."
  (let* ((preceded-by-bracket (= (char-before) close-bracket))
	 (followed-by-bracket (= (char-after) open-bracket)))
    (message "%d is %s %s" (point) (if preceded-by-bracket "preceded-by-bracket" "") (if followed-by-bracket "followed-by-bracket" ""))
    (list
     (concat
      (if preceded-by-bracket
	  "\n" "")
      (let ((indent (calculate-lisp-indent)))
	(make-string (+ 6 (cond
			   ((integerp indent) indent)
			   ((consp indent) (car indent))
			   4))
		     ? )))
     (concat "(" name " " initial-value ")")
     (if followed-by-bracket
	 "\n" ""))))

(defmodal insert-variable-declaration (lisp-mode emacs-lisp-mode lisp-interaction-mode) (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
Assumes we are at the obvious point to add a new variable.
TYPE and INITIAL-VALUE may be null, but the NAME is required."
  (let* ((preceded-by-bracket (= (char-before) open-bracket))
	 (followed-by-bracket (= (char-after) close-bracket)))
    (unless preceded-by-bracket
      (insert "\n"))
    (lisp-indent-line)
    (save-excursion
      (languide-insert "(" name " " initial-value ")")
      (unless followed-by-bracket
	(insert "\n")
	(lisp-indent-line)))
    (indent-sexp)))

(defmodal insert-global-variable-declaration
  (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (name type initial-value)
  "Insert a definition for a global variable called NAME, of TYPE, with INITIAL-VALUE.
TYPE and INITIAL-VALUE may be null, but the NAME is required.
In fact, TYPE is not meaningful as this is the definition for Lisp."
  (languide-insert "\n(defvar " name " " initial-value "\n  \"\")\n"))

(defun arg-name (arg)
  "Return just the name of ARG, which is in languide's format, which may be just the name, or (name . type)."
  (cond
   ((stringp arg) arg)
   ((symbolp arg) (symbol-name arg))
   ((and (consp arg)
	 (stringp (car arg)))
    (car arg))
   ((and (consp arg)
	 (symbolp (car arg)))
    (symbol-name (car arg)))))

(defun lisp-arglist-elements (arglist)
  "Insert the elements of ARGLIST.
They are in languide's format, which may be just the name, or (name . type)."
  (mapconcat 'arg-name arglist " "))

(defmodal insert-function-declaration (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (name result-type arglist
	body &optional docstring)
  "Insert a function definition for NAME, returning RESULT-TYPE, taking ARGLIST, and implemented by BODY.
A DOCSTRING may also be given."
  (languide-insert "(defun " name " (")
  (languide-insert (lisp-arglist-elements arglist))
  (languide-insert ")")
  (newline-and-indent)
  (when (and (stringp docstring)
	     (not (zerop (length docstring))))
    (languide-insert "\"" docstring "\"")
    (newline-and-indent))
  (languide-insert body ")\n\n")
  (beginning-of-defun)
  (indent-sexp))

(defmodal function-call-string (lisp-mode emacs-lisp-mode lisp-interaction-mode) (name arglist where)
  "Return a string for a function call to NAME with ARGLIST. WHERE gives context."
  (concat "(" (lisp-arglist-elements (cons name arglist)) ")"))

(defmodal ambient-defun-name (lisp-mode emacs-lisp-mode lisp-interaction-mode) (where)
  "Give the name of the function defined around WHERE."
  (save-excursion
    (goto-char where)
    (beginning-of-defun)
    (let* ((end (scan-sexps (scan-lists (point) 1 -1) 2)))
      (buffer-substring-no-properties (scan-sexps end -1) end))))

(defmodal languide-find-surrounding-call (lisp-mode emacs-lisp-mode lisp-interaction-mode) ()
  "Return a list of the function call syntax around point.
Each entry is a cons of start and end positions. For most languages
there will be two or three entries, the function name, the
start-of-call or start-of-args (may be merged with the function name),
and the end-of-call or end-of-args. Separators between arguments could
also be included. The caller should treat these as
coming in any order, and being in any quantity; thus, if using them to
modify the buffer, it is usually necessary to sort them and deal with
them in descending order of character position."
  (save-excursion
    (let ((bod (save-excursion (beginning-of-defun) (point))))
      (catch 'found
	(while (and (safe-backward-up-list)
		    (>= (point) bod))
	  (let ((open-bracket (cons (point) (1+ (point)))))
	    (when (save-excursion
		    (down-list)
		    (skip-to-actual-code)
		    (looking-at "\\(\\s_\\|\\sw\\)+"))
	      (let ((function-name (cons (match-beginning 0) (match-end 0))))
		(forward-sexp)
		(throw 'found (list function-name open-bracket (cons (1- (point)) (point))))))))
	nil))))

(defmodal languide-trim-whitespace (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (syntax-before syntax-after)
  "Trim whitespace around point, in a language-dependent way.
The syntax classes of the non-space chars around point are passed in
as SYNTAX-BEFORE and SYNTAX-AFTER."
  ;; do this in two stages, as the second can happen after the first has
  (cond
   ((save-excursion
      (beginning-of-line)
      ;; look for closing brackets with only whitespace before them on the line
      (looking-at "^\\(\\s-*\\))"))
    (delete-region (1- (match-beginning 1)) (match-end 1))))
  (cond
   ((save-excursion
      (beginning-of-line)
      ;; if on a blank line...
      (looking-at "^\\s-*$"))
    (delete-blank-lines)
    (delete-blank-lines)
    (when (looking-at "^(")
      ;; todo: make this use defun-prompt-regexp
      ;; if at beginning of defun, put blank line before it
      (open-line 1)))
   ((and (eq syntax-before close-bracket)
	 (eq syntax-after open-bracket))
    ;; only one space between two bracketted expressions
    ;; todo: make this more general about spaces between expressions
    ;; todo: make it leave two spaces until point moves away? and even make that a dummy expression until then?
    (just-one-space))
   ((or (and (eq syntax-before open-bracket)
	     ;; remove space after open bracket
	     (memq syntax-after
		   '(open-bracket ?w ?_)))
	(and (eq syntax-after close-bracket)
	     ;; remove space before close bracket
	     (memq syntax-before
		   '(close-bracket ?w ?_))))
    (delete-horizontal-space))
   ((and (eq syntax-before open-bracket)
	 (eolp))
    ;; for open bracket at end of line, bring next line up
    (delete-char 1)
    (delete-horizontal-space))))

;; (defmodal function-arglist-boundaries (lisp-mode emacs-lisp-mode lisp-interaction-mode)
;;   (&optional where)
;;   "Return a cons of the start and end of the argument list surrounding WHERE,
;; or surrounding point if WHERE is not given.")

(defmodal deduce-expression-type
  (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (value-text where)
  "Given VALUE-TEXT, try to deduce the type of it."
  nil)					; nice and easy for dynamically typed languages!

(defmodal add-expression-term (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  (operator argument from to)
  "Wrap an expression with OPERATOR and ARGUMENT around the region between FROM and TO."
  (goto-char to)
  (languide-insert ")")
  (goto-char from)
  (languide-insert "(" (symbol-name operator) " " argument " "))

(defmodal move-before-defun (lisp-mode emacs-lisp-mode lisp-interaction-mode) ()
  "Move to before the current function definition."
  (beginning-of-defun 1))

(defmodal static-variable-p (lisp-mode emacs-lisp-mode lisp-interaction-mode) (name where)
  "Return whether a static variable called NAME is visible at WHERE."
  (or (member name '("nil" "t"))
      (documentation-property (intern name) 'variable-documentation)
      (save-excursion
	(goto-char (point-min))
	(re-search-forward (format "(\\(\\(defvar\\)\\|\\(defconst\\)\\|\\(defcustom\\)\\) +%s" name)
			   (point-max) t)
	;; todo: should also look in everything we load with require
	)))

(defmodal variable-bindings-in-region (lisp-mode emacs-lisp-mode lisp-interaction-mode) (from to)
  "Return a list of the bindings between FROM and TO.
Each element is a list of:
  name
  type
  scope-begins scope-ends
  initial-value-as-string"
  (save-excursion
    (let ((result nil))
      (goto-char from)
      (while (re-search-forward "\\((lambda\\>\\)\\|\\((let\\*?\\)" to t)
	(let* ((keystart (safe-scan-lists (match-beginning 0) 1 -1))
	       (keyend (safe-scan-sexps keystart 1))
	       (bindings-end (save-excursion
			       (forward-sexp 1)
			       (point)))
	       (scope-end (save-excursion
			    (backward-up-list)
			    (forward-sexp 1)
			    (1- (point)))))
	  (goto-char keystart)
	  (cond
	   ((looking-at "lambda")
	    (let* ((names-start (safe-scan-lists keyend 1 -1))
		   (names-end (1- (safe-scan-sexps (1- names-start) 1)))
		   (name-start names-start)
		   (name-end (safe-scan-sexps name-start 1)))
	      (while (and name-end (<= name-end names-end))
		(push (list (buffer-substring-no-properties name-start name-end)
			    nil
			    keyend scope-end
			    nil)
		      result)
		(goto-char name-end)
		(skip-to-actual-code)
		(setq name-start (point)
		      name-end (safe-scan-sexps name-start 1)))))
	   ((looking-at "let\\*?\\>")
	    (let ((star (looking-at "let\\*")))
	      (goto-char keyend)
	      (down-list)		; into the bindings
	      (let* ((binding-start (point))
		     (binding-end nil))
		(while (setq binding-end (safe-scan-sexps binding-start 1))
		  (goto-char binding-start)
		  (down-list)
		  (let* ((name-start (point))
			 (name-end (safe-scan-sexps name-start 1))
			 (name-string (buffer-substring-no-properties name-start name-end))
			 (value-end (safe-scan-sexps name-end 1))
			 (value-start (if value-end
					  (safe-scan-sexps value-end -1)
					nil))
			 (value-string (if (and value-start value-end)
					   (buffer-substring-no-properties value-start value-end)
					 nil)))
		    (push (list name-string
				nil
				(if star value-end bindings-end) scope-end
				value-string)
			  result))
		  (setq binding-start binding-end))))))
	  (goto-char bindings-end)))
      (nreverse result))))

(defconst open-bracket (string-to-char "(")
  "Get this out-of-line to avoid confusing indenter when editing functions that use it.")

(defmodal variable-references-in-region (lisp-mode emacs-lisp-mode lisp-interaction-mode) (from to)
  "Return a list of the variable references between FROM and TO.
Each element is a list of:
  name
  location"
  ;; todo: exclude quoted constants (including those inside lists)
  (save-excursion
    (beginning-of-defun)
    (let ((result nil)
	  (bod (point)))
      (goto-char from)
      (while (re-search-forward "\\<[-a-z][-a-z-0-9_]*\\>" to t)
	(let* ((where (match-beginning 0))
	       (end (match-end 0))
	       (preceding-char (char-after (1- where)))
	       (pps (parse-partial-sexp bod where)))
	  (if (and (not (= preceding-char open-bracket))
		   (not (= preceding-char ?'))
		   (not (= preceding-char ?:))
		   (not (fourth pps))	; inside string
		   (not (fifth pps)))	; inside comment
	      (push (list (match-string-no-properties 0) where) result))
	  (goto-char end)))
      result)))

(defun count-sexps (from to)
  "Return the number of sexps between FROM and TO."
  (let ((i 0))
    (while (and from
		(< from to))
      (setq from (safe-scan-sexps from 1)
	    i (1+ i)))
    (if from
	i
      (1- i))))

(defvar languide-lisp-form-functors
  '((quote . quote-statement)
    (let . let-statement)
    (let* . let*-statement)
    (lambda . lambda-statement)
    (cond . cond-statement)
    (setq . assignment-statement)
    (if . if-then-else-statement)
    (when . if-then-statement)
    (unless . if-else-statement)
    (while . while-statement)
    (dolist . dolist-statement)
    (dotimes . dotimes-statement)
    (defun . function-definition)
    (defsubst . function-definition)
    (defmacro . macro-definition)
    (defvar . variable-definition)
    (defcustom . variable-definition))
  "Alist of special form names to languide statement types.
Any not listed in here are regarded as funcalls.")

(defvar languide-lisp-defunoids
  '(defun defmacro defvar defconst defcustom)
  "Defun and similar form names.
Languide uses this to tell whether something is a definition form.")

(defmodal languide-region-type (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (from to)
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
  (cond
   ((save-excursion (goto-char from)
		    ;; this tests whether the selection is a single,
		    ;; non-bracketed, expression
		    (skip-to-actual-code to)
		    (and (not (looking-at "("))
			 (save-excursion
			   (backward-char)
			   (skip-to-actual-code-backwards)
			   (backward-char)
			   (not (looking-at "(")))
			 (progn
			   (forward-sexp)
			   (let ((expr-1-end (point)))
			     (goto-char to)
			     (skip-to-actual-code-backwards from)
			     (eq (point) expr-1-end)))))
    ;; Now try to classify or describe the non-bracketed single expression
    (let* ((start-char (char-after from))
	   (start-syntax (char-syntax start-char))
	   (surround-start (safe-scan-lists from -1 1))
	   (f-start (and surround-start
			 (safe-scan-lists surround-start 1 -1)))
	   (f-end (and f-start
		       (safe-scan-sexps f-start 1)))
	   (functor (and f-end
			 (intern
			  (buffer-substring-no-properties f-start f-end))))
	   (which-member (and f-start (count-sexps f-start from))))
      (cond
       ((eq start-char ?\")
	(when (> languide-region-detail-level 1)
	  (setq languide-region-detail-string (format "contents: \"%s\""
						      (buffer-substring-no-properties
						       (1+ from)
						       (1- (safe-scan-sexps from 1))))))
	(if (or (and (memq functor languide-lisp-defunoids)
		     (eq which-member 4))
		(and (eq functor 'defmodal)
		     (eq which-member 5)))
	    'documentation
	  'string-literal))
       ((eq start-char ??)
	(when (> languide-region-detail-level 1)
	  (let ((charval))
	    (setq languide-region-detail-string (format "value: %c; code: %x"
							charval charval))))
	'character-literal)
       ((and (>= start-char ?0) (<= start-char ?9))
	'numeric-constant)
       ((or (eq start-syntax ?w)
	    (eq start-syntax ?_))
	(cond
	 ((and (memq functor languide-lisp-defunoids)
	       (eq which-member 2))
	  'defun-name)
	 ((and (memq functor '(when unless))
	       (eq which-member 2))
	  'if-condition)
	 ((eq functor 'if)
	  (cond
	   ((eq which-member 2) 'if-else-condition)
	   ((eq which-member 3) 'if-else-body)
	   ((eq which-member 4) 'if-else-tail)))
	 (t 'symbol))))))
   (t (let* ((pps (save-excursion (parse-partial-sexp from to))))
	(cond
	 ((and (zerop (nth 0 pps))	; same level at both ends
	       (>= (nth 6 pps) 0))	; no dip in level between ends
	  ;; we are on a bracketed expression or a series of expressions
	  (let* ((n-parts (count-sexps from to))
		 (f-start (safe-scan-lists from 1 -1))
		 (f-end (and f-start
			     (safe-scan-sexps f-start 1)))
		 (functor (and f-end
			       (not (= (char-after f-start) open-bracket))
			       (intern
				(buffer-substring-no-properties f-start f-end))))
		 (surrounding-start (safe-scan-lists from -1 1))
		 (surrounding-end (and surrounding-start
				       (safe-scan-sexps surrounding-start 1)))
		 (sf-start (and surrounding-start
				(safe-scan-lists surrounding-start 1 -1)))
		 (sf-end (and sf-start
			      (safe-scan-sexps sf-start 1)))
		 (surrounding-functor (and sf-end
					   (not (= (char-after sf-start) open-bracket))
					   (intern
					    (buffer-substring-no-properties sf-start sf-end))))
		 (s-members (and sf-start
				 surrounding-end
				 (count-sexps sf-start (1- surrounding-end))))
		 (which-s-member (and sf-start
				      (count-sexps sf-start from))))
	    ;; (message "functor %S; surrounding-functor %S, of which we are %S..%S of %S" functor surrounding-functor which-s-member (1- (+ which-s-member n-parts)) s-members)
	    (cond
	     ((eq surrounding-functor nil)
	      ;; could be an individual let binding, or something like
	      ;; that, but is likely not to be a common or garden code sexp
	      (let* ((sursurrounding-start (safe-scan-lists surrounding-start -1 1))
		     (ssf-start (and sursurrounding-start
				     (safe-scan-lists sursurrounding-start 1 -1)))
		     (ssf-end (and sf-start
				   (safe-scan-sexps ssf-start 1)))
		     (sursurrounding-functor (and ssf-end
						  (not (= (char-after ssf-start) open-bracket))
						  (intern
						   (buffer-substring-no-properties ssf-start ssf-end)))))
		(cond
		 ((eq sursurrounding-functor 'cond)
		  (if (eq which-s-member 0)
		      'cond-condition
		    ;; (message "cond body which=%S n=%S members=%S" which-s-member n-parts s-members)
		    (if (and (numberp which-s-member)
			     (= which-s-member 2)
			     (= n-parts (- s-members 1)))
			(progn
			  (when (> languide-region-detail-level 0)
			    (setq languide-region-detail-string (format "%d parts" n-parts)))
			  'cond-body)
		      t)))
		 ((memq sursurrounding-functor '(let let*))
		  'variable-binding)
		 (t nil))))
	     ((eq which-s-member 0)
	      'functor)
	     ((eq surrounding-functor 'cond)
	      (if (= n-parts 1)
		  'cond-clause
		'cond-clauses))
	     ((eq functor 'defun) defun-body)
	     ((and (memq surrounding-functor languide-lisp-defunoids)
		   (eq which-s-member 3))
	      'defun-arglist)
	     ((and (memq surrounding-functor '(let let* flet))
		   (or (and (eq which-s-member 2)
			    (eq n-parts 1))
		       (and (= which-s-member 3)
			    (= n-parts (- s-members 2)))))
	      (cond
	       ((eq which-s-member 2)
		(if (eq n-parts 1)
		    (let ((this-binding f-start)
			  (n-bindings 0))
		      (while (setq this-binding (safe-scan-sexps this-binding 1))
			(setq n-bindings (1+ n-bindings)))
		      (when (> languide-region-detail-level 0)
			(setq languide-region-detail-string (format "%d bindings" n-bindings)))
		      'let-bindings)
		  nil))
	       ((and (= which-s-member 3)
		     (= n-parts (- s-members 2)))
		(progn
		  (when (> languide-region-detail-level 0)
		    (setq languide-region-detail-string (format "%d parts" n-parts)))
		  'let-body))
	       (t t)))
	     ;; todo: lots more to do here
	     ((memq functor '(progn save-excursion save-window-excursion))
	      (when (> languide-region-detail-level 0)
		;; this one isn't right yet
		;; (setq languide-region-detail-string (format "%d parts" n-parts))
		)
	      'progn-whole)
	     ((memq surrounding-functor '(when unless))
	      (if (eq which-s-member 2)
		  'if-condition
		(if (and (= which-s-member 3)
			 (= n-parts (- s-members 2)))
		    (progn
		      (when (> languide-region-detail-level 0)
			(setq languide-region-detail-string (format "%d parts" n-parts)))
		      'if-then-body)
		  t)))
	     ((and (eq surrounding-functor 'if)
		   (numberp which-s-member))
	      (cond
	       ((= which-s-member 2)
		(if (= n-parts 1)
		    'if-condition
		  nil))
	       ((= which-s-member 3)
		(if (= n-parts 1)
		    'if-body
		  nil))
	       ((and (= which-s-member 4)
		     (= n-parts (- s-members 3)))
		(progn
		  (when (> languide-region-detail-level 0)
		    (setq languide-region-detail-string (format "%d parts" n-parts)))
		  'if-then-else-tail))
	       (t t)))
	     ((and (memq surrounding-functor '(progn save-excursion save-window-excursion))
		   (= n-parts (1- s-members)))
	      'progn-body)
	     (t
	      (if (= n-parts 1)
		  (or (cdr (assoc functor languide-lisp-form-functors))
		      'funcall-statement)
		t)
	      ))))
	 (t
	  (message "scraps %S..%S" from to)
	  ;; we are on something that is neither an expression nor a
	  ;; series of expressions
	  (when (> languide-region-detail-level 1)
	    (cond
	     ((zerop (nth 0 pps))
	      (setq languide-region-detail-string "Not the same level at both ends"))
	     ((>= (nth 6 pps) 0)
	      (setq languide-region-detail-string "Dips in the middle"))
	     (t nil)))
	  nil))))))

(defmodal adjust-binding-point (lisp-mode emacs-lisp-mode lisp-interaction-mode) (variables-needed)
  "If appropriate, move to the first point at which all of VARIABLES-NEEDED are defined.
Assumes being at the end of a group of bindings, ready to insert a binding."
  ;; iterating back over the variables of this binding group
  (message "adjust-binding-point from %d" (point))
  (while (and (safe-backward-sexp)
	      (let ((this (save-excursion
			    ;; go into the binding to see what variable is there
			    ;; todo: make this conditional on the binding being a list rather than just a symbol
			    (down-list)
			    (let ((start (point)))
			      (forward-sexp) ; get the other end of the variable name
			      ;; (message "considering %s" (buffer-substring-no-properties start (point)))
			      ;; if the variable is one of the once we need, hand back its location
			      (if (not (member (buffer-substring-no-properties start (point))
					       variables-needed))
				  nil
				(point))))))
		(if (null this)
		    t ; this was not one of the variables we were looking for, so carry on
		  ;; it was one of the ones we are interested in
		  (goto-char this) ; go to the end of the variable name
		  (up-list)		; then out of that binding
		  nil))))
  (message "adjust-binding-point to %d" (point)))

(defstatement comment (lisp-mode
		       emacs-lisp-mode
		       lisp-interaction-mode)
  "Comment"
  (head ";+ *")
  (body ";+ *" (upto " *$"))
  (tail "$")
  (create (template "; " r n)))

(defstatement defun (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  ;; (keyword "defun")
  (head "(defun +" (expression-contents 2))
  (body "(defun +" (expression 2) (skip-to-actual-code) (expressions))
  (create (template & "(defun " (p "Function name to define: ")
		    " (" (p "Argument list: ") ")" n>
		    "\"" (p "Documentation string: ") "\"" n>
		    r> ")" n))
  (begin-end ("(defun " . "()\n \"\"\n") ")"))

(defstatement defvar (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  ;; (keyword "defvar")
  (head "(defvar +" (expression-contents 2))
  (body "(defvar +" (expression 2) (skip-to-actual-code) (expressions))
  (create (template "(defvar " (p "Variable name to define: ")
		    "\"" (p "Documentation string: ") "\"" n>
		    r> ")" n)))

(defstatement progn (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "General compound statement for Lisps"
  ;; (keyword "progn")
  (head "(progn" (expression-contents))
  (body "(progn" (skip-to-actual-code) (expressions))
  (create (template & > "(progn " p n>
		    r ")"))
  (begin-end "(progn\n" ")"))

(defstatement save-excursion (emacs-lisp-mode lisp-interaction-mode)
  ""
  ;; (keyword "save-excursion")
  (head "(save-excursion" (expression-contents))
  (body "(save-excursion" (skip-to-actual-code) (expressions))
  (create (template & > "(save-excursion " p n>
		    r ")"))
  (begin-end "(save-excursion\n" ")"))

(defstatement save-window-excursion (emacs-lisp-mode lisp-interaction-mode)
  "Emacs-lisp \"save-window-excursion\" special form"
  ;; (keyword "save-window-excursion")
  (head "(save-window-excursion" (expression-contents))
  (body "(save-window-excursion" (skip-to-actual-code) (expressions))
  (create (template & > "(save-window-excursion " p n>
		    r ")"))
  (begin-end "(save-window-excursion\n" ")"))

(defstatement condition-chain (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  ;; (keyword "cond")
  (head "(cond *")
  (body "(cond *" (skip-to-actual-code) (expressions))
  (create (template & > "(cond" n> "(" p ")" n> "(t (" p " " p ")))"))
  (begin-end ("(cond\n(" . "(") ")))"))

(defstatement function-call (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  ""
  (head "(" (expression))
  (body "(" (expression) (skip-to-actual-code) (expressions))
  (create (template > "(" p ")"))
  (begin-end "(" ")"))

(defstatement variable-declaration (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  ;; (keyword "let")
  (head "(let *(" (expressions))
  (body "(let *" (expression) (skip-to-actual-code) (expressions))
  (framework
   (remember "(") (remember "let") (remember "(") (expressions) (remember ")")
   (expressions) (remember ")"))
  (create
   (template & > "(let ((" (p "Variable name: ") p "))" n> r n> ")"))
  (begin-end ("(let ((" . "))\n ") ")"))

(defstatement assignment (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "Assignment statement"
  ;; (keyword "setq")
  (head "(setq" (expression-contents))
  (body "(setq" (expression) (skip-to-actual-code) (expression))
  (create (template & > "(setq " (p "Variable name: ") " "
		    r ")"))
  (begin-end ("(setq " . " ") ")"))

(defstatement if-then (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "If statement without else clause."
  ;; (keyword "when")
  (head "(when" (expression-contents))
  (body "(when" (expression) (skip-to-actual-code) (expressions))
  (add-head (template & > "(when " r n>))
  (add-trailer (template ")" >))
  (create (precondition (require 'cl))
	  (template & > "(when " p n>
		    r ")"))
  (begin-end ("(when " . "\n") ")")
  (begin-end-with-dummy "(when true " "\n)"))

(defstatement if-then-else (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "If statement with else clause."
  ;; (keyword "if")
  (head "(if" (expression))
  (body "(if" (expression) (skip-to-actual-code) (expression))
  (tail "(if" (expression) (expression) (skip-to-actual-code) (expressions))
  (add-head (template & > "(if " r n>))
  (add-trailer (template ")" >))
  (create (template & > "(if " p n>
		    r n>
		    p ")"))
  (begin-end ("(if " . "\n") ")")
  (begin-end-with-dummy "(if true " ")"))

(defstatement unless (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "Emacs-lisp \"unless\" special form."
  ;; (keyword "unless")
  (head "(unless" (expression-contents))
  (body "(unless" (expression) (skip-to-actual-code) (expression-contents))
  (create (precondition (require 'cl))
	  (template & > "(unless " p n>
		    r ")"))
  (begin-end ("(unless " . "\n") ")")
  (begin-end-with-dummy "(unless false " ")"))

(defstatement while-do (emacs-lisp-mode lisp-interaction-mode)
  "Emacs-lisp \"while\" special form"
  ;; (keyword "while")
  (head "(while" (expression-contents))
  (body "(while" (expression) (skip-to-actual-code) (expressions))
  (add-head (template & > "(while " r n>))
  (add-trailer (template ")" >))
  (create (template & > "(while " p n>
		    r ")"))
  (begin-end ("(while ". "\n") ")")
  (begin-end-with-dummy "(while true " ")"))

(defstatement and (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "And expression."
  ;; (keyword "and")
  (begin-end "(and " ")")
  (begin-end-with-dummy "(and true " ")"))

(defstatement or (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "Or expression."
  ;; (keyword "or")
  (begin-end "(or " ")")
  (begin-end-with-dummy "(or false " ")"))

(defstatement not (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "Not expression."
  ;; (keyword "not")
  (begin-end "(not " ")"))

;; Now put a "framework" and a "whole" in place for each statement
;; type -- these are the same for all lisp "statements":
(mapcar (lambda (mode)
	  (let ((statements (get mode 'statements)))
	    (mapcar (lambda (statement)
		      (unless (assoc 'framework (cdr statement))
			(rplacd statement
				(cons '(framework (remember "(")
						  (remember (expression))
						  (expressions)
						  (remember ")"))
				      (cdr statement))))
		      (unless (assoc 'whole (cdr statement))
			(rplacd statement
				(cons '(whole (expression))
				      (cdr statement)))))
		    statements)))
	'(lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode))

(provide 'languide-lisp-like)

;;; end of languide-lisp-like.el

(provide 'languide-lisp-like)

;;; languide-lisp-like.el ends here
