;;;; languide-python.el -- languide definitions for python
;;; Time-stamp: <2007-12-11 17:27:24 jcgs>

;; Copyright (C) 2007, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007?
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

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


;;; Commentary:
;; 

(require 'cl)
(require 'modal-functions)

;;; Code:
(defmodal move-into-previous-statement (python-mode) ()
  "Move into the previous python statement.
Need only work if already at or just before the start of a statement."
  (skip-to-actual-code-backwards)
  (backward-char 1))

(defmodal move-into-next-statement (python-mode) ()
  "Move into the next python statement.
Need only work if already at or just beyond the end of a statement."
  (skip-to-actual-code)
  (forward-char))

(defmodal beginning-of-statement-internal (python-mode) ()
  "Move to the beginning of a python statement."
  (py-goto-initial-line)
  (back-to-indentation)
  nil)

(defmodal previous-statement-internal (python-mode) (n)
  "Move to the NTH previous statement.
If calling this from a program, other than inside statement navigation,
you should possibly use previous-statement instead.
There an element of DWIM to this:
  If not at the beginning of the statement, move to the beginning of it.
  If already at the beginning, move to the beginning of the previous one.
Then, if N is greater than 1, move back N-1 more statements."
  (py-goto-initial-line)
  (back-to-indentation)
  (let ((this-indent (current-indentation)))
    (while (>= n 1)
      (forward-line -1)
      (back-to-indentation)
      (while (or (> (current-indentation) this-indent)
		 (looking-at "else:\\|elif\\|except:"))
	(forward-line -1)
	(back-to-indentation))
      (setq n (1- n))))
  (save-excursion
    (py-end-of-statement)
    (point)))

(defmodal end-of-statement-internal (python-mode) (hint)
  "Move to the end of a python statement."
  ;; (py-goto-beyond-final-line)
  (py-end-of-statement)
  (skip-to-actual-code-backwards))

(defmodal identify-statement (python-mode) (default)
  "Identify the current statement, or return DEFAULT.
We must be at the start of the statement already, otherwise
this does not have to work."
  (cond
   ((looking-at "if\\|elif\\|print\\|return\\|def\\|for\\except\\|while\\|for\\|def")
    (let ((keyword-string (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
      (cond
       ((or (string= keyword-string "if")
	    (string= keyword-string "elif"))
	(save-excursion
	  (py-mark-block nil t)
	  (skip-to-actual-code)
	  (if (looking-at "else:\\|elif\\|except:")
	      'if-then-else
	    'if-then)))
       ((string= keyword-string "for") 'for)
       ((string= keyword-string "return") 'return)
       ((string= keyword-string "while") 'while-do)
       ((string= keyword-string "pass") 'continue)
       ((string= keyword-string "def") 'defun)
       (t nil))))
   ((save-excursion
      (and (safe-forward-sexp 1)
	   (looking-at " *[-+*/]?= *")))
    'assignment)
   ((save-excursion
      (and (safe-forward-sexp 1)
	   (looking-at "\\(++\\)\\|\\(--\\)")))
    'assignment)
   ((save-excursion
      (and (safe-forward-sexp 1)
	   (looking-at " *(")))
    'function-call)
   ((looking-at "\"\"\"")
    'docstring)
   ;; todo: recognize variable declarations with and without initial values
   (t default)))

(defmodal insert-compound-statement-open (python-mode) ()
  "Insert a block start.")

(defmodal compound-statement-open (python-mode) ()
  "Return a block start."
  "")

(defmodal insert-compound-statement-close (python-mode) ()
  "Insert a block end.")

(defmodal compound-statement-close (python-mode) ()
  "Return a block end."
  "")

(defmodal languide-conditional-needs-unifying (python-mode) ()
  "Whether the conditional statement needs its dependent statements unified for it."
  t)

(defmodal statement-container (python-mode) ()
  "Move to the end of the container of the current statement."
  (py-goto-block-up)
  (py-end-of-statement)
  (point))

(defmacro py-add-var-names (vars)
  "Helper macro for `variables-in-scope'.  VARS is the accumulator variable."
  `(let ((varnames (split-string (match-string-no-properties 1) "[, ]+")))
     (while varnames
       (unless (assoc (car varnames) ,vars)
	 (setq ,vars (cons (list (car varnames)) ,vars)))
       (setq varnames (cdr varnames)))))

(defmodal variables-in-scope (python-mode) (whereat)
  "Return the list of variables in scope at WHEREAT."
  (save-excursion
    (goto-char whereat)
    (py-beginning-of-def-or-class nil)
    (let ((vars nil)
	  (bod (point)))
      (goto-char whereat)
      (while (re-search-backward "^\\s-+\\([a-z][a-z0-9_ ,]*\\)=[^=]" bod t)
	(py-add-var-names vars))
      (goto-char bod)
      (when (re-search-forward "(\\([^)]+\\)):" (line-end-position) t)
	(py-add-var-names vars))
      vars)))

(defmodal variable-bindings-in-region (python-mode)  (from to)
  "Return a list of the bindings between FROM and TO.
Each element is a list of:
  name
  type
  scope-begins scope-ends
  initial-value-as-string"
  (save-excursion
    (let ((bindings nil))
      (goto-char from)
      (while (re-search-forward
	      ;; "^\\s-+\\([a-z][a-z0-9_ ,]*\\)=[^=]\\s-*\\([^#]*\\)\\(#.+\\)?$"
	      "^\\s-+\\([a-z][a-z0-9_ ,]*\\)=[^=]\\s-*\\(.*\\)$"
	      to t)
	(message "got %S %S" (match-string-no-properties 1) (match-string-no-properties 2))
	(let* ((vars (save-match-data
		      (split-string (match-string-no-properties 1) "[, ]+")))
	      (values (split-string (match-string-no-properties 2) "[, ]+")))
	  (while vars
	    (unless (assoc (car vars) bindings)
	      (setq bindings (cons (list (car vars) nil (point) to (car values))
				   bindings)))
	    (setq vars (cdr vars)
		  values (cdr values)))))
      bindings)))

(defmodal variable-references-in-region (python-mode) (from to)
  "Return a list of the variable references between FROM and TO.
Each element is a list of:
  name
  location"
  (save-excursion
    (let ((references nil))
      (goto-char from)
      (while (re-search-forward "[^.a-z_]\\([a-z][a-z_0-9]*\\)[^(a-z_]" to t)
	(let ((ref (match-string-no-properties 1)))
	  (message "%s %s?" ref (get-text-property (point) 'face))
	  (unless (or (py-in-literal)
		      (member ref references)
		      (string-match "\\`\\(and\\|assert\\|break\\|class\\|continue\\|def\\|del\\|elif\\|else\\|except\\|exec\\|for\\|from\\|global\\|if\\|import\\|in\\|is\\|lambda\\|not\\|or\\|pass\\|print\\|raise\\|return\\|while\\)\\'" ref))
	    (setq references (cons ref references)))))
      references)))

(defmodal static-variable-p (python-mode) (name where)
  "Return whether a static variable called NAME is visible at WHERE."
  nil)

(defmodal move-to-enclosing-scope-last-variable-definition (python-mode)
  (&optional allow-conversions)
  "Move to the end of the nearest set of variable bindings.
This is the place at which you would naturally insert a new
variable, allowing for its initial value referring to any
variable already declared.
Optional arguments list names of variables needed in the definition of the new one.
This lets clever implementations put the definition as far out as possible."
  (beginning-of-line))

(defmodal variable-declaration-texts (python-mode) (name type initial-value)
  "Return the texts for a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
TYPE and INITIAL-VALUE may be null, but the NAME is required.
The result is a list of three strings: any preceding whitespace,
the actual declaration, and any following whitespace."
  (list (make-string (current-indentation) ? )
	(format "%s = %s" name initial-value)
	"\n"))

(defmodal insert-variable-declaration (python-mode) (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
Assumes we are at the obvious point to add a new variable.
TYPE and INITIAL-VALUE may be null, but the NAME is required."
  (open-line 1)
  (py-indent-line)
  (insert name " = " initial-value))

(defmodal insert-function-declaration (python-mode) (name result-type arglist body &optional docstring)
  "Insert a function definition for a function called NAME, returning RESULT-TYPE, taking ARGLIST, and implemented by BODY.")

(defmodal ambient-defun-name (python-mode) (where)
  "Give the name of the function defined around WHERE."
  (save-excursion
    (py-beginning-of-def-or-class)
    (forward-sexp 2)
    (let ((end (point)))
      (backward-sexp 1)
      (buffer-substring-no-properties (point) end))))

(defmodal function-call-string (python-mode) (name arglist where)
  "Return a function call for a function called NAME taking ARGLIST. WHERE gives context."
  (concat name "("
	  (mapconcat (function
		      (lambda (arg)
			(if (consp arg)
			    (car arg)
			  arg)))
		     arglist ", ")
	  ")"))

(defmodal languide-find-surrounding-call (python-mode) ()
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
    (let ((bod (save-excursion (py-beginning-of-def-or-class) (point))))
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

(defmodal deduce-expression-type (python-mode) (value-text where)
  "Given VALUE-TEXT, try to deduce the type of it.
Second arg WHERE gives the position, for context."
  nil)

(defmodal add-expression-term (python-mode)
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

(defmodal move-before-defun (python-mode) ()
  "Move to before the current function definition."
  (py-beginning-of-def-or-class)
  (forward-line -1))

(defmodal languide-region-type (python-mode) (from to)
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
  nil)

(defstatement comment (python-mode)
  "Comment"
  (head (python-docstring))
  (create))

(defstatement progn (python-mode)
  "Sequential execution statement."
  (head)
  (body))

(defstatement if-then (python-mode)
  "If statement without else clause."
  (head "if\\s-+" (rest-of-line))
  (body "if\\s-+" (forward-line) (block-at-this-depth))
  (add-head)
  (adjust-body (lambda (from to) (indent-rigidly from to py-indent-offset)))
  (framework (remember "if") (remember ":"))
  (create)
  (begin-end)
  (begin-end-with-dummy))

(defstatement if-then-else (python-mode)
  "If statement with else clause."
  (head "if\\s-+" (rest-of-line))
  (body "if\\s-+" (forward-line) (block-at-this-depth))
  (tail "if\\s-+" (forward-line) (block-at-this-depth)
	"else:\\|elif" (forward-line) (block-at-this-depth))
  (add-head)
  (adjust-body (lambda (from to) (indent-rigidly from to py-indent-offset)))
  (framework (remember "if") (remember ":")
	     (forward-line) (block-at-this-depth)
	     (forward-line) (remember "else:"))
  (create)
  (begin-end)
  (begin-end-with-dummy))

(defstatement while-do (python-mode)
  "While statement."
  (head "while\\s-+" (rest-of-line))
  (body "while\\s-+" (forward-line) (block-at-this-depth))
  (add-head)
  (adjust-body (lambda (from to) (indent-rigidly from to py-indent-offset)))
  (framework (remember "while") (remember ":"))
  (create)
  (begin-end)
  (begin-end-with-dummy))

(defstatement do-while (python-mode)
  "Do-While statement."
  (head)
  (body)
  (create))

(defstatement for (python-mode)
  "For statement."
  (head "for\\s-+" (rest-of-line))
  (body "for\\s-+" (forward-line) (block-at-this-depth))
  (framework (remember "for") (remember "in") (remember ":"))
  (add-head)
  (adjust-body (lambda (from to) (indent-rigidly from to py-indent-offset)))
  (create))

(defstatement defun (python-mode)
  "Function definition"
  (head "def" (expression 2))
  (body "def" (expression 2) ":" (forward-line) (block-at-this-depth))
  (framework (remember "def") (remember ":"))
  (create)
  (begin-end))

(defstatement variable-declaration (python-mode)
  "Local variable"
  (head)
  (body)
  (framework)
  (create))

(defstatement assignment (python-mode)
  "Assignment"
  (head "=" (start-of-match) (from-start-of-statement))
  (body "=\\s-*" (rest-of-line))
  (framework "=")
  (create))

(defstatement function-call (python-mode)
  "Function call"
  (head (expression))
  (body (expression) (expression-contents))
  (framework (remember "(") (expressions) (remember ")"))
  (create (template & > (p "Function name: ") "(" r ")")))

(defstatement return (python-mode)
  "Return, with optional result"
  (head "return" (start-of-match) (from-start-of-statement))
  (body "return" (rest-of-line))
  (framework (remember "return") (expression))
  (create (template & > "return" r)))

(defstatement and (python-mode)
  "And expression."
  (begin-end)
  (begin-end-with-dummy))

(defstatement or (python-mode)
  "Or expression."
  (begin-end)
  (begin-end-with-dummy))

(defstatement not (python-mode)
  "Not expression."
  (begin-end))

(provide 'languide-python)

;;; end of languide-python.el

(provide 'languide-python)

;;; languide-python.el ends here
