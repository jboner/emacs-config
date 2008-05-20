;;;; languide-functional.el -- languide definitions for functional languages such as Haskell
;;; Time-stamp: <2007-12-11 17:27:24 jcgs>

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

(provide 'languide-functional)
(require 'modal-functions)

(defmodal move-into-previous-statement (haskell-mode literate-haskell-mode) ()
  "Move into the previous Haskell statement.
Need only work if already at or just before the start of a statement.")

(defmodal move-into-next-statement (haskell-mode literate-haskell-mode) ()
  "Move into the next Haskell statement.
Need only work if already at or just beyond the end of a statement.")

(defmodal beginning-of-statement-internal (haskell-mode literate-haskell-mode) ()
  "Move to the beginning of a Haskell statement."
  nil)

(defmodal end-of-statement-internal (haskell-mode literate-haskell-mode) (hint)
  "Move to the end of a Haskell statement.")

(defmodal identify-statement (haskell-mode literate-haskell-mode) (default)
  "Identify the current statement, or return DEFAULT.
We must be at the start of the statement already, otherwise
this does not have to work."
  (cond
   ((looking-at "module\\|import\\|data\\|instance")
    (intern (match-string-no-properties 0)))
   ((looking-at "\\(\\<[a-z]+\\>\\)\\s-+::")
    'type-signature)
   ((looking-at "\\(\\<[a-z]+\\>\\)\\s-+[^=]+=")
    'defun)))

(defmodal insert-compound-statement-open (haskell-mode literate-haskell-mode) ()
  "Insert a block start.")

(defmodal compound-statement-open (haskell-mode literate-haskell-mode) ()
  "Return a block start.")

(defmodal insert-compound-statement-close (haskell-mode literate-haskell-mode) ()
  "Insert a block end.")

(defmodal compound-statement-close (haskell-mode literate-haskell-mode) ()
  "Return a block end.")

(defmodal languide-conditional-needs-unifying (haskell-mode literate-haskell-mode) ()
  "Whether the conditional statement needs its dependent statements unified for it.")

(defmodal statement-container (haskell-mode literate-haskell-mode) ()
  "Move to the end of the container of the current statement.")

(defmodal variables-in-scope (haskell-mode literate-haskell-mode) (whereat)
  "Return the list of variables in scope at WHEREAT.")

(defmodal variable-bindings-in-region (haskell-mode literate-haskell-mode)  (from to)
  "Return a list of the bindings between FROM and TO.
Each element is a list of:
  name
  type
  scope-begins scope-ends
  initial-value-as-string")

(defmodal variable-references-in-region (haskell-mode literate-haskell-mode) (from to)
  "Return a list of the variable references between FROM and TO.
Each element is a list of:
  name
  location")

(defmodal static-variable-p (haskell-mode literate-haskell-mode) (name where)
  "Return whether a static variable called NAME is visible at WHERE.")

(defmodal move-to-enclosing-scope-last-variable-definition (haskell-mode literate-haskell-mode)
  (&optional allow-conversions)
  "Move to the end of the nearest set of variable bindings.
This is the place at which you would naturally insert a new
variable, allowing for its initial value referring to any
variable already declared.
Optional arguments list names of variables needed in the definition of the new one.
This lets clever implementations put the definition as far out as possible.")

(defmodal variable-declaration-texts (haskell-mode literate-haskell-mode) (name type initial-value)
  "Return the texts for a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
TYPE and INITIAL-VALUE may be null, but the NAME is required.
The result is a list of three strings: any preceding whitespace,
the actual declaration, and any following whitespace.")
  
(defmodal insert-variable-declaration (haskell-mode literate-haskell-mode) (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
Assumes we are at the obvious point to add a new variable.
TYPE and INITIAL-VALUE may be null, but the NAME is required.")

(defmodal insert-function-declaration (haskell-mode literate-haskell-mode) (name result-type arglist body &optional docstring)
  "Insert a function definition for a function called NAME, returning RESULT-TYPE, taking ARGLIST, and implemented by BODY.")

(defmodal ambient-defun-name (haskell-mode literate-haskell-mode) (where)
  "Give the name of the function defined around WHERE.")

(defmodal function-call-string (haskell-mode literate-haskell-mode) (name arglist where)
  "Return a function call for a function called NAME taking ARGLIST. WHERE gives context.")

(defmodal languide-find-surrounding-call (haskell-mode literate-haskell-mode) ()
  "Return a list of the function call syntax around point.
Each entry is a cons of start and end positions. For most languages
there will be two or three entries, the function name, the
start-of-call or start-of-args (may be merged with the function name),
and the end-of-call or end-of-args. Separators between arguments could
also be included. The caller should treat these as coming in any
order, and being in any quantity; thus, if using them to modify the
buffer, it is usually necessary to sort them and deal with them in
descending order of character position.")

(defmodal deduce-expression-type (haskell-mode literate-haskell-mode) (value-text where)
  "Given VALUE-TEXT, try to deduce the type of it.
Second arg WHERE gives the position, for context.")

(defmodal add-expression-term (haskell-mode literate-haskell-mode)
  (operator argument from to)
  "Wrap an expression with OPERATOR and ARGUMENT around the region between FROM and TO.")

(defmodal move-before-defun (haskell-mode literate-haskell-mode) ()
  "Move to before the current function definition.")

(defmodal languide-region-type (haskell-mode literate-haskell-mode) (from to)
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
languide-region-detail-level says how much incidental information to include.")

(defstatement comment (haskell-mode literate-haskell-mode)
  "Comment"
  (head)
  (body)
  (tail)
  (create))

(defstatement progn (haskell-mode literate-haskell-mode)
  "Sequential execution statement."
  (head)
  (body))

(defstatement if-then (haskell-mode literate-haskell-mode)
  "If statement without else clause."
  (head)
  (body)
  (add-head)
  (framework)
  (create)
  (begin-end)
  (begin-end-with-dummy))

(defstatement if-then-else (haskell-mode literate-haskell-mode)
  "If statement with else clause."
  (head)
  (body)
  (tail)
  (framework)
  (create)
  (begin-end)
  (begin-end-with-dummy))

(defstatement while-do (haskell-mode literate-haskell-mode)
  "While statement."
  (head)
  (body)
  (framework)
  (create)
  (begin-end)
  (begin-end-with-dummy))

(defstatement do-while (haskell-mode literate-haskell-mode)
  "Do-While statement."
  (head)
  (body)
  (create))

(defstatement for (haskell-mode literate-haskell-mode)
  "For statement."
  (head)
  (body)
  (framework)
  (create))

(defstatement switch (haskell-mode literate-haskell-mode)
  "Switch statement"
  (head)
  (body)
  (framework)
  (create))

(defstatement defun (haskell-mode literate-haskell-mode)
  "Function definition"
  (head)
  (body)
  (framework)
  (create)
  (begin-end))

(defstatement variable-declaration (haskell-mode literate-haskell-mode)
  "Local variable"
  (head)
  (body)
  (framework)
  (create))

(defstatement assignment (haskell-mode literate-haskell-mode)
  "Assignment"
  (head)
  (body)
  (framework)
  (create))

(defstatement function-call (haskell-mode literate-haskell-mode)
  "Function call"
  (head)
  (body)
  (framework)
  (create))

(defstatement return (haskell-mode literate-haskell-mode)
  "Return, with optional result"
  (head)
  (body)
  (framework)
  (create))

(defstatement and (haskell-mode literate-haskell-mode)
  "And expression."
  (begin-end)
  (begin-end-with-dummy))

(defstatement or (haskell-mode literate-haskell-mode)
  "Or expression."
  (begin-end)
  (begin-end-with-dummy))

(defstatement not (haskell-mode literate-haskell-mode)
  "Not expression."
  (begin-end))


;;; end of languide-functional.el
