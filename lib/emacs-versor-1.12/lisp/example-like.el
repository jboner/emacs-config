;;;; example-like.el -- example
;;; Time-stamp: <2007-08-11 14:43:48 jcgs>

(require 'modal-functions)

(defmodal move-into-previous-statement example-mode ()
  "Move into the previous statement.
This need be valid only after a beginning-of-statement-internal.
It should move point back such that another beginning-of-statement-internal
will go back another statement."
)

(defmodal move-into-next-statement example-mode ()
  "Move into the next statement.
This need be valid only after an end-of-statement-internal.
It should move point forward such that another end-of-statement-internal
will go forward another statement."
)

(defmodal beginning-of-statement-internal example-mode ()
  "Move to the beginning of the statement.
Do not do auxiliary stuff that might be associated with this."
)

(defmodal end-of-statement-internal example-mode ()
  "Move to the end of the current statement.
Do not do auxiliary stuff that might be associated with this."
)

(defmodal identify-statement example-mode (default)
  "Note what kind of statement we are at the start of.
Need not work if not at the start of a statement.
If the statement cannot be identified, return DEFAULT."
)

(defmodal compound-statement-open example-mode ()
  "Return a block start."
)

(defmodal compound-statement-close example-mode ()
  "Return a block end."
)

(defmodal insert-compound-statement-open example-mode ()
  "Insert the start of a compound statement"
)

(defmodal insert-compound-statement-close example-mode ()
  "Insert the end of a compound statement"
)

(defmodal binding-around example-mode (whereat)
  "Return the variable definition around WHEREAT."
)

(defmodal scope-around example-mode (whereat)
  "Return the scope (start and end) around WHEREAT."
)

(defmodal variable-reference example-mode (varname)
  "Make a variable reference from VARNAME"
)

(defstatement comment (c-mode)
  "Comment"
  (head )
  (body )
  (create ))

(defstatement if-then example-mode
  "If statement without else clause."
  (head )
  (body )
  (framework )
  (create )
  (begin-end )
  (begin-end-with-dummy ))

(defstatement if-then-else example-mode
  "If statement with else clause."
  (head )
  (body )
  (tail )
  (framework )
  (statements)
  (create )
  (begin-end )
  (begin-end-with-dummy ))

(defstatement while-do example-mode
  "While statement."
  (head )
  (body )
  (create )
  (begin-end )
  (begin-end-with-dummy ))

(defstatement for example-mode
  "For statement."
  (head )
  (body )
  (framework )
  (create ))

(defstatement switch example-mode
  "Switch statement"
  (head )
  (body )
  (framework )
  (create ))

(defstatement variable-declaration example-mode
  "Local variable"
  (head )
  (body )
  (framework )
  (create ))

(defstatement assignment example-mode
  "Assignment"
  (head )
  (body )
  (framework )
  (create ))

;;; end of example-like.el
