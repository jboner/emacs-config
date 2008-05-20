;;;; languide-bindings.el -- handle variable bindings in a language-parameterized way
;;; Time-stamp: <2007-08-22 19:18:38 jcgs>

;; Copyright (C) 2007, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2004?
;; Keywords: editing

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


(require 'cl)
(require 'modal-functions)

(defmodel scope-around (whereat)
  "Return (as a list / multiple value) the start and end of the scope around WHEREAT.")

(defmodel binding-around (whereat)
  "Return (as a list / multiple value) a description of the binding around WHEREAT.
The results are:
  name                The name bound, as a string
  value               The expression giving the initial value for the variable,
                      as a string
  namestart nameend   Character positions of the start and end of the name
  valuestart valueend Character positions of the start and end of the initial
                      value expression
")

(defmodel variable-bindings-in-region (from to)
  "Return a list of the bindings between FROM and TO.
Each element is a list of:
  name
  type
  scope-begins scope-ends
  initial-value-as-string")

(defmodel variable-references-in-region (from to)
  "Return a list of the variable references between FROM and TO.
Each element is a list of:
  name
  location")

(defmodel static-variable-p (name where)
  "Return whether a static variable called NAME is visible at WHERE.")

(defun free-variables-in-region (from to)
  "Return a list of free variables between FROM and TO.
These are the variables used in that region, but not defined it it."
  (let ((references (variable-references-in-region from to))
	(bindings (variable-bindings-in-region from to))
	(result nil))
    ;; (message "In %d..%d, references are %S; bindings are %S" from to references bindings)
    (dolist (reference references)
      (let* ((name (car reference))
	     (binding (assoc name bindings)))
	(if binding
	    (let ((where-used (cadr reference)))
	      (unless (or (and (< (third binding) where-used)
			       (< where-used (fourth binding)))
			  (static-variable-p name from))
		(pushnew name result :test 'string=)))
	  (unless (static-variable-p name from)
	    (pushnew name result :test 'string=)))))
    (nreverse result)))

(defmodel variables-in-scope (whereat)
  "Return, as an alist, the names and types of variables in scope at WHEREAT.
Where types are not declared, as in Lisp, nil can be given as the type.")

(defmodel variable-reference (varname) "")

(defmodel move-to-enclosing-scope-last-variable-definition
  (&optional allow-conversions)
  "Move to the end of the nearest set of variable bindings.
This is the place at which you would naturally insert a new
variable, allowing for its initial value referring to any
variable already declared.

Optional first argument WIDEST lets you indicate that you want the
widest scope possible, that is, the widest at which all the variables
needed in the definition of the new variable are already defined. If
WIDEST is a number, it is used to count how many scoping points to
move outwards by.

Optional second argument VARIABLES-NEEDED lists names of variables
needed in the definition of the new one. This is used if you specify
you want the widest scope.

It may create a suitable place if there is none; for example, in Lisp
it could wrap the outermost forms of a \"defun\" with a \"let\".

Optional third argument ALLOW-CONVERSIONS allows conversion of
possible scoping points into actual ones. For example, in lisp, this
means a \"progn\" can be changed to a \"let\".

Returns t if it found a binding, nil if it went to the top level
of the defun.")

(defmodel variable-last-possibly-assigned-before (variable wherebefore)
  "Return the position of the most recent assignment to VARIABLE before WHEREBEFORE.")

(defmodel variable-declaration-texts (name type initial-value)
  "Return the texts for a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
TYPE and INITIAL-VALUE may be null, but the NAME is required.
The result is a list of three strings: any preceding whitespace,
the actual declaration, and any following whitespace.
This may not be quite as refined as insert-variable-declaration, which has
the chance to play around with the buffer text. variable-declaration-text
is used to generate after-strings for overlays, to suggest possible
bindings to the user.")

(defmodel insert-variable-declaration (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
TYPE and INITIAL-VALUE may be null, but the NAME is required.")

(defmodel insert-global-variable-declaration (name type initial-value)
  "Insert a definition for a global variable called NAME, of TYPE, with INITIAL-VALUE.

TYPE and INITIAL-VALUE may be null, but the NAME is required.")

(provide 'languide-bindings)

;;; end of languide-bindings.el
