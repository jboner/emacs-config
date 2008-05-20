;;;; statement-definition.el -- description mechanism for statement syntax in various languages
;;; Time-stamp: <2007-07-17 12:15:11 john>

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

(defmacro defstatement (statement modes &rest parts)
  "Define, STATEMENT, in MODES, to consist of PARTS.
Each part is a list naming and describing that part.
The part names are:
  head
  body
  tail
and the descriptions are a mixture of strings and function calls.
Strings are searched for, and functions move over, or return,
parts of the statement. The last of the functions is used to
return that part of the statement.

Functions written for this purpose are listed in
statement-navigate-list-selector-functions, which see (and if you
write your own, you must add their names to that variable).

The commonest ones are as follows:
  expression
  expression-contents
  expressions
  preceding-expression
  start-of-match
  upto pattern

For example, to name and describe how to get the \"else\" part of
an if-then-else statement in C, Java, or Perl, we write
  (tail \"if\" (expression) (expression) \"else\" (expression-contents))
which will look for \"if\", skip two expressions, move over an \"else\",
and return the contents of the next expression.

You can also give 'create as a part name, and follow it with a tempo template
or a skeleton; and likewise 'add-head and 'add-trailer, which want a template/skeleton for
adding just a statement head.

You can supply 'adjust-body as a part name, giveing a function to call to adjust the body
of that kind of statement when wrapping the statement around a block of code."
  (defstatement0 statement modes parts))

(defun defstatement0 (statement modes parts)
  "See defstatement."
  (let ((documentation nil))
    (when (stringp (car parts))
      (setq documentation (car parts)
	    parts (cdr parts)))
    (dolist (mode modes)
      (let ((modal-statement-list (get mode 'statements)))
	(setq modal-statement-list
	      (cons (cons statement (make-statement parts))
		    modal-statement-list))
	(put mode 'statements modal-statement-list)))))

(defun make-statement (parts)
 "Make up a statement description from PARTS."
  (mapcar 'make-statement-part parts))

(defun make-statement-part (part)
  "Turn PART into a statement descriptor."
  (let ((type (car part)))
    (cond
     ((memq type '(head body tail framework))
      (cons type `(statement-navigate ,@(cdr part))))
     ((memq type '(create add-head add-trailer))
      ;; (message "make-statement-part %S %S --> %S" type (cadr part) (make-statement-create-part (cadr part)))
      (cons type
	    (make-statement-create-part
	     (cadr part))))
     (t
      part))))

(defun make-statement-create-part (part)
  "Process PART as a statement creation part."
  (cond
   ((consp part)
    (let ((part-op (car part)))
      (cond
       ((eq part-op 'template)
	;; (message "defining template using %S" (cdr part))
	(let* ((name (gensym))
	       (template (tempo-define-template (symbol-name name) (cdr part))))
	  (list 'template template)))
       ((eq part-op 'skeleton)
	;; todo: implement skeletons
	;; (list 'skeleton ....)
	)
       (t part))))
   (t part)))

(defun statement-types (&optional full)
  "Return (and display, if interactive) the list of statement types available in this mode."
  (interactive "P")
  (let ((statement-types (get major-mode 'statements)))
    (when (interactive-p)
      (with-output-to-temp-buffer "*Statement types*"
	(dolist (statement-description statement-types)
	  (let ((statement (car statement-description)))
	    (princ (format "%c %s\n" (if (eq statement statement-navigation-type)
					 ?* ? )
			   (symbol-name statement)))
	    (when full
	      (let ((blueprint (cdr (assoc 'create  (cdr statement-description)))))
		(when blueprint
		  (let ((template (cadr (assoc 'template blueprint))))
		    (princ (format "    %S\n\n" (symbol-value template)))))))))))
    statement-types))

(defun create-statement ()
  "Insert a statement of the current type."
  (interactive)
  (create-or-surround nil))

(defun surround-statement ()
  "Surround the region with a statement of the current type."
  (interactive)
  (create-or-surround t))

;;;; pieces for the commands

(defun precondition-require (requirement)
  "Ensure that REQUIREMENT is met."
  ;; this is lisp-specific!
  (let ((require-string (format "(require '%s)" requirement)))
    (save-excursion
      (goto-char (point-min))
      (unless (search-forward require-string (point-max) t)
	(goto-char (point-max))
	(re-search-backward "^(\\(require\\)\\|\\(provide\\)" (point-min) 'to-limit)
	(beginning-of-line 2)
	(insert require-string "\n")))))

(defun precondition-not-within (from to)
  "Make sure we are not between matches for FROM and TO."
  (let* ((from-place (save-excursion (re-search-backward from (point-min) t)))
	 (to-place (save-excursion (re-search-backward to (point-min) t))))
    (if (and from-place
	     (or
	      (and to-place (> from-place to-place))
	      (not to-place)))
	(re-search-forward to (point-max) t))))

(defvar preconditions
  '((require . precondition-require)
    (not-within . precondition-not-within)
    )
  "The possible preconditions.")

(defun languide-precondition (precondition)
  "Implement PRECONDITION."
  (message "want precondition %S" precondition)
  (let ((handler (cdr (assoc (car precondition) preconditions))))
    (if handler
	(apply handler (cdr precondition))
      (error "Unknown precondition: %S" precondition))))

(defun languide-postcondition (postcondition)
  "Implement POSTCONDITION."
  (message "want postcondition %S" postcondition)
  )

(defun create-or-surround (surrounding &optional statement)
  "Insert a statement of the current type, optionally SURROUNDING the current region.
Interactively, uses the current surrounding / following status.
Optionally, the statement type can be passed in as second argument."
  (interactive (list (not (eq statement-navigation-forwards 'forwards))))
  (let* ((tempo-interactive t)
	 (statement-type (or statement statement-navigation-type))
	 (description (get-statement-part statement-navigation-type 'create)))
    (if description
	(progn
	  (message "Handling description %s" description)
	  (when (eq (car (car description)) 'precondition)
	    (message "precondition: %S" (cdr (car description)))
	    (mapcar 'languide-precondition (cdr (car description)))
	    (setq description (cdr description)))
	  (when (eq (car (car description)) 'template)
	    (message "Inserting template %S %S" (second (car description)) surrounding)
	    ;; todo: allow these to be skeletons, as an alternative
	    (tempo-insert-template (second (car description)) surrounding)
	    (setq description (cdr description)))
	  (when (eq (car (car description)) 'postcondition)
	    (message "postcondition %S" (cdr (car description)))
	    (mapcar 'languide-postcondition (cdr (car description)))
	    (setq description (cdr description))))
      (error "No %S defined for %S for %S" part statement-type major-mode))))

(provide 'statement-definition)

;;; end of statement-definition.el
