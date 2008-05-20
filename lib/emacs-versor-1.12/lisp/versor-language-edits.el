;;;; versor-language-edits.el -- versor commands to access commands in language-edits.el
;;; Time-stamp: <2006-08-03 21:33:09 john>

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


(require 'languide-edits)

(defun versor-languide-convert-selection-to-variable (name &optional nearest)
  "Make a variable declaration for NAME initialized to the selection, and replace the selection with it.
Useful when you realize you want to re-use a value you had calculated
in-line.
If languide-make-variables-interactively is non-nil, let the user choose the scope;
otherwise, use the following rules:
A likely scope is found automatically, but is often
optimistically wide. With prefix arg, take the narrowest scope. With a
positive number as prefix, take that numbered scope outward from the
selection. With a negative number as prefix, let the user choose the
scope interactively. The variable name is left at the top of the kill
ring."
  (interactive "sVariable name: ")
  (versor-as-motion-command item
    (languide-convert-region-to-variable (versor-overlay-start item)
					 (versor-overlay-end item)
					 name
					 (if current-prefix-arg
					     (if (numberp current-prefix-arg)
						 (if (< current-prefix-arg 0)
						     'interactive
						   current-prefix-arg)
					       (if (eq current-prefix-arg '-)
						   'interactive
						 nil))
					   t) )))

(defun versor-languide-convert-selection-to-global-variable (name)
  "Make a variable declaration holding the current selection, and substitute it.
Useful when you realize you want to re-use a value you had calculated in-line.
The variable name is left at the top of the kill ring."
  (interactive "sVariable name: ")
  (versor-as-motion-command item
      (languide-convert-region-to-global (versor-overlay-start item)
					 (versor-overlay-end item)
					 name)))

(defun versor-languide-convert-selection-to-function (name &optional docstring)
  "Take the selected code, and make it into a function, substituting a call to it.
The function name is left at the top of the kill ring."
  (interactive
   (let* ((name (read-from-minibuffer "Function name: "))
	  (documentation (read-from-minibuffer
			  "Documentation: "
			  (format "Helper function for %s."
				  (ambient-defun-name
				   (versor-overlay-start
				    (versor-get-current-item)))))))
     (list name documentation))) 
  (versor-as-motion-command item
     (message "item is %S" item)
     (languide-convert-region-to-function (versor-overlay-start item)
					  (versor-overlay-end item)
					  name
					  docstring)))

(defun versor-languide-surround-selection-with-call (name)
  "Surround the selection with a function call."
  (interactive "sFunction name: ")
  (versor-as-motion-command item
     (languide-surround-region-with-call (versor-overlay-start item)
					 (versor-overlay-end item)
					 name)))

(defun versor-languide-remove-function-call ()
  "Remove the selected function call."
  (interactive)
  (versor-as-motion-command item
    (languide-remove-surrounding-call (versor-overlay-start item))))

(defun versor-languide-create-function-for-call ()
  "Create a function definition suitable for the function call around point."
  (interactive)
  (versor-as-motion-command item
      (let ((defn-place (languide-create-function-for-call)))
	(goto-char (1+ defn-place))
	(navigate-this-body))))

(defun versor-languide-unify-statements ()
  "Unify statements"
  (interactive)
  (versor-as-motion-command item
    (languide-unify-statements-region (versor-overlay-start item)
				      (versor-overlay-end item))))

(defun versor-languide-comment-selection ()
  "Turn the selection into a comment."
  (interactive)
  (versor-as-motion-command current-item
   (let* ((items (versor-last-item-first)))
     (while items
       (comment-region (versor-overlay-start (car items))
		       (versor-overlay-end (car items)))
       (setq items (cdr items))))))

(defun versor-languide-enclosing-scoping-point ()
  "Move to enclosing scoping point"
  (interactive)
  (versor-as-motion-command current-item
   (languide-enclosing-scoping-point 1)))

(defun versor-languide-enclosing-decision-point ()
  "Move to enclosing decision point"
  (interactive)
  (versor-as-motion-command current-item
   (languide-enclosing-decision-point 1)))

(defun versor-languide-employ-variable ()
  "Employ variable"
  (interactive)
  (versor-as-motion-command current-item
   (languide-employ-variable (point))))

(defun versor-languide-make-conditional (condition)
  "Make the current selection conditional."
  (interactive "sCondition: ")
  (versor-as-motion-command item
     (languide-make-conditional (versor-overlay-start item)
				(versor-overlay-end item)
				condition)))

(defun versor-languide-make-iterative (continue-condition)
  "Make the current selection iterative."
  (interactive "sContinue condition: ")
  (versor-as-motion-command item
     (languide-make-iterative (versor-overlay-start item)
			      (versor-overlay-end item)
			      continue-condition)))

(defun versor-languide-remove-control ()
  "Remove the control around the current selection."
  (interactive)
  (languide-remove-control))

(provide 'versor-language-edits)

;;; end of versor-language-edits.el
