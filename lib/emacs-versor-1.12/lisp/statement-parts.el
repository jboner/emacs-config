;;;; statement-parts.el -- navigate around parts of statements
;;; Time-stamp: <2007-12-11 18:00:54 jcgs>

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


(require 'statement-nav-directions)

(defun navigate-this-whole ()
  "Navigate to the whole of the current statement."
  (interactive)
  (navigate-to 'whole))

(defun navigate-this-head ()
  "Navigate to the head of the current statement."
  (interactive)
  (navigate-to 'head)) 

(defun navigate-this-body ()
  "Navigate to the body of the current statement."
  (interactive)
  (navigate-to 'body))

(defun navigate-this-tail ()
  "Navigate to the tail of the current statement."
  (interactive)
  (navigate-to 'tail))

(defun navigate-this-framework ()
  "Navigate to the framework of the current statement."
  (interactive)
  (navigate-to 'framework))

(defun navigate-this-container ()
  "Navigate to the container of the current statement."
  (interactive)
  (versor-as-motion-command current-item
    (navigate-this-whole) ; in case of assumptions made when moving outwards
    (let ((end (statement-container)))
      (when navigate-container-whole-statement
	(previous-statement 1)
	(setq navigated-latest-part 'container)
	(versor-set-current-item (point) end)
	(versor-display-highlighted-choice "container" (languide-parts))
	(cons (point) end)))))

(defun locate-this-container ()
  "Locate the container of the current statement.
Return a cons of the start and end of it.
Like navigate-this-container, but without the versor motion code."
  (interactive)
  (navigate-this-whole) ; in case of assumptions made when moving outwards
  (let ((end (statement-container)))
    (previous-statement 1)
    (cons (point) end)))

(defun get-statement-part (type part)
  "For the major mode, get statement description of TYPE, PART thereof."
  (let ((modal-statements (get major-mode 'statements)))
    (if modal-statements
	(let ((statement (cdr (assoc type modal-statements))))
	  (if statement
	      (let ((description (cdr (assoc part statement))))
		(if description
		    description
		  nil))
	    nil))
      nil)))

(defun statement-navigate-parts-next (&optional ignore)
  "Navigate to the next part of the statement."
  (interactive)
  (versor-as-motion-command current-item
   (case navigated-latest-part
     ('container (navigate-this-framework))
     ('framework (navigate-this-whole))
     ('whole (navigate-this-head))
     ('head (navigate-this-body))
     ('body (if (get-statement-part statement-navigation-type 'tail)
		(navigate-this-tail)
	      (navigate-this-head)))
     ('tail
      (cond
       (statement-navigate-parts-cyclic (navigate-this-head))
       (statement-navigate-parts-include-container (navigate-this-container))
       (t (navigate-this-whole)))))))

(defun statement-navigate-parts-previous (&optional ignore)
  "Navigate to the previous part of the statement."
  (interactive)
  (versor-as-motion-command current-item
   (case navigated-latest-part
     ('container (navigate-this-head))
     ('framework (cond
		  (statement-navigate-parts-include-container (navigate-this-container))
		  (statement-navigate-parts-cyclic
		   (if (get-statement-part statement-navigation-type 'tail)
		       (navigate-this-tail)
		     (navigate-this-body)))))
     ('whole (navigate-this-framework))
     ('head (navigate-this-whole))
     ('body (navigate-this-head))
     ('tail (navigate-this-body)))))

(defvar navigated-latest-part nil
  "The latest part of a statement that we navigated to.")

(defvar navigated-latest-place nil
  "The latest place we got to by statement navigation.")

(defvar statement-latest-start nil
  "The last statement start position we navigated to.")

(mapcar 'make-variable-buffer-local
	'(navigated-latest-part
	  navigated-latest-place
	  statement-latest-start))

(defun languide-parts ()
  "The parts we can navigate to."
  (if (get-statement-part statement-navigation-type 'tail)
      (if statement-navigate-parts-include-container
	  '("container" "framework" "whole" "head" "body" "tail")
	'("framework" "whole" "head" "body" "tail"))
    (if statement-navigate-parts-include-container
	'("container" "framework" "whole" "head" "body")
      '("framework" "whole" "head" "body"))))

(defvar languide-last-statement-selector-command nil
  "The last command to call establish-current-statement.")

(defvar languide-safe-commands
  '(statement-navigate-parts-next
    statement-navigate-parts-previous)
  "Commands that leave things as languide would like to find them.")

(defun latest-move-was-languide ()
  "Return whether the latest move was by languide."
  (or (eq last-command languide-last-statement-selector-command)
      (memq last-command languide-safe-commands)
      (and (memq last-command versor-commands)
	   (memq versor-last-vicarious-command languide-safe-commands))))

(provide 'statement-parts)

;;; end of statement-parts.el
