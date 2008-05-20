;;;; statement-navigation.el -- Statement-based navigation for languide and versor
;;; Time-stamp: <2007-03-19 20:35:45 jcgs>

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
(require 'versor-commands) ; for versor-as-motion-command
(require 'statement-cache)
(require 'statement-parts)

;;;; models for modals

(defmodel beginning-of-statement-internal ()
  "Move to the beginning of the statement.
Do not do auxiliary stuff that might be associated with this.
Return the type of statement if obvious, in a form that can
be given to end-of-statement-internal as a hint."
  (interactive))

(defmodel end-of-statement-internal (hint)
  "Move to the end of the current statement, with HINT as to the statement type.
Do not do auxiliary stuff that might be associated with this."
  (interactive))

(defmodel move-into-previous-statement ()
  "Move into the previous statement.
This need be valid only after a beginning-of-statement-internal.
It should move point back such that another beginning-of-statement-internal
will go back another statement."
  (interactive))

(defmodel move-into-next-statement ()
  "Move into the next statement.
This need be valid only after an end-of-statement-internal.
It should move point forward such that another end-of-statement-internal
will go forward another statement."
  (interactive))

(defmodel identify-statement (default)
  "Note what kind of statement we are at the start of.
Need not work if not at the start of a statement.
If the statement cannot be identified, return DEFAULT.")

(defun establish-current-statement (command &optional as-far-as)
  "Record some things about the statement at point."
  ;; After each movement that may take us into a different statement,
  ;; we update some state variables that tell us about the current
  ;; statement
  (setq languide-last-statement-selector-command command)
  (let ((type (identify-statement nil)))
    (statement-set-type type)
    (statement-remember (point) type as-far-as)
    (setq statement-latest-start (point)
	  navigated-latest-part 'whole)
    type))

(defun beginning-of-statement ()
  "Move to the beginning of the statement.
Argument 1 means the current statement, 2 the next, etc."
  (interactive)
  (let ((hint (beginning-of-statement-internal)))
    (establish-current-statement 'beginning-of-statement hint)))

;; this is for testing
;; (global-set-key [ kp-subtract ] 'beginning-of-statement-internal)

(defun end-of-statement ()
  "Move to the end of the current statement.
Argument 1 means the current statement, 2 the next, etc."
  (interactive)
  (let ((hint (beginning-of-statement-internal)))
    (setq hint (establish-current-statement 'end-of-statement hint))
    (end-of-statement-internal hint)))

(defmodel previous-statement-internal (n)
  "Move to the NTH previous statement.
If calling this from a program, other than inside statement navigation,
you should possibly use previous-statement instead.
There an element of DWIM to this:
  If not at the beginning of the statement, move to the beginning of it.
  If already at the beginning, move to the beginning of the previous one.
Then, if N is greater than 1, move back N-1 more statements.
Return the buffer offset of the end of the statement.")

(defmodal previous-statement-internal (fundamental-mode) (n)
  "Move to the NTH previous statement.
If calling this from a program, other than inside statement navigation,
you should possibly use previous-statement instead.
There an element of DWIM to this:
  If not at the beginning of the statement, move to the beginning of it.
  If already at the beginning, move to the beginning of the previous one.
Then, if N is greater than 1, move back N-1 more statements.
Return the buffer offset of the end of the statement."
  (let* ((starting-point (point))
	 (previous-end nil)
	 ;; first, try going back to the start of a statement, to see
	 ;; whether we end up where we started:
	 (hint (beginning-of-statement-internal)))
    (when (= (point) starting-point) ; this means we were already at the start, so go back another
      ;; (languide-debug-message 'previous-statement "Was already at start, going back an extra one")
      (incf n))
    (while (> n 1)
      (setq previous-end (point))
      (move-into-previous-statement)
      (setq hint (beginning-of-statement-internal))
      (decf n))
    ;; now adjust our idea of the "end of the statement"; either we
    ;; can go all the way up to the next statement (set
    ;; versor-statement-up-to-next to get this behaviour, but I think
    ;; it's ugly), or find the real end of the statement of which we
    ;; have just found the beginning
    (let ((end (if (and versor-statement-up-to-next previous-end)
		   previous-end
		 (save-excursion
		   (let ((start (point)))
		     (condition-case nil
			 (progn
			   (end-of-statement-internal hint))
		       (error (message "Could not find end of statement")))
		     (point))))))
      end)))

(defun previous-statement (n)
  "Move to the NTH previous statement.
There an element of DWIM to this:
  If not at the beginning of the statement, move to the beginning of it.
  If already at the beginning, move to the beginning of the previous one.
Then, if N is greater than 1, move back N-1 more statements."
  (interactive "p")
  (versor-as-motion-command current-item
   (let ((end (previous-statement-internal n)))
     (versor-set-current-item (point) end)
     (establish-current-statement 'previous-statement end))))

(defun next-statement-internal (n)
  "Move to the NTH next statement.
If calling this from a program, other than inside statement navigation,
you should possibly use next-statement instead."
  (let ((first t)
	(starting (point)))
    (while (> n 0)
      ;; todo: this does the seemingly wrong thing if called before the start of the first statement -- it ends up taking us to the second statement
      (end-of-statement-internal nil)
      ;; not sure what the point of this next bit was... seems to do better without it!
      (when nil first ; go back to see if we were before the statement
	    (let ((first-end (point)))
	      (beginning-of-statement-internal)
	      (if (> (point) starting)
		  ;; if the beginning of the first statement is past
		  ;; where we started, i.e. we started before the
		  ;; statement, then no more iterations
		  (progn
		    (setq n 0))
		(goto-char first-end))))
      (skip-to-actual-code)
      (when (> n 1)
	(move-into-next-statement))
      (decf n)
      (setq first nil)))
  ;; move over any comment
  (skip-to-actual-code))

(defun next-statement (n)
  "Move to the NTH next statement, and set up the statement variables."
  (interactive "p")
  (versor-as-motion-command current-item
    (message "next-statement starting from %d, navigated-latest-part=%S" (point) navigated-latest-part)
    (unless (and (eq navigated-latest-part 'body)
		 (eq (point) navigated-latest-place))
      ;; move on to the next statement, unless we have just selected a
      ;; grouping body construct, in which case we want to select the
      ;; first statement within the group
      (next-statement-internal n)
      (setq navigated-latest-part 'whole))
    (skip-to-actual-code)
    (let* ((start (point))
	   (hint nil)			; todo: get statement type here if possible
	   (end (save-excursion (end-of-statement-internal hint) (point))))
      (message "next-statement start=%d end=%d" start end)
      (versor-set-current-item start end)
      (establish-current-statement 'next-statement end))))

(defvar statement-navigation-type 'comment
  "The current type of statement to navigate to parts of.
Initialized to 'comment as pretty well every language should have a definition of this.")

(defvar statement-navigation-type-string "comment"
  "The name of the current type of statement to navigate to parts of.
Initialized to \"comment\" as pretty well every language should have a definition of this.")

(defun statement-set-type (type)
  "Set the statement type to TYPE.
If TYPE is nil, do nothing (might change this to say \"unknown\").
This also updates the mode line display of it."
  (interactive
   (list
    (choose-in-steps "Statement type: "
		     (mapcar
		      (function
		       (lambda (statement)
			 (symbol-name (car statement))))
		      (get major-mode 'statements)))))
  (message "%S statement" type)
  ;; (message "%S statement at %d" type (point))
;;   (backtrace)
  (if type
      (progn
	(setq statement-navigation-type (if (symbolp type) type (intern type))
	      statement-navigation-type-string (if (symbolp type) (symbol-name statement-navigation-type) type))
	(force-mode-line-update t)
	t)
    (progn
      (message "No type given")
      (setq statement-navigation-type 'unknown
	    statement-navigation-type-string "unknown")
      (force-mode-line-update t)
      nil)))

(defun statement-type-at-point ()
  "Return the type of statement at point."
  (save-excursion
    (beginning-of-statement 1)
    (identify-statement nil)))

(provide 'statement-navigation)

;;; end of statement-navigation.el
