;;;; versor-alter-item.el -- choose possible value for the current item
;;; Time-stamp: <2007-10-07 20:36:37 jcgsversatile cursors for GNUemacs
;;
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
;; 

(require 'modal-functions)
(require 'languide)			; for variables-in-scope

;;; Code:
(defun versor-alterations:set-current ()
  "Set the current alterations posssibility.
This writes it into the buffer and points versor's item at it, and updates the display."
  (versor-alterations-trim-index)
  (let* ((old-item (versor-get-current-item))
	 (slot (if (and (arrayp versor-alterations-values)
			(< versor-alterations-index (length versor-alterations-values))
			(>= versor-alterations-index 0))
		   (aref versor-alterations-values versor-alterations-index)
		 nil))
	 (new-text (if slot (car slot) "")))
    ;; todo: make this use versor-get-current-items, and change all of them
    (delete-region (car old-item) (cdr old-item))
    (when (eq versor-alterations-old-undo t)
      ;; On the first alteration, we record the undo list as far as
      ;; the deletion of the original selection, so we can fiddle with
      ;; the undo list when we finish altering, such that undoing will
      ;; treat an alteration atomically.
      (setq versor-alterations-old-undo buffer-undo-list))
    (goto-char (car old-item))
    (let ((new-start (point)))
      (insert (versor-insertable-form new-text))
      (versor-set-current-item new-start (point))
      (versor-alterations:show-current))))

(defun versor-alterations:get-type-values (&optional force)
  "Get the vector of possible values for the current source of values.
Calculates them if necessary, but the optional last parts of the
elements of `versor-alterations-types' are used as a cache.
Optional argument FORCE means recalculate them anyway."
  (if (and (arrayp versor-alterations-types)
	   (< versor-alterations-type-index (length versor-alterations-types))
	   (>= versor-alterations-type-index 0))
      (let* ((current-type (aref versor-alterations-types versor-alterations-type-index))
	     (current-type-name (car current-type))
	     (current-type-function (cadr current-type))
	     (current-type-values (cddr current-type)))
	(when (or force
		  (null current-type-values))
	  (message "Getting values of type \"%s\", using %S"
		   current-type-name current-type-function)
	  ;; the function might go wrong, e.g. no tag table, so do it
	  ;; in a condition-case
	  (condition-case evar
	      (setq current-type-values (apply 'vector
					       (funcall current-type-function
							(point))))
	    (error
	     (setq current-type-values ["**_none_defined**"])
	     nil))
	  (rplacd (cdr current-type) current-type-values))
	current-type-values)
    nil))

(defun versor-insertable-form (var)
  "Return VAR in a form suitable for inserting."
  (when (consp var) (setq var (first var)))
  (cond
   ((stringp var) var)
   ((symbolp var) (symbol-name var))
   (t (error "Cannot get string for %S" var))))

(defun versor-alterations:show-current ()
  "Show the current alterations posssibilities."
  ;; todo: rather than a fixed "spread", perhaps use mapconcat, and
  ;; then substring it down? the problem with that would be that some
  ;; of the lists will be very large (e.g. a well-populated tags
  ;; table)
  (let* ((spread 3) ; how many to show each side of the current choice
	 ;; first to show
	 (m (max 0
		 (- versor-alterations-index spread)))
	 ;; last to show
	 (n (min (1- (length versor-alterations-values))
		 (+ versor-alterations-index spread)))
	 ;; how much room before current choice
	 (before-display-length (- (/ (frame-width) 2)
				   12))
	 ;; first a list, then a string, of choices to left of current
	 (before nil)
	 (current (versor-highlighted-string
		   (versor-insertable-form
		    (aref versor-alterations-values versor-alterations-index))))
	 ;; first a list, then a string, of choices to right of current
	 (after nil))
    ;; collect up the possibilities before the current choice
    (let ((i (1- versor-alterations-index)))
      (while (>= i m)
	(push (aref versor-alterations-values i) before)
	(decf i)))
    ;; collect up the possibilities after the current choice
    (let ((i n))
      (while (> i versor-alterations-index)
	(push (aref versor-alterations-values i) after)
	(decf i)))
    ;; convert the "before" and "after" possibilities to strings
    (setq before (mapconcat 'versor-insertable-form before ", ")
	  after (mapconcat 'versor-insertable-form after ", "))
    ;; if the before-string is too long, trim it
    (when (> (length before) before-display-length)
      (setq before (concat "... "
			   (substring before (- before-display-length 4)))))
    ;; if the whole lot is too long, trim the after-string
    (when (> (+ (length before) (length current) (length after) 4)
	     (frame-width))
      (setq after (concat (substring after 0 (- (frame-width)
						(+ (length before)
						   (length current)
						   8)))
			  " ...")))
    ;; prepare to suppress stray commas if any of the lists are empty
    (when (string= before "") (setq before nil))
    (when (string= after "") (setq after nil))
    (cond
     ((and before after)
      (message "%s, %s, %s" before current after))
     (before
      (message "%s, %s" before current))
     (after
      (message "%s, %s" current after))
     (t
      (message "%s" current))))
  ;; update the display
  (unless (memq 'versor-alterations-type-name header-line-format)
    (setq header-line-format (append header-line-format
				     (list " " 'versor-alterations-type-name))))
  (setq versor-alterations-type-name
	(car (aref versor-alterations-types versor-alterations-type-index)))
  (force-mode-line-update))

(defun versor-alterations-trim-index ()
  "Ensure that `versor-alterations-type-index' and `versor-alterations-index' are valid."
  (cond
   ((< versor-alterations-type-index 0)
    (setq versor-alterations-type-index 0))
   ((>= versor-alterations-type-index (length versor-alterations-types))
    (setq versor-alterations-type-index (1- (length versor-alterations-types)))))
  (setq versor-alterations-values (versor-alterations:get-type-values))
  (cond
   ((< versor-alterations-index 0)
    (setq versor-alterations-index 0))
   ((>= versor-alterations-index (1- (length versor-alterations-values)))
    (setq versor-alterations-index (1- (length versor-alterations-values))))))

(defun versor-alter-item-next-value ()
  "Replace the current item with the next possible value."
  (interactive)
  (barf-if-buffer-read-only)
  (versor-as-motion-command current-item
   (incf versor-alterations-index)
   (versor-alterations:set-current)))

(defun versor-alter-item-previous-value ()
  "Replace the current item with the previous possible value."
  (interactive)
  (barf-if-buffer-read-only)
  (versor-as-motion-command current-item
    (decf versor-alterations-index)
    (versor-alterations:set-current)))

(defun versor-alter-item-next-type ()
  "Replace the current item with the corresponding value from the next range."
  (interactive)
  (barf-if-buffer-read-only)
  (versor-as-motion-command current-item
    (incf versor-alterations-type-index)
    (versor-alterations:set-current)
    (versor-display-highlighted-choice
     (car (aref versor-alterations-types versor-alterations-type-index))
     (map 'list 'car versor-alterations-types))))

(defun versor-alter-item-previous-type ()
  "Replace the current item with the corresponding value from the previous range."
  (interactive)
  (barf-if-buffer-read-only)
  (versor-as-motion-command current-item
    (decf versor-alterations-type-index)
    (versor-alterations:set-current)
    (versor-display-highlighted-choice
     (car (aref versor-alterations-types versor-alterations-type-index))
     (map 'list 'car versor-alterations-types))))

(defmodel versor-get-alterations-possibility-types ()
  "Return an list of relevant alteration possibility types, relevant around point.
Each element is a list of a name and a function to call.
When called, that function should return a list of the possibilities of that type.
This gets cached in the cddr of the element.
It is called at the start of each alteration.
A typical entry might be (\"Local variables\" variables-in-scope) where variables-in-scope
will be a function to list all the variables in scope at point.")

(defun versor-tags-as-alist (&optional ignore sorted)
  "Return the visible tags, as an alist.
Ignores first optional argument IGNORE, which is needed as it is
sometimes called from a list of functions, some of which expect
an argument.
SORTED, if non-nil, says to sort the result textually."
  (save-excursion
    (let ((result nil))
      (visit-tags-table-buffer)
      (mapatoms (lambda (atom) (setq result (cons (list atom) result)))
		(tags-completion-table))
      (if sorted
	  (sort result
		(function (lambda (a b) (string< (car a) (car b)))))
	result))))

(defun versor-tags-as-sorted-alist (&optional ignore)
  "Like `versor-tags-as-alist', but the tags are sorted lexicographically.
Ignores first optional argument IGNORE, which is needed as it is
sometimes called from a list of functions, some of which expect
an argument."
  (versor-tags-as-alist nil t))

(defun versor-alterations-get-current (&optional ignore)
  "Return, as a completion list, the current item.
Putting this into the possible types of alteration value is a way
of making sure that the current value is amongst the possible
values.
Ignores optional argument IGNORE, needed as it is sometimes
called from a list of functions, some of which expect an
argument."
  (let ((item (versor-get-current-item)))
    (list (list (buffer-substring-no-properties (car item) (cdr item))))))

(defun get-statement-types (&optional mode)
  "Get the statement types, as an alist, for the current mode, or for MODE if given."
  (let* ((raw-statements (get (or mode major-mode) 'statements))
	 (statement-names nil))
    (while raw-statements
      (let ((name (second (assoc 'keyword (car raw-statements)))))
	(when name
	  (setq statement-names (cons (cons name
					    (car raw-statements))
				      statement-names))))
      (setq raw-statements (cdr raw-statements)))
    statement-names))

(defun versor-get-statement-types (&rest junk)
  "Versor packaging for get-statement-types, which it calls with nil.
Optional argument JUNK is for compatibility with other functions, and is ignored."
  (get-statement-types))

(defmodel versor-get-language-constants (where)
  "Return the common constants for a language.")

(defmodal versor-get-language-constants (emacs-lisp-mode
					 lisp-interaction-mode
					 lisp-mode)
  (where)
  "Return the common constants for Lisp."
  '(("t" . "t") ("nil" . "nil")))

(defmodal versor-get-language-constants (c-mode)
  (where)
  "Return the common constants for C."
  '(("NULL" . "NULL") 
    ("stdin" . "stdin")
    ("stdout" . "stdout")
    ("stderr" . "stderr")))

(defmodal versor-get-alterations-possibility-types
  (
   ;; probably most programming language modes will use this definition
   emacs-lisp-mode lisp-interaction-mode
   c-mode)
  ()
  "Return the possible types of values for emacs-lisp-mode."
  ;; todo: make this more selective, in particular, don't include tags
  ;; if there is no applicable tags table
  '(("local variables" variables-in-scope)
    ("tags" versor-tags-as-alist)
    ("statements" versor-get-statement-types)
    ("constants" versor-get-language-constants)
    ("sorted tags" versor-tags-as-sorted-alist)
    ("current" versor-alterations-get-current)))

(defvar versor-alterations-types nil
  "The possible types of alterations for the current alteration, as a vector.
The elements of the vector are the results of
`versor-get-alterations-possibility-types'.
Thus, each element is a list of:
  a string naming that type of possibility
  a function to call to get an alist of the possibilities
  a list, possibly nil, of those possibilities, caching the result of the above function.")

(defvar versor-alterations-type-index 0
  "The current index into `versor-alterations-types'.")

(defvar versor-alterations-type-name ""
  "The name of the current type of alteration possibility.")

(defvar versor-alterations-values nil
  "The possible values of alterations for the current type of alteration.
This is a vector; its elements are the results of an element of
`versor-get-alterations-possibility-types'.")

(defvar versor-alterations-index 0
  "The current index into the current entry in `versor-alterations-types'.")

(defvar versor-alterations-old-keymap nil
  "The local map that was in use before we started alterations.")

(defvar versor-alterations-old-item nil
  "The old selection, just before we started altering it.")

(defvar versor-alterations-old-value nil
  "The value of the selected item just before we started altering it.")

(defvar versor-alterations-old-undo nil
  "The value of the undo list just as we start alterations.
We set it to t at the start of an alteration, which tells the first
change to record the undo list up to the deletion of the original
selection.")

(defun versor-begin-altering-item ()
  "Start selecting amongst possible values for the selected item.
Sets up mapping for the arrow keys, such that they now change the
value of the item, and the menu/select key to keep the value you
have at the time."
  (interactive)
  (barf-if-buffer-read-only)
  (versor-as-motion-command versor-alterations-old-item
    (setq versor-alterations-old-value (buffer-substring-no-properties
					(car versor-alterations-old-item)
					(cdr versor-alterations-old-item))
	  versor-alterations-old-undo t
	  versor-alterations-types (apply 'vector (versor-get-alterations-possibility-types))
	  versor-alterations-type-index 0
	  versor-altering-mode t)
    (versor-alterations-trim-index)
    (use-local-map versor-altering-mode-map)
    ;; look among the possible values we can alter the selection to, to
    ;; find the first one that is the same as the initial value of the
    ;; selection
    ;; (message "Possible types are %S" versor-alterations-types)
    (catch 'done
      (while (< versor-alterations-type-index (length versor-alterations-types))
	(setq versor-alterations-index 0
	      versor-alterations-values (versor-alterations:get-type-values t))
	;; (message "For type %d, values are %S" versor-alterations-type-index versor-alterations-values)
	(let ((n (length versor-alterations-values)))
	  (while (< versor-alterations-index n)
	    (when (string= versor-alterations-old-value
			   (car (aref versor-alterations-values versor-alterations-index)))
	      ;; (message "Got one in type %d" versor-alterations-type-index)
	      (throw 'done nil))
	    (incf versor-alterations-index)))
	(incf versor-alterations-type-index))
      (message "Could not find an alteration possibility matching the initial selection"))
    (versor-alterations:show-current)
    (versor-set-current-item (car versor-alterations-old-item)
			     (cdr versor-alterations-old-item))))

(defun versor-end-altering-item ()
  "Take the currently selected value of the item, and quit alteration mode."
  (interactive)
  (versor-as-motion-command current-item
    (setq versor-alterations-types nil
	  versor-alterations-type-name ""
	  versor-altering-mode nil)
    (versor-set-status-display)
    ;; fix up the undo list, to have just the removal of our last
    ;; insertion, and the reinstatement of the original selection
    (setq buffer-undo-list (cons nil (cons (second buffer-undo-list) versor-alterations-old-undo)))
    ))

(defun versor-abandon-altering-item ()
  "Take the original value of the item, and quit alteration mode."
  (interactive)
  (versor-as-motion-command old-item
    (delete-region (car old-item) (cdr old-item))
    (goto-char (car old-item))
    (insert versor-alterations-old-value)
    (setq versor-alterations-types nil
	  versor-alterations-type-name ""
	  versor-altering-mode nil)
    (versor-set-status-display)
    (setq buffer-undo-list (cdr versor-alterations-old-undo))
    ))

(define-minor-mode versor-altering-mode
  "Let the user change the contents of the versor selection, from a collection.
For example, they could switch between all the local variables in
scope at that point in the text."
  nil
  "**ALTERING**"
  nil
  (if versor-altering-mode
      (versor-begin-altering-item)
    (versor-end-altering-item)))

(provide 'versor-alter-item)

;;; versor-alter-item.el ends here
