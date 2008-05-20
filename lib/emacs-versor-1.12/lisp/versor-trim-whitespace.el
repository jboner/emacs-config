;;;; versor-trim-whitespace.el -- trim whitespace after a versor command
;;; Time-stamp: <2007-11-20 14:42:04 jcgs>

;; Copyright (C) 2007, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2006?
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


;;; Commentary:
;; 

;;; Some of my keystrokes go into adjusting the whitespace after
;;; deleting or inserting something.  I think the adjustment is
;;; completely predicatable, so I'll try getting Emacs to do the work
;;; for me.  The end of a versor editing command is often the right
;;; time to adjust whitespace, hence doing it this way.  I doubt it
;;; would make a good general post-command-hook!

;;; todo: possibly use insert-for-yank?

;;; Code:
(defvar versor-trim-whitespace t
  "*Whether to adjust the whitespace around the place affected by each versor editing action.")

(defun versor-trim-whitespace (whereabouts)
  "Trim the whitespace around WHEREABOUTS."
  ;; Used from versor-yank (commented out),
  ;; languide-remove-surrounding-call. Is this function obsolescent?  I
  ;; think it's all moving towards versor-copy-region,
  ;; versor-kill-region, versor-adjust-whitespace, and
  ;; versor-adjusting-insert.
  (when versor-trim-whitespace
    (condition-case evar
	(save-excursion
	  (goto-char whereabouts)
	  (let ((syntax-before (save-excursion
				 (skip-syntax-backward "-")
				 (char-syntax (char-before))))
		(syntax-after (save-excursion
				(skip-syntax-forward "-")
				(char-syntax (char-after)))))
	    (when (and syntax-before syntax-after)
	      (languide-trim-whitespace syntax-before syntax-after))))
      (error nil))))

(defun versor-what-is-preceding (where)
  "Return a description of the whitespace or other characters preceding WHERE.
This can be:
  'blank-line if there is a blank line there
  'start      if it is the start of the line
  'margin     if there is only whitespace as far as the start of line
  'space      if there is whitespace there
  'opening    if there is some kind of opening bracket there
  'closing    if there is some kind of closing bracket there
  nil         otherwise."
  (save-excursion
    (cond
     ((progn
	(goto-char (1- where))
	(looking-at "\\s("))
      'opening)
     ((progn
	(goto-char (1- where))
	(looking-at "\\s)"))
      'closing)
     ((progn
	(goto-char where)
	(beginning-of-line 0)
	(looking-at "^\\s-*$"))
      'blank-line)
     ((progn
	(goto-char where)
	(bolp))
      'start)
     ((progn
	(goto-char where)
	(skip-syntax-backward "-" (line-beginning-position))
	(bolp))
      'margin)
     ((progn
	(goto-char (1- where))
	(looking-at "\\s-"))
      'space)
     (t nil))))

(defun versor-what-is-following (where)
  "Return a description of the whitespace or other characters following WHERE.
This can be:
  'blank-line if there is a blank line there
  'end        if it is the end of the line
  'margin     if there is only whitespace as far as the end of line
  'space      if there is whitespace there
  'closing    if there is some kind of closing bracket there
  nil         otherwise."
  (save-excursion
    (goto-char where)
    (cond
     ((looking-at "\\s)") 'closing)
     ((save-excursion
	(beginning-of-line 2)
	(looking-at "^\\s-*$"))
      'blank-line)
     ((save-excursion
	(skip-syntax-forward "-" (line-end-position))
	(eolp))
      'margin)
     ((eolp) 'end)
     ((looking-at "\\s-") 'space)
     (t nil))))

(defvar versor-debug-adjust-whitespace t
  "*Whether to say what's going on with the whitespace adjustment.")

(defun versor-copy-region (start end)
  "Copy the region between START and END.
It is labelled with descriptions of the neighbouring whitespace.
The descriptions of the whitespace are returned, as a cons."
  (let* ((text (buffer-substring start end))
	 (len (length text))
	 (preceding (versor-what-is-preceding start))
	 (following (versor-what-is-following end)))
    (when versor-debug-adjust-whitespace
      (message "versor-copy-region %S..%S..%S"
	       preceding
	       (buffer-substring-no-properties start end)
	       following))
    (put-text-property 0 len
		       'preceded-by
		       preceding
		       text)
    (put-text-property 0 len
		       'followed-by
		       following
		       text)
    (if (eq last-command 'kill-region)
	(kill-append  text (< end start))
      (kill-new text))
    (if (fboundp 'update-shown-stacks)	; from rpn-edit.el
	(update-shown-stacks))
    (cons preceding following)))

(defun versor-kill-region (start end)
  "Kill the region between START and END, labelling it with descriptions of the neighbouring whitespace."
  (let ((ws (versor-copy-region start end)))
    (delete-region start end)
    (when versor-adjust-whitespace
      (when versor-debug-adjust-whitespace
	(message "versor-kill-region %S..%S" (car ws) (cdr ws)))
      (versor-adjust-whitespace start
				(car ws) (cdr ws)
				" around deleted text"))))

(defun versor-whitespace-show-latest ()
  "Show the whitespace information for the top of the `kill-ring'.
For debugging."
  (interactive)
  (let* ((str (current-kill 0))
	 (copy (copy-sequence str))
	 (preceded-by (get-text-property 0 'preceded-by str))
	 (followed-by (get-text-property 0 'followed-by str)))
    (set-text-properties 0 (length str) nil copy)
    (message "%S %S %S"
	     preceded-by
	     copy
	     followed-by)))

(defun versor-whitespace-show-current()
  "Show the whitespace information for the current selection.
For debugging."
  (interactive)
  (versor-as-motion-command item
    (message "%S [...] %S"
	     (versor-what-is-preceding (versor-overlay-start item))
	     (versor-what-is-following (versor-overlay-end item)))))

(defvar versor-can-do-deferred-deletions t
  "Whether we can currently do deferred deletions.
This is not meant to be set; it gets bound to nil within some functions,
to hold off the deferred deletions.  Use `versor-defer-deletions' to control
whether deletions are deferred.")

(defvar versor-deferred-deletions nil
  "Things pending deletion resulting from `versor-deferred-delete'.
Each item is a cons of a cons of start and end, and an optional insertion.")

(make-variable-buffer-local 'versor-deferred-deletions)

(defvar versor-cancelling-deledendum nil)

(defun versor-delete-deledendum (old new)
  "point-left hook function for versor-deferred-delete."
  (when versor-can-do-deferred-deletions
    (let ((versor-cancelling-deledendum t))
      (message "Doing deferred deletions")
      (backtrace)
      (mapcar (lambda (update)
		(message "  deferred: deleting %d..%d (and inserting %S)" (first update) (second update) (third update))
		(when (overlayp (fourth update))
		  (delete-overlay (fourth update)))
		(versor-perform-deletion
		 (first update) (second update) (third update)))
	      versor-deferred-deletions)
      (setq versor-deferred-deletions nil))))

(defun versor-cancel-deledendum (from to)
  "modification-hooks hook function for versor-deferred-delete."
  (when (and versor-can-do-deferred-deletions
	     (not versor-cancelling-deledendum))
    (let ((versor-cancelling-deledendum t))
      (message "Cancelling deferred deletions")
      (mapcar (lambda (update)
		(message "  cancelling %d..%d" (first update) (second update))
		(remove-text-properties
		 (first update) (second update)
		 '(point-left nil
			      modification-hooks nil
			      face nil)))
	      versor-deferred-deletions)
      (setq versor-deferred-deletions nil))))

(defun versor-perform-deletion (start end &optional insertion)
  "Delete text from START to END.  If INSERTION is given, insert it.
INSERTION may be a string to insert, or a form to eval at that place."
  (delete-region start end)
  (when insertion
    (goto-char start)
    (if (stringp insertion)
	(insert insertion)
      (eval insertion))))

(defun versor-deferred-delete (start end &optional insertion)
  "Delete the text between START and END when point leaves it.
If point is already out of it, delete the text immediately.
With optional INSERTION, insert that after the deletion,
if it is a string, otherwise evaluate it after the deletion.
If a change happens before point moves out of it, don't delete it.
If versor-defer-deletions is nil, just do an ordinary deletion immediately."
  (message "versor-deferred-delete %S..%S %S" start end insertion)
  (if versor-defer-deletions
      (if (and ;; nil
	   ;; if point is outside the text being deleted, we don't
	   ;; need to defer it, because the point of deferral is to
	   ;; avoid a deletion when you're about to type there; and
	   ;; anyway, it wouldn't work at the expected time, because
	   ;; it works by point leaving that text, and so won't be
	   ;; triggered until point goes in and out of that text
	   (or (< (point) start)
	       (>= (point) end)))
	  (versor-perform-deletion start end insertion)
	(let ((versor-can-do-deferred-deletions nil)
	      (deledendum-overlay (make-overlay start end)))
	  (push (list start end insertion deledendum-overlay)
		versor-deferred-deletions)
	  (overlay-put deledendum-overlay
		       'face (cons 'background-color "red"))
	  ;; todo: set fringes as well
	  (put-text-property start end 'point-left 'versor-delete-deledendum)
	  (put-text-property start end 'modification-hooks
			     (cons 'versor-cancel-deledendum
				   (get-text-property start
						      'modification-hooks)))
	  (goto-char start)))
    (versor-perform-deletion start end insertion)))

(defun versor-deferred-delete-horizontal-space (place)
  "Like `delete-horizontal-space' at PLACE, but using `versor-deferred-delete'."
  (save-excursion
    (goto-char place)
    (skip-syntax-backward "-")
    (let ((space-start (point)))
      (skip-syntax-forward "-")
      (versor-deferred-delete start (point)))))

(defun versor-deferred-one-blank-line-at (place)
  "Leave one blank line at PLACE.
If `versor-defer-deletions' is non-nil, don't actually do it until
point leaves the text that would be deleted; and cancel it if the
user makes changes in that text."
  (save-excursion
    (goto-char place)
    (message "versor-deferred-one-blank-line-at %d" (point))
    (skip-syntax-backward "->")
    (let ((start (point)))
      (skip-syntax-forward "->")
      (versor-deferred-delete start (point) "\n\n"))))

(defun versor-deferred-just-one-space (place)
  "Leave one space at PLACE.
If `versor-defer-deletions' is non-nil, don't actually do it until
point leaves the text that would be deleted; and cancel it if the
user makes changes in that text."
  (save-excursion
    (goto-char place)
    (message "versor-deferred-just-one-space at %S" place)
    (skip-syntax-backward "-")
    (let ((space-start (point)))
      (message "skipped back to %d" (point))
      (skip-syntax-forward "-")
      (versor-deferred-delete space-start (point) " "))))

(defun versor-deferred-delete-blank-lines (place)
  "Delete blank lines around PLACE.
If `versor-defer-deletions' is non-nil, don't actually do it until
point leaves the text that would be deleted; and cancel it if the
user makes changes in that text."
  (save-excursion
    (goto-char place)
    (message "versor-deferred-delete-blank-lines %d" (point))
    (skip-syntax-backward "->")
    (let ((start (point)))
      (skip-syntax-forward "->")
      (versor-deferred-delete start (point)
			      '(newline-and-indent)))))

(defun versor-deferred-delete-all-space (place)
  "Delete blank lines and horizontal whitespace around PLACE.
If `versor-defer-deletions' is non-nil, don't actually do it until
point leaves the text that would be deleted; and cancel it if the
user makes changes in that text."
  (save-excursion
    (goto-char place)
    (message "versor-deferred-delete-all-space %d" (point))
    (skip-syntax-backward "->")
    (let ((start (point)))
      (skip-syntax-forward "->")
      (versor-deferred-delete start (point)))))

(defun versor-deferred-newline-and-indent (place)
  "Like `newline-and-indent' at PLACE, but deferred."
  (save-excursion
    (goto-char place)
    (skip-syntax-backward "-")
    (let ((space-start (point)))
      (skip-syntax-forward "-")
      (versor-deferred-delete
       start (point)
       '(newline-and-indent)))))

(defun versor-adjust-whitespace (around
				 neighbouring-a neighbouring-b
				 &optional debug-label)
  "Adjust whitespace after an insertion or deletion.
The first argument AROUND is where the edit occurred.
Each of the following two arguments NEIGHBOURING-A and
NEIGHBOURING-B can be chosen from 'blank-line, 'end or 'start,
'margin, 'spaces, 'space, 'closing or 'opening, or nil.  If
NEIGHBOURING-A and NEIGHBOURING-B are the same, make sure there
is only one of whatever they are in the buffer at point.
When deleting a piece of text, NEIGHBOURING-A describes the text
just before the deleted piece, and NEIGHBOURING-B the text just
after it.
When inserting text, function `versor-adjust-whitespace' is called twice,
first with NEIGHBOURING-A being describing the text before the
insertion, and NEIGHBOURING-B describing the start of the
insertion; then with NEIGHBOURING-A describing the end of the
insertion, and NEIGHBOURING-B describing the text just after the
insertion.
Optional argument DEBUG-LABEL is an annotation for debugging output."
  (when versor-adjust-whitespace
    (when versor-debug-adjust-whitespace
      (unless debug-label
	(setq debug-label ""))
      (message "")
      (message "versor-adjust-whitespace%s at %s, neighbouring-a=%S neighbouring-b=%S"
	       debug-label
	       (point)
	       neighbouring-a
	       neighbouring-b))
    (save-excursion
      (goto-char around)
      (let ((versor-can-do-deferred-deletions nil))
	(if (eq neighbouring-b neighbouring-a)
	    (cond
	     ((eq neighbouring-a 'space)
	      (when versor-debug-adjust-whitespace
		(message "%s: both were spaces, leaving one space" debug-label))
	      (versor-deferred-just-one-space around)
	      (goto-char around))
	     ((eq neighbouring-a 'margin)
	      (when versor-debug-adjust-whitespace
		(message "%s: both were in margin, deleting blank lines" debug-label))
	      (versor-deferred-delete-blank-lines around))
	     ((eq neighbouring-a 'blank-line)
	      (when versor-debug-adjust-whitespace
		(message "%s: both were blank lines, leaving one blank line" debug-label))
	      (versor-deferred-one-blank-line-at around)))
	  (cond
	     ;; todo: blank-line overrides most other requirements
	   ((and (memq neighbouring-a '(end closing))
		 (eq neighbouring-b 'margin))
	    ;; we are inserting something that came from the start of
	    ;; a line, at the end of a line; typically, we want to
	    ;; keep it at the start of a line, so make a newline and
	    ;; indent
	    (when versor-debug-adjust-whitespace
	      (message "%s: neighbour-a was end/closing, neighbour-b was margin, newline-and-indent at %S" debug-label around))
	    (goto-char around)
	    (newline-and-indent))
	   ((eq neighbouring-a 'opening)
	    ;; we are inserting after an opening bracket; typically,
	    ;; we want the next thing to follow immediately, so close
	    ;; up horizontal and vertical space
	    (when versor-debug-adjust-whitespace
	      (message "%s: neighbouring-a was opening, closing up" debug-label))
	    (versor-deferred-delete-all-space around))
	   ((eq neighbouring-b 'closing)
	    ;; we are inserting before a closing bracket; typically,
	    ;; we want the next thing to precede immediately, so close
	    ;; up horizontal and vertical space
	    (when versor-debug-adjust-whitespace
	      (message "%s: neighbouring-b was closing, closing up" debug-label))
	    (versor-deferred-delete-all-space around))
	   ((eq neighbouring-a 'blank-line)
	    (when versor-debug-adjust-whitespace
	      (message "%s: neighbour-a was blank line, opening line at %S" debug-label around))
	    (versor-deferred-one-blank-line-at around))
	   ((eq neighbouring-a 'margin)
	    ;; todo: don't always want to do this
	    (if (eq neighbouring-b 'end)
		(progn
		  (when versor-debug-adjust-whitespace
		    (message "%s: neighbour-a was margin, neighbouring-b was end, delete-blank-lines at %S" debug-label around))
		  ( ;; versor-deferred-one-blank-line-at
		   versor-deferred-delete-blank-lines
		   around))
	      (when versor-debug-adjust-whitespace
		(message "%s: neighbour-a was margin, neighbouring-b was not end, newline-and-indent at %S" debug-label around))
	      (versor-deferred-newline-and-indent around)))
	   ((eq neighbouring-a 'space)
	    (when versor-debug-adjust-whitespace
	      (message "%s: neighbour-a was whitespace, inserting space at %S" debug-label around))
	    (versor-deferred-just-one-space around))))))))

(defun versor-adjusting-insert (string)
  "Insert STRING.
Use its text properties 'preceded-by and 'followed-by, in conjunction with
the results of `versor-what-is-preceding' and `versor-what-is-following'
called at the insertion point, to adjust the whitespace around the insertion.
STRING may be a single string, or a cons of two strings.  In the
latter case, the car of them is inserted first, the variable
`versor-mid-insertion-place' gets set to point, and then then cdr
of them is inserted.  (Otherwise, `versor-mid-insertion-place' is nil.)
Returns a cons of the start and end positions of where STRING itself
was inserted."
  (setq versor-mid-insertion-place nil)
  (let ((preceding-in-string (get-text-property
			      0
			      'preceded-by
			      (if (stringp string)
				  string
				(car string))))
	(preceding-in-buffer (versor-what-is-preceding (point)))
	(following-in-string (if (stringp string)
				 (get-text-property (1- (length string))
						    'followed-by string)
			       (get-text-property (1- (length (cdr string)))
						  'followed-by (cdr string))))
	(following-in-buffer (versor-what-is-following (point))))
    (when versor-debug-adjust-whitespace
      (message "versor-adjusting-insert %S:\"%S ... %S\":%S"
	       preceding-in-buffer preceding-in-string
	       following-in-string following-in-buffer))
    (let ((start (point-marker)))
      (set-marker-insertion-type start t)
      (versor-adjust-whitespace start
				preceding-in-buffer
				preceding-in-string
				" before inserted text")
      (when versor-debug-adjust-whitespace
	(message "after before-adjust, point is at %d, start is %S" (point) start))
      (goto-char start)
      (if (stringp string)
	  ;; todo: use insert-for-yank
	  (insert string)
	;; todo: use insert-for-yank
	(insert (car string))
	(setq versor-mid-insertion-place (point))
	;; todo: use insert-for-yank
	(insert (cdr string)))
      (let ((end (point)))
	(versor-adjust-whitespace end
				  following-in-string
				  following-in-buffer
				  " after inserted text")
	(cons start end)))))

(provide 'versor-trim-whitespace)

;;; versor-trim-whitespace.el ends here
