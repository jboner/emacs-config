;;; versor-selection.el -- versatile cursor
;;; Time-stamp: <2008-01-17 16:28:27 jcgs>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2005, 2006, 2007, 2008  John C. G. Sturdy
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; highlighting current selection ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar versor-items (list 'not-yet-set)
  ;; what was this old value? see if it works ok without it!
  ;; (list (cons (cons nil nil) nil))
  "The current versor items.
There is a separate value of this variable for each buffer.
Each item is represented by an overlay.
There is always at least one item in this list, to avoid churning
overlays; if there is no current item, the top item has nil for its
buffer.")

(defvar versor-latest-items nil
  "The items list as it was at the start of the current command,
but converted from overlays to conses of start . end, as the overlays
get cancelled on the pre-command-hook versor-de-indicate-current-item,
which we use to make sure we go away quietly when the user wants to type
other emacs commands.")

(mapcar 'make-variable-buffer-local
	'(versor-items
	  versor-latest-items))

(defun versor-set-current-item (start end)
  "Set the START and END of the current item, and get rid of any extra parts."
  (when versor-items
      (mapcar 'delete-overlay (cdr versor-items))
    (rplacd versor-items nil))
  (let ((item (and versor-items
		   (car versor-items))))
    ;; re-use overlay if there's one ready
    (if (and (overlayp item) (overlay-buffer item))
	(move-overlay item start end (current-buffer))
      (make-versor-overlay start end))))

(defun versor-add-to-current-item (start end)
  "Add the region between START and END to the current selection.
It is added as a separate piece."
  (versor-extra-overlay start end))

(defun versor-set-current-items (items)
  "Set the ITEMS, given as conses (or existing overlays) ."
  (mapcar 'delete-overlay versor-items)
  (if (null items)
      (setq versor-items nil)
    (setq versor-items
	  (mapcar (lambda (item)
		    (let ((overlay (if (overlayp item)
				       item
				     (make-overlay (car item) (cdr item)))))
		      (overlay-put overlay 'face
				   (if versor-use-face-attributes
				       'versor-item-face
				     'region))
		      overlay))
		  items))
    (goto-char (apply 'min (mapcar 'car items)))))

(defun versor-current-item-valid ()
  "Return whether there is a valid current item."
  (let ((item (car versor-items)))
    (and (overlayp item)
	 (overlay-buffer item))))

(defun versor-get-current-item ()
  "Return (start . end) for the current item.
If there are multiple items, return only the first one."
  (cond
   ((versor-current-item-valid)
    (when versor-debug-motion-framework
      (message "versor-get-current-item: current item valid"))
    (let ((olay (car versor-items)))
      (cons (overlay-start olay)
	    (overlay-end olay))))
   ((versor-valid-item-list-p versor-latest-items)
    (when versor-debug-motion-framework
      (message "versor-get-current-item: latest item valid"))
    (car versor-latest-items))
   (t
    (when versor-debug-motion-framework
      (message "versor-get-current-item: constructing item"))
    (versor-invent-item))))

(defun versor-last-item-first ()
  "Return the current versor items, sorted so the last comes first.
This is the sensible order in which to edit them (e.g. to delete
them), to avoid upsetting the buffer positions in the items you have
yet to edit."
  (setq versor-items
	(sort (versor-get-current-items)
	      (function
	       (lambda (a b)
		 (> (versor-overlay-start a)
		    (versor-overlay-start b)))))))

(defun versor-get-current-items ()
  "Return the current items."
  (let ((result
	 (cond
	  ((versor-current-item-valid)
	   (when versor-debug-motion-framework
	     (message "versor-get-current-items: current item valid"))
	   ;; versor-as-motion-command uses (versor-get-current-item) correspondingly to here
	   versor-items)
	  ((versor-valid-item-list-p versor-latest-items)
	   (when versor-debug-motion-framework
	     (message "versor-get-current-items: latest item valid"))
	   versor-latest-items)
	  (t
	   (when versor-debug-motion-framework
	     (message "versor-get-current-items: constructing item"))
	   (let ((pair (versor-invent-item)))
	     (make-versor-overlay (car pair) (cdr pair))
	     versor-items
	     )))
	 ))
    ;; make it into conses, in case we delete the overlays before the
    ;; result is used
    (mapcar (function
	     (lambda (item)
	       (if (overlayp item)
		   (cons (overlay-start item)
			 (overlay-end item))
		 item)))
	    result)))

(defun versor-overlay-start (olay)
  "Return the start of OLAY.
OLAY may be an overlay, or a cons used to preserve data from a destroyed overlay."
  (if (consp olay)
      (car olay)
    (overlay-start olay)))

(defun versor-overlay-end (olay)
  "Return the end of OLAY.
OLAY may be an overlay, or a cons used to preserve data from a destroyed overlay."
  (if (consp olay)
      (cdr olay)
    (overlay-end olay)))

(defun versor-display-item-list (label il)
  "With LABEL, display IL.
Meant for debugging versor itself."
  (message label)
  (mapcar (lambda (item)
	    (let ((start (versor-overlay-start item))
		  (end (versor-overlay-end item)))
	      (if (< (- end start) 16)
		  (message "  %d..%d: %s" start end
			   (buffer-substring start end))
	      (message "  %d..%d: %s..%s" start end
		       (buffer-substring start (+ start 8))
		       (buffer-substring (- end 8) end)))))
	  il))

(defun versor-invent-item ()
  "Invent a versor item around the current point.
Meant to be used by things that require an item, when there is none.
Leaves point in a position compatible with what has just been returned."
  ;; (message "Using %S in inventing item" (versor-current-level))
  (let* ((end (condition-case evar
		  (progn (funcall (or (versor-get-action 'end-of-item)
				  (versor-get-action 'next))
			      1)
		     ;; (message "invented end") (sit-for 2)
		     (point))
		(error
		 ;; (message "Could not find end when inventing item")
		 ;; todo: think of something tidier to do here; even better, reduce the occasions when it happens; the main one is "next sexp" when amongst a series of closing brackets
		 (point))))
	 (start (condition-case evar
		    (progn (funcall (versor-get-action 'previous) 1)
			   ;; (message "invented start") (sit-for 2)
			   (point))
		  (error
		   ;; (message "Could not find start when inventing item")
		   (point)))))
    ;; (message "Invented item %d..%d" start end)
    (cons start end)))

(defun versor-valid-item-list-p (a)
  "Return whether A is a valid item list."
  (and (consp a)
       (let ((cara (car a)))
	 (or (and (consp cara)
		  (integerp (car cara))
		  (integerp (cdr cara)))
	     (and (overlayp cara)
		  (bufferp (overlay-buffer cara)))))))

(defun versor-item-overlay ()
  "The (primary) item indication overlay for this buffer."
  (car versor-items))

(defun make-versor-overlay (start end)
  "Make a versor overlay at START END.
If there is already a versor overlay for this buffer, reuse that.
You should normally call versor-set-current-item rather than this."
  (unless (overlayp (versor-item-overlay))
    (let ((overlay (make-overlay start end (current-buffer))))
      (setq versor-items (list overlay))
      (overlay-put overlay 'face
		   (if versor-use-face-attributes
		       'versor-item-face
		     'region)
		   )))
  (move-overlay (versor-item-overlay) start end (current-buffer)))

(defun versor-extra-overlay (start end)
  "Make an extra versor overlay between START and END."
  (let ((overlay (make-overlay start end (current-buffer))))
    (overlay-put overlay 'face
		 (if versor-use-face-attributes
		     'versor-item-face
		   'region))
    (rplacd versor-items
	    (cons overlay
		  (cdr versor-items)))))

(defun delete-versor-overlay ()
  "Delete the versor overlay for this buffer."
  ;; in fact, we just disconnect it from the buffer,
  ;; to avoid having to keep creating and destroying overlays
  (let ((overlay (versor-item-overlay)))
    (when (overlayp overlay)
      (delete-overlay overlay)))
  ;; get rid of any extras
  (when versor-items
    (mapcar #'(lambda (o)
		(when (overlayp o)
		  (delete-overlay o)))
	    (cdr versor-items))
    (rplacd versor-items nil)))

(defun versor-clear-current-item-indication ()
  "Intended to go at the start of each versor motion command.
It clears variables looked at by versor-indicate-current-item, which sets them
up for itself only if the command didn't set them for it."
  (setq versor-indication-known-valid nil)
  ;; todo: we should remember what the indication was, I think this goes in versor-latest-items

  ;; if we do this, it can construct the wrong thing... for example,
  ;; if it should delete a pair of brackets, it deletes everything
  ;; between them instead... in such a case, I see it is running
  ;; "constructing items"

  ;; (versor-save-selection)

  (delete-versor-overlay))

(defvar versor-quiet-commands '(versor-out
				versor-in
				versor-next-meta-level
				versor-prev-meta-level)
  ;; todo: mention in manual
  "Commands after which the region description is not announced.")

(defvar versor-transients-for-other-end nil
  "Whether something has been done to show the end of the item.
If so, the value is either the overlay hiding the middle of the
item, or the window showing the end of the item, depending on
which was used.  It is set in `versor-indicate-current-item' and
used and cleared in `versor-de-indicate-current-item'.")

(defvar versor-hiding-middle-banner "\n...\n"
  "String for showing elision of a large selection.
Alternatively, it can be a function to call to return such a
string.  The function is given one argument, the number of lines
hidden to make the elision.
\(lambda (n)
   \(concat \"\n\"
            comment-start
            (format \"%d lines hidden\" n)
            comment-end \"\n\"))
is a suitable function.")

(defun versor-indicate-current-item (&optional no-adjust)
  "Make the current item distinctly visible.
This is intended to be called at the end of all versor commands.
See also the complementary function `versor-de-indicate-current-item',
which goes on the `pre-command-hook', to make sure that versor gets out
of the way of ordinary Emacs commands.
We assume point to be at the start of the item.
With optional NO-ADJUST non-nil, don't adjust point to the apparent
start of the likely selection."
  (condition-case error-var
      (progn

	;; The command we have just run may have set these for us, if
	;; it knows in such a way that it can tell us much faster than
	;; we could work out for ourselves (only relevant for commands
	;; written specially for versor -- if it's a command which
	;; came with GNUemacs, we have to work it out here); they are
	;; cleared at the start of each command by
	;; versor-clear-current-item-indication, which, like this
	;; function, is called by the versor-as-motion-command macro

	(unless (versor-current-item-valid)
	  (when versor-debug-motion-framework
	    (message "Establishing current item"))
	  (versor-set-current-item
	   (progn
	     (when (and versor-trim-item-starts-to-non-space
			(not no-adjust))
	       ;; To maintain consistency, it generally makes sense to
	       ;; adjust point so that it's at the start of some
	       ;; non-blank text; most commands will probably do this
	       ;; anyway, but here we just make sure, which makes it
	       ;; easier to borrow underlying commands not written
	       ;; specially for versor.
	       (let ((trimmer (versor-get-action 'start-of-item)))
		 (when trimmer
		   (when versor-debug-motion-framework
		     (message "trimming from %d" (point)))
		   (funcall trimmer)
		   (when versor-debug-motion-framework
		     (message "trimmed to %d" (point))))))
	     (point))
	   (versor-end-of-item-position)))

	    ;; Try to get the whole selection on-screen, if the user
	    ;; so desires.  First, we simply move it, if it will fit
	    ;; in the window.  Otherwise (according to
	    ;; versor-show-both-ends-of-item) we can either
	    ;; temporarily split the window, or make the middle of the
	    ;; invisible (and put dots there to indicate that this has
	    ;; been done)

	(when (or versor-try-to-display-whole-item
		versor-show-both-ends-of-item)
	  (let* ((items (versor-get-current-items))
		 (item (car items))
		 (end (cdar (last items))))
	    (when (> end (window-end))
	      (let* ((start (car item))
		     (lines-needed (count-lines start end))
		     (lines-available (- (window-height
					  (selected-window))
					 2)))
		(if (<= lines-needed lines-available)
		    ;; whole selection fits in the window
		    (recenter (/ (- lines-available lines-needed) 2))
		  ;; we can't get the whole selection visible in the
		  ;; window, but we can try to show both ends
		  (recenter 1)	       ; get the top onto the top line
		  (let ((extra (- lines-needed lines-available)))
		    (cond
		     ((eq versor-show-both-ends-of-item 'window)
		      ;; split the window to show the other end
		      ;; (cancelled at the start of the next command)
		      (let ((end-window (split-window)))
			(with-selected-window end-window
			  (goto-char end)
			  (message "Split window temporarily to show both ends")
			  (recenter -2)
			  (push end-window versor-transients-for-other-end))))
		     (versor-show-both-ends-of-item
		      ;; hide the middle of the selection (cancelled
		      ;; at the start of the next command)
		      (let ((lines-per-part (max (- (/ lines-available 2) 2)
						 0)))
			(save-excursion
			  (goto-char start)
			  (forward-line lines-per-part)
			  (let ((hide-start (point)))
			    (goto-char end)
			    (forward-line (- lines-per-part))
			    (let ((hide-end (point)))
			      (push (make-overlay hide-start hide-end)
				    versor-transients-for-other-end)
			      (overlay-put (car versor-transients-for-other-end) 'invisible t)
			      (overlay-put (car versor-transients-for-other-end) 'before-string
					   (cond
					    ((stringp versor-hiding-middle-banner)
					     versor-hiding-middle-banner)
					    ((functionp versor-hiding-middle-banner)
					     (funcall versor-hiding-middle-banner extra))
					    (t "\n...\n"))))))))
		     (t
		      (when (> extra 0)
			(message
			 "%d more lines of this item would not fit on screen"
			 extra))))))))))

	;; re-do this because it somehow gets taken off from time to time
	(add-hook 'pre-command-hook 'versor-de-indicate-current-item)
	;; Put up a textual description of the selection in the
	;; mini-buffer or the header line; this is probably the most
	;; sensible point to attach a spoken description in emacspeak.
	(when (and versor-describe-selection
		   (not (memq this-command versor-quiet-commands)))
	  (versor-describe-selection)))
    (error
     (progn
       ;; (message "Caught error %S in item indication" error-var)
       ;; (with-output-to-temp-buffer "*Backtrace for item indication*" (backtrace))
       (versor-de-indicate-current-item)))))

(defun versor-save-selection ()
  "Save the current selection, into `versor-latest-items'."
  (setq versor-latest-items
	(catch 'no-region
	  (mapcar
	   (lambda (item)
	     (when versor-debug-motion-framework
	       (message "  Saving item %S" item))
	     (if (overlayp item)
		 (cons (overlay-start item) (overlay-end item))
	       (if (condition-case error
		       (progn
			 (region-beginning)
			 nil)
		     (error t))
		   (throw 'no-region nil)
		 (cons (region-beginning) (region-end)))))
	   versor-items))))

(defun versor-de-indicate-current-item ()
  "Remove the current item marking.
Intended to go on `pre-command-hook', to make sure that versor gets out
of the way of ordinarily Emacs commands.  In case, however, the new
command is itself a versor command, we save the item marking as a list
of conses of start . end, in `versor-latest-items'."
  (dolist (versor-transient-for-other-end versor-transients-for-other-end)
    (cond
     ((windowp versor-transient-for-other-end)
      (delete-window versor-transient-for-other-end))
     ((overlayp versor-transient-for-other-end)
      (delete-overlay versor-transient-for-other-end))))
  (setq versor-transients-for-other-end nil)
  (unless (eq (car versor-items) 'not-yet-set)
    (versor-save-selection)
    ;; (versor-display-item-list (format "starting command %S" this-command) versor-latest-items)
    (setq languide-region-description "")
    (delete-versor-overlay)))

(provide 'versor-selection)

;;;; end of versor-selection.el
