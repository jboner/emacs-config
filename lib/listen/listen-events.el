; /* ====================================================================
;  * Copyright (c) 2004 Carnegie Mellon University.  All rights
;  * reserved.
;  *
;  * Redistribution and use in source and binary forms, with or without
;  * modification, are permitted provided that the following conditions
;  * are met:
;  *
;  * 1. Redistributions of source code must retain the above copyright
;  *    notice, this list of conditions and the following disclaimer. 
;  *
;  * 2. Redistributions in binary form must reproduce the above copyright
;  *    notice, this list of conditions and the following disclaimer in
;  *    the documentation and/or other materials provided with the
;  *    distribution.
;  *
;  * This work was supported in part by AT&T Labs, CMU Sphinx Speech
;  * Consortium and Clairgrove, LLC.
;  *
;  * THIS SOFTWARE IS PROVIDED BY CARNEGIE MELLON UNIVERSITY ``AS IS'' AND 
;  * ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
;  * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;  * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY
;  * NOR ITS EMPLOYEES BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;  * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
;  * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
;  * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
;  * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
;  * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
;  * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;  *
;  * ====================================================================
;  *
;  */
 
;; listen-events.el
 
(require 'cl)
(provide 'listen-events)

;;
(defvar listen-discarded-input nil)

;; LISTEN COMMAND OBARRAY

(defvar listen-ID 0
 "A number that was stamped on a recent listen event.")

;;; COMMAND TABLE

(defvar listen-command-table (make-vector (1- (expt 2 15)) 0)
 "The table describing all listen commands.  Each string consisting of
a sequence of command words is hashed to a symbol describing the
corresponding command.")

(defsubst listen-get-command (string)
  (intern-soft (downcase string) listen-command-table))

(defsubst listen-lookup-command (string)
  (intern (downcase string) listen-command-table))

(defsubst listen-make-command (commandwords type effect &optional
				    parameters estimate)
 "Insert a listen command into command table."
 (let ((entry (listen-lookup-command commandwords)))
       (put entry 'type type)
       (put entry 'effect effect)
       (put entry 'parameters parameters)
       (put entry 'estimate estimate)))

(defsubst listen-command-p (symb)
  "T if symb is a symbol describing a command (it is not checked that it actually sits in listen-command-table)."
  (and (symbolp symb) 
       ;; we can't, unfortunately, check in an efficient manner
       ;; whether the symbol actually sits in listen-command-table
       (member (get symb 'type)
	       '(listen-nullary listen-interactive listen-predicate))
       (get symb 'effect)))

(defun listen-set-command-property (name-segment-regexp 
				    property
				    value)
  (mapatoms 
   (function (lambda (symb)
	       (if (string-match name-segment-regexp (symbol-name symb))
		   (put symb property value))))
   listen-command-table))


(defsubst listen-command-words-p (string)
  (and (stringp string)
       (listen-get-command string)
       (get (listen-get-command string) 'type)))

(defun listen-make-prefix (commandwords)
 "Mark COMMANDWORDS as a prefix."
  (let ((entry (listen-lookup-command (downcase commandwords))))
       (put entry 'prefix t)))

(defun listen-is-prefix-p (commandwords)
 "True if COMMANDWORDS constitute a prefix of some command."
  (let ((entry (listen-get-command commandwords)))
    (and entry (get entry 'prefix))))

(defun listen-read-commands (&optional infile)
  (if (not infile)
      (setq infile (concat listen-path "/grammar.lcm")))
  (let ((b (create-file-buffer infile)))
    (set-buffer b)
    (insert-file-contents infile nil)
    (condition-case nil
	(while t 
	  (let ((next-description (read b)))
	    (apply 'listen-make-command next-description)))
    (end-of-file nil))
    (kill-buffer b)
    ;; calculate prefixes
    (let ((command-list nil))
      ;; find all commands inserted
      (mapatoms 
       (function 
	(lambda (symb)
	  (setq command-list (cons symb command-list))))
       listen-command-table)
      ;; process each command
      (while command-list
	(let ((word-list
	       (mapcar (function (lambda (s)
				   (if (symbolp s)
				       (symbol-name s)
				     (format "%s" s))))
		       (car (read-from-string
			     (let ((str
				    (concat "(" (symbol-name
						 (car command-list))
					    ")")))
			       ;; turn things like "x-y" into "x -y"
			       (if (string-match "-" str 0)
				   (replace-match " -" t t str)
				 str))))))
	      (prefix ""))
	  (while word-list
	    (if (string= prefix "")
		(setq prefix (car word-list))
	      (setq prefix (concat prefix 
				   (if (eq (aref (car word-list) 0) ?-)
				       ""
				     " ")
				   (car word-list))))
	    (setq word-list (cdr word-list))
	    (let ((entry (listen-lookup-command prefix)))
	      (put entry 'prefix t))))
	(setq command-list (cdr command-list))))))

;;; LISTEN EVENTS

(defun listen-event-p (event)
 "Return t if EVENT is a listen event.  Listen events are either do
events or redo events.  These are the four do events:

  (listen-nullary COMMANDWORDS ALIST ID)
  (listen-post-predicate COMMANDWORDS TEXT ALIST ID)
  (listen-predicate COMMANDWORDS TEXT ALIST ID)
  (listen-word TEXT ALIST ID)

COMMANDWORDS is string, which is interned in listen-command-table
obarray, ID is an integer.  ALIST contains keys such as 'nofix,
'capitalize, and 'phonetic.  A listen-word also may define a key
'spaces. All the above, except for word events, are known also as
command events.

Redo events are of the form

  (listen-redo IDINTERVAL EVENTLIST) 

where EVENTLIST is a list of do events and IDINTERVAL is a 
pair (BEGINID ENDID) of IDs of do events already executed."

  (and (consp event)
       (member (car event) 
	       '(listen-nullary listen-interactive
				listen-predicate listen-post-predicate
				listen-word listen-redo))
       (cond 
	((eq (car event) 'listen-nullary)
	 (and (eq (length event) 4) 
	      (stringp (nth 1 event))
	      (listp (nth 2 event)))
	 (atom (nth 3 event)))
	((eq (car event) 'listen-predicate)
	 (and (eq (length event) 5)
	      (stringp (nth 1 event)) 
	      (stringp (nth 2 event))
	      (listp (nth 3 event))
	      (atom (nth 4 event))))
	((eq (car event) 'listen-post-predicate)
	 (and (eq (length event) 5)
	      (stringp (nth 1 event)) 
	      (stringp (nth 2 event))
	      (listp (nth 3 event))
	      (atom (nth 4 event))))
	((eq (car event) 'listen-word)
	 (and (eq (length event) 4) 
	      (stringp (nth 1 event))
	      (listp (nth 2 event)) 
	      (atom (nth 3 event))))
	((eq (car event) 'listen-redo)
	 (and (eq (length event) 3)
	      (apply 'listen-and (mapcar 'integerp (nth 1 event)))
	      (eq (length (nth 1 event)) 2)
	      (apply 'listen-and 
		     (mapcar 'listen-do-event-p (nth 2 event))))))))

(defun listen-command-event-p (event)
  "Return t if EVENT is a listen command."
  (and (listen-event-p event)
       (member (car event) 
	       '(listen-nullary listen-post-predicate
		 listen-predicate))))


(defun listen-event-type (event)
  "Return the event type of a listen EVENT"
  (car event))

(defun listen-do-event-p (event)
 "Return t if EVENT is a listen do event."
  (and (listen-event-p event)
       (member (car event) 
	       '(listen-nullary 
		 listen-predicate listen-post-predicate
		 listen-word))))

(defun listen-redo-event-p (event)
 "Return t if EVENT is a listen redo event."
  (and (listen-event-p event)
       (eq (car event) 'listen-redo)))

(defun listen-word-event-p (event)
  "T if EVENT is a list word event"
  (and (listen-event-p event)
       (eq (car event) 'listen-word)))

(defun listen-event-command-words (event)
  "Return the command words of a listen do EVENT"
  (nth 1 event))

(defun listen-event-ID (event)
  "Return the ID of a listen do EVENT"
  (car (last event)))


(defun listen-event-alist (event)
  "Return the ALIST of a listen do EVENT"
  (car (last event 2)))

(defun listen-add-key-value-event (event status)
  "Return EVENT, but with a key-value pair (STATUS . t) added the
alist if it is not there already."
  (if (not (consp 
	    (assq status 
		  (listen-event-alist event))))
       (nconc 
	(subseq event 0 -2)
	(cons
	 (append `((,status . t))
		 (car (last event 2)))
	 (last event)))
    event))
 
(defun listen-replace-ID (event new-ID)
  "Return EVENT with its ID replaced by NEW-ID"
  (if (listen-do-event-p event)
      (append (subseq event 0 -1) (list new-ID))
    event))

;; bind speech commands to events
(global-set-key [listen-nullary] 'listen-heard-nullary)
(global-set-key [listen-predicate] 'listen-heard-predicate)
(global-set-key [listen-post-predicate] 'listen-heard-post-predicate)
(global-set-key [listen-word] 'listen-heard-word)
(global-set-key [listen-redo] 'listen-heard-redo)
(global-set-key [vox-repeat] 'listen-repeat-last-command-or-recording)

;; If the escape key has just been pressed, we still want to execute
;; the command
(global-set-key [?\e listen-nullary] 'listen-heard-nullary)
(global-set-key [?\e listen-predicate] 'listen-heard-predicate)
(global-set-key [?\e listen-post-predicate] 'listen-heard-post-predicate)
(global-set-key [?\e listen-word] 'listen-heard-word)
(global-set-key [?\e listen-redo] 'listen-heard-redo)
(global-set-key [?\e vox-repeat] 'listen-repeat-last-command-or-recording)

(global-set-key [?\C-x listen-nullary] 'listen-heard-nullary)
(global-set-key [?\C-x listen-predicate] 'listen-heard-predicate)
(global-set-key [?\C-x listen-post-predicate] 'listen-heard-post-predicate)
(global-set-key [?\C-x listen-word] 'listen-heard-word)
(global-set-key [?\C-x listen-redo] 'listen-heard-redo)
(global-set-key [?\C-x vox-repeat] 'listen-repeat-last-command-or-recording)

(global-set-key [?\C-c listen-nullary] 'listen-heard-nullary)
(global-set-key [?\C-c listen-predicate] 'listen-heard-predicate)
(global-set-key [?\C-c listen-post-predicate] 'listen-heard-post-predicate)
(global-set-key [?\C-c listen-word] 'listen-heard-word)
(global-set-key [?\C-c listen-redo] 'listen-heard-redo)
(global-set-key [?\C-c vox-repeat] 'listen-repeat-last-command-or-recording)











;;; EXAMINING THE PREVIOUS LISTEN-EVENT

(defun listen-previous-command-words ()
  (let ((previous-event
	 listen-last-listen-event))
    (cond ((listen-event-p previous-event)
	   (listen-event-command-words previous-event)))))

;;; GO AGAIN 
(defun listen-register-go-again-event ()
  "Assume that last-command-event is a listen command event.
Determine whether to make listen-last-listen-event the same as
last-command-event."
  (if (and 
       ;; not synthesized ID??
       (integerp (listen-event-ID last-command-event))
       ;; don't record secondary events 
       (not listen-secondary-events)
       ;; and not explicitly excluded
       (or (not (symbolp this-command))
	   (not (get this-command 'no-go-again))))
	   (setq
	     listen-last-listen-event last-command-event
	     listen-ephemeral-recorded-events nil)))  


(defun listen-heard-nullary (levent)
  (interactive "e")
  (assert (not listen-executing-nullary))
  (assert (listen-do-event-p levent))
  (assert (eq (listen-event-type levent) 'listen-nullary))
  ;;
  (save-match-data
    (setq listen-event-command-words (listen-event-command-words levent))
    ;; some Emacs code tries to test last-nonmenu-event (whether it is a
    ;; list) to see whether the current event is a mouse event
    (setq last-nonmenu-event t)
    ;;
    (let ((entry (listen-get-command listen-event-command-words))
	  (unread-events unread-command-events;; this is not needed?
			 ))
      (assert entry nil "heard-listen-nullary: %s not in listen-command-table" 
	      listen-event-command-words)
      (assert (eq (get entry 'type) 'listen-nullary))
      (let ((effect (get entry 'effect)))
	;; setting this-command is important for voice commands bound to
	;; e.g. previous-line (which looks at the last-command to
	;; determine whether to set a new temporary goal column)
	(setq this-command effect)
	(listen-register-go-again-event)
	(run-hooks 'listen-pre-hook)
	(listen-execute effect)))))

(defun listen-heard-predicate (levent)
  (interactive "e")
  (assert (listen-do-event-p levent))
  (assert (eq (listen-event-type levent) 'listen-predicate))
  (save-match-data
  (setq listen-event-command-words (listen-event-command-words levent))
  (setq listen-event-argument-words (nth 2 levent))
  (let ((entry (listen-get-command listen-event-command-words)))
    (assert entry nil  
	    "heard-listen-predicate %s not in listen-command-table"
	    listen-event-command-words)
    (assert (eq (get entry 'type) 'listen-predicate))
    (let ((effect (get entry 'effect)))
      (setq this-command effect)
      (listen-register-go-again-event) 
      (if (fboundp 'delete-selection-pre-hook)
	  (delete-selection-pre-hook))
      (listen-execute effect)))))


;;; This function handles commands of the form "something I say tair",
;;; which inserts "something I say" at the mouse position.
(defun listen-heard-post-predicate (levent)
  (interactive "e")
  (assert (listen-do-event-p levent))
  (assert (eq (listen-event-type levent) 'listen-post-predicate))
  (save-match-data
    (setq listen-event-command-words (listen-event-command-words levent))
    (setq listen-event-argument-words (nth 2 levent))
    (setq listen-event-fix (nth 3 levent))
    (let ((entry (listen-get-command listen-event-command-words)))
      (assert entry nil  
	      "heard-listen-post-predicate %s not in listen-command-table"
	      listen-event-command-words)
      (assert (eq (get entry 'type) 'listen-post-predicate))
      (let ((effect (get entry 'effect)))
	(setq this-command effect)
	(listen-register-go-again-event) 
	(if (fboundp 'delete-selection-pre-hook)
	    (delete-selection-pre-hook))
	(listen-execute effect))))) 

(defun listen-heard-word (levent)
  (interactive "e")
  (assert (listen-do-event-p levent))
  (assert (eq (listen-event-type levent) 'listen-word))
  (save-match-data
    (when (and (or delete-selection-mode 
		   (and (boundp 'cua-mode)
			cua-mode))
	       transient-mark-mode mark-active
	       (not buffer-read-only))
      (delete-region (point) (mark))
      (setq mark-active nil)
      (run-hooks 'deactivate-mark-hook))
    (setq this-command 'listen-heard-word)
    (let* ((text (nth 1 levent))
	   (alist (nth 2 levent))
	   (nofix (consp (assq 'nofix alist)))
	   (nofix-before (consp (assq 'nofix-before alist)))
	   (phonetic (consp (assq 'phonetic alist)))
	   (spaces (car-safe (assq 'spaces alist))))
      
      (if (string-match "\\(\n+\\)\\|\\( +\\)\\|\\(.+\\)"
		    text)
	  (cond ((match-beginning 1)
		 ;; return keys, change ^j into ^m
		 (listen-set-last) 
		 (listen-execute
		  (subst-char-in-string 10 13 text)))
		;; space keys
		((match-beginning 2)
		 (listen-set-last) 
		 (listen-execute text))
		;; text 
		((match-beginning 3)
		 (cond 
		  (buffer-read-only
		   (if (integerp (listen-event-ID levent))
		       ;; if it was a real speech
		       ;; event, not a synthesized one, then prepare event
		       ;; for repeat
		       (progn
			 (setq listen-last-listen-event levent)
			 (setq listen-ephemeral-recorded-events nil)))
		   ;;  then, try to look for the word 
		   (when (not (search-forward text nil t)) 
		     (goto-char (point-min))
		     (search-forward text nil t)))
		  (t;; (not buffer-read-only)
		   (listen-set-last 
		    ;; don't move "last" concept except if first word
		    ;; after speech began
		    t)
		   (let ((old-point (point-marker)))
		     (if (or (not (window-minibuffer-p (selected-window)))
			     (not listen-no-special-fix-in-minibuffer))
			 (progn
			   ;; no spaces
			   (insert text)
			   ;;, but do fix before and after
			   (unless (or nofix phonetic)
			     (listen-fix (point) t))
			   (unless (or nofix-before)
			     (save-excursion
			       (listen-fix old-point t (point))))
			   (if auto-fill-function
			       (funcall auto-fill-function)))
		       ;; else, in minibuffer and listen-no-special-fix-in-minibuffer
		       (cond ((eq listen-no-special-fix-in-minibuffer 'no-space)
			      (insert (remove* ?  text)))
			     ((eq listen-no-special-fix-in-minibuffer 'hyphen)
			      (insert (substitute ?- ?
						  text))))))))))))))

;		       (save-excursion
;			 (goto-char old-point)
;			 (listen-explicit-spacing t))))))))))))
   
(defun listen-heard-redo (levent)
  "Execute a listen redo event of the form (listen-redo IDLIST EVENTLIST). 
In particular, undo all events back to the first event described in the 
IDLIST of listen records, and execute EVENTLIST followed by the
events described by records later than the last record in IDLIST."
  (interactive "e")
  (assert (listen-redo-event-p levent))
  (save-match-data
  (let* ((undo-ID-interval (nth 1 levent))
	 (begin-ID-to-be-undone (nth 0 undo-ID-interval))
	 (end-ID-to-be-undone (nth 1 undo-ID-interval))
	 (new-events (nth 2 levent)))	; these replace the events in the
					; undo-ID-list 

    ;; first, see whether it is at all possible to undo back to the
    ;; first record in undo-ID-list---also check (for sanity) that the
    ;; end-ID-to-be-undone is encountered down the way back to the
    ;; first record; finally, make a list of all the events that
    ;; occurred after the one in the record end-ID-to-be-undone
    (let ((current-record listen-last-index) 
	  (remaining-records listen-recent-records-size)
	  (undo-possible t) ;; DELETE THIS VARIABLE
	  (end-ID-encountered nil)
	  (end-ID-record)
	  (done nil)
	  (events-to-be-played-back nil))
      (while (and (not done)
		  (> remaining-records 0)
		  undo-possible)
	;;	(if (not (ListenRecord-undoable
	;;		  (aref listen-recent-records current-record)))
	;;	    (setq undo-possible nil))
	;; see if we've reached end record
	(let ((current-event (ListenRecord-event
			      (aref listen-recent-records current-record))))
	  (if (and (listen-event-p current-event)
		   (eq (listen-event-ID current-event) end-ID-to-be-undone))
	      ;; then, we encountered the last record whose event is
	      ;; to be replaced
	      (progn
		(setq end-ID-encountered t
		      end-ID-record current-record))
	    ;; otherwise, if we are not below the last record yet
	    (if (not end-ID-encountered)
		;; and we're dealing with a primary event 
		(if (not (listen-get-record-field 
			  current-record secondary))
		    ;; then we'll gather the event in the list of events 
		    ;; to be played back
		    (setq events-to-be-played-back 
			  (if (not
			       (listen-get-record-field 
				current-record
				no-replay))
			      (cons
			       (listen-get-record-field 
				current-record
				event)
			       events-to-be-played-back)
			    events-to-be-played-back)))))
	  ;; check to see whether we reached the record corresponding to
	  ;; the begin event
	  (if (and (listen-event-p current-event)
		   (eq (listen-event-ID current-event) 
		       begin-ID-to-be-undone))
	      ;; if so, then done
	      (setq done t)
	    ;; otherwise, not done yet
	    (setq current-record
		  (mod (1- current-record)
		       listen-recent-records-size))
	    (decf remaining-records))))
      ;;
   
      (let ((number-to-be-undone
	     (1+ (- listen-recent-records-size remaining-records))))

	(assert (and end-ID-encountered
		   (<= number-to-be-undone listen-valid-records))
		nil 
		"Listen redo by speech: can't undo any more")
	(while (not (eq number-to-be-undone 0))
	  (decf number-to-be-undone)
	  (listen-undo-last-event-record)
	  (decf listen-valid-records)))
      
      ;; if we could undo, then this will hold:
      (assert (eq listen-last-index 
		  (mod (1- current-record) 
		       listen-recent-records-size)) t)
      (listen-go-away-from-listen-frame)
      ;; finally, insert the events to be redone followed by the ones
      ;; that are to be simply replayed 
      (listen-playback-events
       (append new-events events-to-be-played-back))))))

(defun listen-playback-events (event-list) 
  (assert (listp event-list))
  (setq unread-command-events
	(append event-list unread-command-events)))
