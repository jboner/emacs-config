; ; ; /* ====================================================================
; ;  * Copyright 2004 Barry Jaspan, <bjaspan@mit.edu>.  All rights reserved.
; ;  *
; ;  * Redistribution and use in source and binary forms, with or without
; ;  * modification, are permitted provided that the following conditions
; ;  * are met:
; ;  *
; ;  * 1. Redistributions of source code must retain the above copyright
; ;  *    notice, this list of conditions and the following disclaimer. 
; ;  *
; ;  * 2. Redistributions in binary form must reproduce the above copyright
; ;  *    notice, this list of conditions and the following disclaimer in
; ;  *    the documentation and/or other materials provided with the
; ;  *    distribution.
; ;  *
; ;  * THIS SOFTWARE IS PROVIDED BY Barry Jaspan ``AS IS'' AND 
; ;  * ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
; ;  * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; ;  * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL Barry Jaspan
; ;  * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
; ;  * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
; ;  * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
; ;  * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
; ;  * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
; ;  * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
; ;  * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
; ;  *
; ;  * ====================================================================
; ;  *
; ;  */


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LISTEN MODE ADDITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file contains code that enhances the use of EmacsListen, the
;;; ShortTalk Lisp source code when interpreted under the GNU EMACS
;;; editor.

;;; This code allows EmacsListen to be tied to NaturallySpeaking a
;;; specific commercial speech recognition engine.  The code is
;;; derived from vr-mode written by Barry Jaspan. 

;;; Barry has kindly allowed me to distribute this code under the BSD
;;; style license above.

;;; The code is not needed when EmacsListen is used with other speech
;;; recognition engines.


;;; Nils Klarlund

;;; ORIGINAL HEADER
;;;----------------------------------------------------------------------
;;
;; VR Mode - integration of GNU Emacs and Dragon NaturallySpeaking.
;;
;; Copyright 2004 Barry Jaspan, <bjaspan@mit.edu>.  All rights reserved.
;;
;; $Id: listen-vr-additions.el,v 1.1 2004/09/04 01:24:11 klarlund Exp $
;;
;; This file is part of Emacs VR Mode (http://emacs-vr-mode.SourceForge.net).
;;
;; Emacs VR Mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; Emacs VR Mode is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs VR Mode; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;; MODIFIED CODE
;;;----------------------------------------------------------------------

(remove-hook 'vr-cmd-make-changes-hook 'vr-cmd-make-changes)
(add-hook 'vr-cmd-make-changes-hook 'listen-vr-cmd-make-changes)

 
(remove-hook 'vr-cmd-get-buffer-info-hook 'vr-cmd-get-buffer-info)
(add-hook 'vr-cmd-get-buffer-info-hook 'listen-vr-cmd-get-buffer-info)

(defvar listen-fake-point nil "A terrible hack.")
(defvar listen-ignore-changes nil)

(defun listen-inject-events-now (event-list)
;;  (message "listen-inject-events-now %s %s" unread-command-events
;;  	   event-list)
  (setq unread-command-events
	(append unread-command-events
		event-list)))

(defun listen-inject-events (event-list)
  (assert (listp event-list))
  (setq unread-command-events
	(append unread-command-events event-list)))


(defun listen-vr-cmd-make-changes (vr-request) 
  ;; modified from vr-cmd-make-changes (vrmode 008)
  ;;Nils 8 lines
  (save-match-data
  (let ((inhibit-quit nil)
	(debug-on-quit t)
	(debug-on-error t)
	(listen-ignore-changes t)
	(original-buffer (current-buffer))
	(listen-window (frame-first-window listen-input-frame)))
    (with-current-buffer vr-listen-buffer
      (if (eq (current-buffer) vr-buffer)
	  (let ((start (vr-pte (nth 1 vr-request)))
		(num-chars (nth 2 vr-request))
		(text (nth 3 vr-request))
		(sel-start (vr-pte (nth 4 vr-request)))
		(sel-chars (nth 5 vr-request))
		vr-queued-changes)
	    (if (and buffer-read-only (or (< 0 num-chars) (< 0 (length text))))
		;; if the buffer is read-only we don't make any changes
		;; to the buffer, and instead we send the 
		;; the inverse command back
		(progn
		  (vr-log "make changes:Buffer is read-only %d %d\n"
			  num-chars (length text))
		  (let ((cmd (format "change-text \"%s\" %d %d %d %d %s"
				     (buffer-name) (vr-pfe start)
				     (+ (vr-pfe start) num-chars) (length text)
				     (buffer-modified-tick)
				     (string-make-unibyte
				      (vr-string-replace
				       (buffer-substring start (+ start num-chars))
				       "\n" "\\n")))))
		    (setq vr-queued-changes (cons cmd
						  vr-queued-changes))))
	    
	      ;; if it's not read-only we perform the changes as before
	      (when (eq num-chars 0)

		(let ((vr-ignore-changes 'self-insert))
		  (mapcar (lambda (c)
			    (let ((last-command-char 		       
				   c))
			      (self-insert-command 1)))
			  text))
		;; don't accept any change to vr-listen-buffer if it is
		;; the current buffer (except for "redo" stuff)
		(if (not (eq original-buffer vr-listen-buffer))
		    ;; parse that new stuff into listen do events
		    (listen-inject-events
		     (listen-make-do-events-from-text text start t)))) 
	      
	      ;; otherwise, not a simple insertion at the end--if something to
	      ;; redo, calculate to-be-undone-interval, the pair of the begin
	      ;; and end event spanning the events to be redone
	      (when (> num-chars 0)
		(let* ((end1 (+ start num-chars))
		       ;; major hack, sometimes things get out of sync
		       (end (or (when (> end1 (point-max))
				  (message "oooooooooooooooooooooooch")
;;				  (vr-resynchronize vr-listen-buffer)
				  (setq start (- start (- end1 (point-max))))
				  (point-max))
				end1))
		       (to-be-undone-interval nil)
		       ;; in NaturallySpeaking 5, correction does not follow
		       ;; recognition boundaries: a subinterval of a recognized
		       ;; utterance may be subjected to change -- thus, we want to
		       ;; identify the whole interval
		       (start-boundary 
			(previous-single-property-change (1+ start)
							 'listen-ID
							 (current-buffer) 
							 (point-min)))
		       (end-boundary
			(save-excursion 
			  (goto-char end)
			  (next-single-property-change end
						       'listen-ID
						       (current-buffer) 
						       (point-max))))
		       (position (1- end-boundary)))
	    
		  ;; start from high positions (not that it matters)
		  (while (>= position start-boundary)
		    (let ((ID (get-text-property position 'listen-ID
						 vr-listen-buffer)))
		      (when (not (eq ID (nth 0 to-be-undone-interval)))
			;; then we discovered a new ID
			(if to-be-undone-interval
			    ;; then we have a new first event to be undone
			    (setq to-be-undone-interval 
				  (list ID (nth 1 to-be-undone-interval)))
			  ;; otherwise, we have found the last event to be undone
			  (setq to-be-undone-interval (list ID ID))))
		      (decf position))) 
		  ;; now we know what events are to be undone,
		  ;; delete those characters from listen buffer as requested by 
		  ;; NaturallySpeaking
		  (let ((vr-ignore-changes 'delete))
		    (delete-region start (+ start num-chars)))
		  (goto-char start)
		  (let ((vr-ignore-changes 'self-insert))
		    (mapcar (lambda (c)
			      (let ((last-command-char c))
				(self-insert-command 1)))
			    text))
		  

		  (when (> start-boundary (point-min))
		    (let* ((ID-just-before 
			    (get-text-property (1- start-boundary)
					       'listen-ID))
			   (current-record listen-last-index) 
			   (remaining-records listen-recent-records-size)
			   (done nil))
		      ;; Locate event right before if possible
		      (while (and (not done)
				  (> remaining-records 0))
			(let ((current-event (ListenRecord-event
					      (aref listen-recent-records current-record))))
			  (if (and (listen-event-p current-event)
				   (eq (listen-event-ID current-event) ID-just-before))
			      (setq done t)
			    ;; Else, not done yet
			    (setq current-record
				  (mod (1- current-record)
				       listen-recent-records-size))
			    (decf remaining-records))))
		      (let ((listen-preceding-event ; used in
					; listen-spoken-to-written
					; called by
					; listen-make-do-events-from-text
			     (if done
				 (ListenRecord-event
				  (aref listen-recent-records current-record)))))
			
			;; Now, extract the listen do events from the
			;; new text into a listen redo event, which
			;; also contains the list of events to be
			;; undone; put that redo event at the end of
			;; unread-command-events

			;; emergency repair (this should always be true)
			(if (<= (+ end-boundary
				   (- (length text) 
				      num-chars)) (point-max))
			    (listen-inject-events
			     (list 
			      (list 
			       'listen-redo
			     to-be-undone-interval
			     (listen-make-do-events-from-text 
			      (let ((s (buffer-substring start-boundary 
							 (+ end-boundary
							    (- (length text) 
							       num-chars)))))
				;; remove bold face, etc, from s before its
				;; stuffed into an event
				(set-text-properties 0 (length s) nil s)
				s)
			      start-boundary
			      ;; don't make speech true, that is, don't
			      ;; try to influence last-position concept
			      ;; in too obscure ways
			      ))))
			  (debug)
			  (message "ALERT ALERT: listen-vr-additions")))))))


	    ;; Nils --end
	   
	    ;; NILS --> no
	    ;;  ;;we make the changes by inserting the appropriate
	    ;; 	    ;;keystrokes and evaluating them
	    ;; 	    (setq unread-command-events
	    ;; 		  (append unread-command-events
	    ;; 			  (listify-key-sequence text)))
	    ;; 	    (while unread-command-events
	    ;; 	      (let* ((event(read-key-sequence-vector nil))
	    ;; 		     (command (key-binding event))
	    ;; 		     (this-command command)
	      ;; 		     (last-command-char (elt event 0))
	      ;; 		     (last-command-event (elt event 0))
	    ;; 		     (last-command-keys event)
	    ;; 		     )
	    ;; 		(vr-log "key-sequence %s %s %s\n" event
	    ;; 			command last-command-char)
	    ;; 		(run-hooks 'pre-command-hook)
	    ;; 		(if (eq command 'self-insert-command)
	    ;; 		    (command-execute command nil)
	    ;; 		  (vr-log "command is not a self insert: %s\n"
	    ;; 			  command )
	    ;; 		  ;; send back a "delete command", since when
	    ;; 		  ;; command is executed it will send the insertion.
	    ;; 		  (let ((cmd (format "change-text \"%s\" %d %d %d %d %s"
	    ;; 				     (buffer-name) (vr-pfe (point))
	    ;; 				     (1+ (vr-pfe (point)))
	    ;; 				     1 (buffer-modified-tick) ""))
	    ;; 			(vr-ignore-changes 'command-insert ))
	    ;; 		    (setq vr-queued-changes (cons cmd
	    ;; 						  vr-queued-changes))
	    ;; 		    ;; exit-minibuffer is a command that does not
	    ;; 		    ;; return properly , so to avoid timeouts waiting
	    ;; 		    ;; for the replies, we put it in the deferred
	    ;; 		    ;; function
	    ;; 		    (if (memq command vr-nonlocal-exit-commands )
	    ;; 			(setq vr-deferred-deferred-function command )
	    ;; 		      (command-execute command nil))
	    ;; 		    (vr-log "executed command: %s\n" command)
	    ;; 		    ))
	    ;; 		(run-hooks 'post-command-hook)
	    ;; 		))); ends self-insert region

	    ;; whether or not we should put point where
	    ;; NaturallySpeaking wants is not so easy to decide.  If
	    ;; point is not there, dictation won't work correctly if
	    ;; there are characters in front of point.  On the other
	    ;; hand, keys can be bound to multiple characters, and
	    ;; deferred functions can move point in which case
	    ;; NaturallySpeaking has no idea where it should be.  This
	    ;; is some kind of heuristic.
	    (if (equal (length text) 0)
		;; this is a pure selection or cursor repositioning,
		;; just put it there
		(progn
		  (vr-log "make changes: putting point at %s\n" sel-start)
		  (goto-char sel-start))
	      ;; Text is being inserted, so we move point to where it
	      ;; should be relative to the end of the string we got from
	      ;; NaturallySpeaking.  This should work even if keys are
	      ;; bound to multiple characters, and surprisingly enough
	      ;; even if deferred functions have moved point completely!
	      (vr-log "make changes: positioning point relative\n")

	      ;; tricky with invisible text: if we're inserting
	      ;; characters, sel-start is really *relative*to point, and
	      ;; should be shifted as point. Actually, we don't need to
	      ;; shifted at all, since it's just start - sel-start
	      ;; that matters.
	      (goto-char (+ (point)
					;(- sel-start (+ start (length text)))))
			    (- (nth 4 vr-request) 
			       (nth 1 vr-request) (length text))))
	      )
	    (delete-overlay mouse-drag-overlay)
	    (if (equal sel-chars 0)
		(delete-overlay vr-select-overlay)
	      (move-overlay vr-select-overlay
			    sel-start (+ sel-start sel-chars)
			    (current-buffer))))
	    
	    ;; in any case, we send the replies and the queued changes.
	    (vr-log "sending tick\n")
	    (vr-send-reply (buffer-modified-tick))
	    (vr-send-reply (length vr-queued-changes))
	    
	    ;; Nils -- begin
	(assert (= 0 (length vr-queued-changes))) ; we should delete vr-queued-changes
	;; narrow so that stuff last dictated is nicely displayed near the
	;; right boundary of *listen* window
					;      (goto-char (point-min))
					;      (while (>= (- (point-max) (point)) (+ 30 listen-frame-width))
	;; ughh, 30???, infinite loop if sentence is longer than 30!!
					;	(goto-char (1+ (point)))
					;	(while (not (eq (get-text-property (point) 'face) 
					;			'listen-boundary))
					;	  (goto-char (1+ (point)))))

	;; narrow-to-region won't work with Barry's interface
	;;  (narrow-to-region (point) (1+ (buffer-size)))

	;; make sure to display (point-min) now (otherwise, window may already
	;; have moved to the right so that text is displaced to the left)
	;; PROBLEM: after new-line you may not see end of previous line (thus,
	;; correction not possible)
	(let ((line-start-pos 
	       (save-excursion (goto-char (point-max)) 
			       (beginning-of-line nil) 
			       (point))))
	  (set-window-start listen-window
			    (if (> (- (point-max) line-start-pos)
				   listen-frame-width)
				;; 10 is better than 1 
				;; (otherwise, vr-mode seems to get confused???)  
				;;				(+ (-
	;; (point-max) listen-frame-width) 5)
				0
			      ;; emacs 21 broken
			      line-start-pos)))
	;; make utterance boundaries visible in listen buffer
 	(when (> (length text) 0)
	  (listen-put-face start (1+ start)
			   'listen-boundary-face)
	  ;; raise listen frame, but only when text was inserted (when
	  ;; correction is in progress don't try (aarghh)
	  ;;	(setq listen-frame-raise t)
	  )
	;; make sure to be at end
	;;	(goto-char sel-start) if no selection specified,
	;; otherwise go to end of buffer
					;	(let ((new-point
					;	       (if (> sel-chars 0) 
					;		   (+ sel-start sel-chars)
					;		 (point-max)))
					;	      (mark-active nil))
					;	  (goto-char new-point)
					;	  (set-window-point (frame-first-window listen-input-frame) 
					;			    new-point))
	    
	(set-window-point listen-window 
			  (point-max))
	(goto-char (point-max))
	(setq listen-fake-point 
	      (and mark-active 
		   sel-start))
	(listen-mode-log "listen-vr-cmd-make-changes (vr-request)
   unread-command-events %s" unread-command-events)
;;; Nils -- end

	(mapcar 'vr-send-reply (nreverse vr-queued-changes))
	(if vr-deferred-deferred-function
	    (progn
	      (vr-log "executing deferred function in make-changes: %s\n"
		      vr-deferred-deferred-function)
	      (setq vr-deferred-deferred-deferred-function
		    vr-deferred-deferred-function )
	      (setq vr-deferred-deferred-function nil)
	      (fix-else-abbrev-expansion)
	      (vr-execute-command vr-deferred-deferred-deferred-function)))
	)
      ;; if the current buffer is not VR-buffer
      (vr-send-reply "-1"))
    t)
    ;; Nils next lines
    )))

(defun listen-vr-cmd-get-buffer-info (vr-request)
  ;; forced vrmode to always work on the that is buffer
  (let  ((old-frame (selected-frame)))
    (with-current-buffer vr-listen-buffer
      (select-frame listen-input-frame t)
      (select-window (frame-first-window listen-input-frame))
      (vr-cmd-get-buffer-info vr-request))
    (select-frame old-frame)))

(defun vr-post-command ()
  (add-hook 'post-command-hook 'vr-post-command)
  (if (overlayp vr-select-overlay)
      (delete-overlay vr-select-overlay))
  ;; vr-log created problems with active mark
;   (vr-log "post-command: %s %s %s\n" this-command
; 	  vr-cmd-executing (buffer-name))
  (if vr-emacs-cmds
      (progn
	;; nils commented out next line
	;;	(vr-maybe-activate-buffer (current-buffer))
	(if (and vr-cmd-executing t) ;  (eq vr-cmd-executing this-command))
; apparently this-command is not always set to the name of the
; command, for example kill-line is executed with "kill-region" in
; this-command, so this check doesn't really work
	    (progn
	      (vr-send-cmd (format "command-done %s" vr-cmd-executing))
	      (setq vr-cmd-executing nil)))
	)))


(if (not (fboundp 'vr-maybe-activate-buffer-original))
    (fset 'vr-maybe-activate-buffer-original
	  (symbol-function 'vr-maybe-activate-buffer)))

(defun vr-maybe-activate-buffer (buffer)
  (vr-maybe-activate-buffer-original vr-listen-buffer))

(defun vr-overlay-modified (overlay after beg end &optional len)
  ;; Nils inserted one line here
  (if (eq (overlay-buffer overlay) vr-listen-buffer)
  (vr-log " overlay modified: a:%s vro:%s %d %d %d: \"%s\"\n"
	  after (eq vr-overlay overlay) beg end (if after len 0)
	  (buffer-substring beg end))
  (vr-log "  modification stack: %d\n" (length vr-modification-stack))
  (if after
      (progn
	(vr-log "   %s %s \n" (car vr-modification-stack)
		(buffer-substring beg end))
	(if (equal (car vr-modification-stack) (buffer-substring beg end))
	    ;; the before and after text are the same, so so it's one of these
	    ;; funky changes we can ignore.
	    (vr-log "ignoring bogus change\n" );;nil
	  ;; they're not equal, so we call the modification routine like before.
	  (vr-report-change overlay after beg end len))
	(setq vr-modification-stack (cdr vr-modification-stack))
	(if (< 0 vr-overlay-before-count)
	    (setq vr-overlay-before-count (1- vr-overlay-before-count))))

    ;; for the before call, we just save the prechange string in the stack
    (setq vr-modification-stack (cons (buffer-substring beg end)
				      vr-modification-stack)))))

(defun vr-startup ()
  "Initialize any per-execution state of the VR Mode subprocess."
  (let* ((l (lambda (x)
             (cond ((eq x 'vr-default-voice-commands)
                    (mapcar l vr-default-voice-command-list))
		   ;; evaluate a list of commands if the name of the
		   ;; list is specified
                   ((condition-case e (listp (symbol-value x)) ('error nil))
                    (mapcar l (symbol-value x)))
                   ((symbolp x)
                    (vr-send-cmd
                     (concat "define-command "
                             (vr-strip-dash x) "|" (symbol-name x))))
                   ((and (listp x) (eq (car x) 'list))
                    (vr-send-cmd
                     (format "define-list %s|%s" (nth 1 x) (nth 2 x))))
                   ((and (consp x) (vectorp (cdr x)))
                    (vr-send-cmd
                     (format "define-command %s|%s" (car x) (cdr x))))
                   ((and (consp x) (symbolp (cdr x)))
                    (vr-send-cmd
                     (format "define-command %s|%s" (car x) (cdr x))))
                   ((and (consp x) (stringp (cdr x)))
                    (vr-send-cmd
                     (format "define-command %s|%s" (car x) (cdr x))))
                   (t
                    (error "Unknown vr-voice-command-list element %s"
                           x))
                   )
             )))
    (mapcar l (if (eq vr-voice-command-list t)
                  vr-default-voice-command-list
                vr-voice-command-list)))
  ;; don't set up these hooks until after initialization has succeeded
  (add-hook 'post-command-hook 'vr-post-command)
;; nk  (add-hook 'minibuffer-setup-hook 'vr-enter-minibuffer)
;; nk   (add-hook 'kill-buffer-hook 'vr-kill-buffer)
;;  (vr-maybe-activate-buffer (current-buffer))
  (run-hooks 'vr-mode-startup-hook)
  )


(provide 'listen-vr-additions)



