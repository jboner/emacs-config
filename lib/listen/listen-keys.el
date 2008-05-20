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

;;; KEY BINDINGS SUGGESTED FOR EMACS-LISTEN USERS.  THE BINDINGS
;;; SUGGESTED ALSO INCLUDE MOUSE FUNCTIONALITY THAT IS ACCOMPLISHED
;;; PREFERENTIALLY THROUGH THE USE OF FOOT-OPERATED CONTROL AND SHIFT
;;; KEYS (TO DISCOURAGE USE OF KEYBOARD).


;; change C-x b and others for substring matching
(if (boundp 'iswitchb-mode)
    (iswitchb-mode 1))

;;; KEYBOARD ASSIGNMENTS

;; suggested
;; (define-key global-map [?\C-x ?\C-b] 'listen-buffer-menu)
;; 
;; (define-key global-map [?\C-x ?b] 'listen-switch-to-buffer)

(global-set-key [f2] 'listen-undo)

;; REPEAT, F3 is often used repetitively (for searching) in Windows; here
;; use to it to repeat last command or recording
(global-set-key [f3] 'listen-repeat-last-command-or-recording)
(define-key isearch-mode-map '[f3] 
  'listen-repeat-last-command-or-recording)

(define-key global-map [f8] 'start-kbd-macro)
(define-key global-map [f9] 'end-kbd-macro)
(define-key global-map [f10] 'call-last-kbd-macro)
(define-key global-map [f11] 'scroll-other-window-down)
(define-key global-map [S-f11] 'scroll-other-window-down) 
(define-key global-map [f12] 'scroll-other-window)
(define-key global-map [S-f12] 'scroll-other-window)


;; UNDO
(global-set-key "\C-_" 'listen-conventional-undo)
(global-set-key "\C-X u" 'listen-conventional-undo)

(global-set-key [?\C-z] 'listen-undo)
(global-set-key [?\C-t] 'listen-redo)

;;; TAB

;; dabbrev-expand expands a prefix and is often useful for saving
;; keystrokes when typing long words or identifiers that have already
;; appeared
(global-set-key [C-tab] 'dabbrev-expand)

;;; SPACE

;; Go to last mark (instead of C-u C-SPC finger gymnastics)
(global-set-key [?\S- ] [?\C-u ?\C- ])

;;; ENTER    

(global-set-key [C-return] 'case-convert)  ; fix case
(global-set-key [C-S-return] '[?\M-q]) ; forward paragraph


;; MOUSE PHILOSOPHY

;; Ctrl + left button equivalent to middle button
;; Ctrl + right button kills selection
;; 
;; Ctrl + Shft + left button: popup yank
;; Ctrl + Shft + right button: popup buffer menu

;; Meta : secondary selection, useful for selecting and copying text
;; without moving point. 
;; Meta + double right click: paste secondary selection at point

;; Meta + Shft: for built-in functionality

;;; MOUSE

(global-set-key '[C-mouse-1] 'listen-do-mouse-2)
;; also in isearch mode: 
(define-key isearch-mode-map '[C-mouse-1] 'listen-do-mouse-2)

(global-unset-key [C-down-mouse-1]) ; where "buffer menu" is
					; which is moved to mouse-3
(define-key isearch-mode-map [C-down-mouse-1] 
  (lambda () (interactive) nil))
					; which is moved to mouse-3
(global-set-key [C-drag-mouse-1] 'listen-mouse-kill-region)


;; make left button with control + shift
(global-set-key '[S-C-down-mouse-1] 
		'(lambda(event)(interactive "e") 
		   (mouse-menu-choose-yank event)))

(global-set-key [M-mouse-1] 'mouse-start-secondary) 
(global-set-key [M-drag-mouse-1] 'mouse-set-secondary)
; (global-set-key [M-down-mouse-1] 'mouse-drag-secondary)
(global-set-key [M-down-mouse-1] 'listen-mouse-drag-secondary)
(global-set-key [M-S-mouse-1] 'mouse-yank-secondary)



;;; RIGHT BUTTON
(global-set-key '[S-C-down-mouse-3] 'mouse-buffer-menu)
(global-set-key [C-mouse-3] 	    'listen-mouse-instant-kill-region)
(global-unset-key [C-down-mouse-3]) ; where mouse-major-mode-menu is as a default
(global-set-key [M-S-down-mouse-3] 'mouse-major-mode-menu)


;;; MIDDLE  (just in case it were present)

(global-set-key [M-mouse-2]    
    '(lambda()(interactive) 
       (let ((mouse-yank-at-point t))
	 (mouse-yank-secondary nil))))

(global-set-key [S-down-mouse-2] 
    '(lambda(event)(interactive "e") 
       (mouse-menu-choose-yank event)))


;; MODE LINE MOUSE FUNCTIONALITY 

;; switch buffer
(global-set-key [mode-line mouse-1] 
    '(lambda()(interactive)(switch-to-buffer (other-buffer))))

;; [mode-line mouse-3] is bound to delete-window
; kill buffer
(global-set-key [mode-line C-mouse-3] 
    (function (lambda()(interactive)(kill-buffer (current-buffer)))))

; page up and down
(global-set-key [mode-line S-mouse-1] 
    '(lambda()(interactive)(scroll-down)))

(global-set-key [mode-line S-mouse-3] 
    '(lambda()(interactive)(scroll-up)))

; recenter
(global-set-key [mode-line S-mouse-2] 
    '(lambda()(interactive)(call-interactively 'recenter)))

;;; HELPER STUFF

;; This function was once part of the Emacs distribution 
;; it has been slightly modified.
(defun mouse-menu-choose-yank (event)
  "Pop up a menu of the kill-ring for selection with the mouse.
The kill-ring-yank-pointer is moved to the selected element.
A subsequent \\[yank] yanks the choice just selected."
  (interactive "e")
  (let* ((count 0)
	 (menu (mapcar (lambda (string)
			 (if (> (length string) yank-menu-length)
			     (setq string (substring string
						     0 yank-menu-length)))
			 (prog1 (cons string count)
			   (setq count (1+ count))))
		       kill-ring))
	 (arg (x-popup-menu event 
			    (list "Yank Menu"
				  (cons "Choose Next Yank" menu)))))
    ;; A mouse click outside the menu returns nil.
    ;; Avoid a confusing error from passing nil to rotate-yank-pointer.
    ;; XXX should this perhaps do something other than simply return? -rm
    (if arg
	(progn
	  ;; We don't use `rotate-yank-pointer' because we want to move
	  ;; relative to the beginning of kill-ring, not the current
	  ;; position.  Also, that would ask for any new X selection and
	  ;; thus change the list of items the user just chose from, which
	  ;; would be highly confusing.
  	  (setq kill-ring-yank-pointer (nthcdr arg kill-ring))
	  (if (interactive-p)
	      (message "The next yank will insert the selected text.")
	    (current-kill 0))
	  (call-interactively 'yank)))))

(put 'mouse-menu-choose-yank 'menu-enable 'kill-ring)

; Enable emulation of middle button 
(defun listen-do-mouse-2 (click &optional arg)
  (interactive "e\nP")
  (listen-click 'mouse-2))

;; When doing secondary drag, we'll always work in
;; "word" mode--that's faster and convenient
(defun listen-mouse-drag-secondary (start-event)
  (interactive "e")
  (mouse-minibuffer-check start-event)
  (let* ( (start-posn (event-start start-event))
	  (start-point (posn-point start-posn)))
    (if (and mouse-secondary-overlay
	     (eq (overlay-buffer mouse-secondary-overlay) 
		 (window-buffer (posn-window start-posn)))
	     (>= start-point
		 (overlay-start mouse-secondary-overlay))
	     (< start-point
		(overlay-end mouse-secondary-overlay)))
	(let ((old-point (point-marker)))
	  (insert
	   (with-current-buffer (overlay-buffer mouse-secondary-overlay) 
	     (buffer-substring (overlay-start mouse-secondary-overlay)
			       (overlay-end mouse-secondary-overlay))))
	  (let ((new-point (point-marker)))
	    (listen-fix (point))
	    (listen-fix old-point)
	    (indent-region old-point new-point nil)
	    (goto-char new-point)))
      ;; else
      (or mouse-secondary-start
	  (setq mouse-secondary-start (make-marker)))
      (set-marker mouse-secondary-start start-point)
      (if mouse-secondary-overlay
	  (delete-overlay mouse-secondary-overlay))
      ;; now make the event appear as if it was a double click by
      ;; faking the click-count part:
      (let ((start-event-modified 
	     (list (nth 0 start-event) (nth 1 start-event)  2)))
	(mouse-drag-secondary start-event-modified)))))
 
(defun listen-kill-region (event)
  (interactive "e")
    (mouse-set-region event)
  (call-interactively 'kill-region))

(defun listen-mouse-instant-kill-region (event)
  (interactive "e")
  (let ((oldpoint (point)))
    (mouse-set-point event)
    (kill-region oldpoint (point))))

(defun listen-case-convert ()
  (interactive)
  (let ((case case-fold-search))
    (setq case-fold-search nil)
    (if (looking-at "\\W*[A-Z]")
	(downcase-word 1)
      (capitalize-word 1))
    (setq case-fold-search case)))





