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
 
(require 'cl)

;;; Major issue: Use overlays (with 'window property) instead of
;;; copying text into peek buffer? That is, make the peek buffer equal
;;; to the buffer that's peeked into?  The problem is that the
;;; existence of a peek window should probably be transparent to the
;;; usual selection of windows for buffers etc.

(defvar listen-peek-mode nil)

;;; KEYMAP
(defvar listen-peek-keymap (make-sparse-keymap))
(define-key listen-peek-keymap [mouse-1]
  'listen-peek-mouse-select)
(define-key listen-peek-keymap [drag-mouse-1]
  'listen-peek-mouse-select)

(define-key listen-peek-keymap [mouse-2] 
  'listen-peek-mouse-other-select)

;;; delete the other windows before selecting
(define-key listen-peek-keymap [mouse-3] 
  'listen-peek-mouse-full-select)

;;; split current window before selecting
(define-key listen-peek-keymap [mode-line mouse-1] 
  'listen-peek-mouse-split-select)

(define-key listen-peek-keymap [mode-line mouse-3] 
  'listen-peek-kill-select)   

;;; COLORS
(defconst listen-peek-colors '(red blue green brown purple orange yellow pink
white))

(defconst listen-peek-colors-list
  (mapcar (function (lambda (c) (cons c c))) listen-peek-colors))

(defvar listen-peek-used-colors nil)

(defconst listen-peek-real-colors '((red . "Salmon")  
				      (blue . "LightSkyBlue1")  
				      (green . "medium spring green")
				      (orange . "orange")
				      (yellow . "lemon chiffon")
				      (brown . "sandy brown")
				      (purple . "light slate blue")
				      (pink . "PaleVioletRed1") 
				      (white . "white")))
(defvar listen-peek-window-faces)

(defconst listen-peek-number-colors 8)

(defvar listen-peek-window-height 5)
(defvar listen-peek-frame-width '20)

;;; GLOBAL VARS 
(defvar listen-peek-frame nil "Frame for peek windows.")
(defvar listen-peek-main-frame (selected-frame) "Frame where buffers selected 
in peek frame are displayed.")
(defvar listen-peek-display-table (make-display-table))

(set-display-table-slot listen-peek-display-table 'truncation ?#)
;;			(make-char 'latin-iso8859-4 133))
;;		(make-char 'ascii 133))

;;; FACES
(defun listen-peek-face (color)
  (intern (concat "listen-face-" (symbol-name color))))

;;; OVERLAYS
(defun listen-peek-overlay-symbol (i)
  (intern (concat "listen-" (prin1-to-string i))))

(defun listen-peek-overlay (i)
  (eval (listen-peek-overlay-symbol i)))

;;; WINDOW

(defvar listen-peek-windows 
  (make-vector listen-peek-number-colors nil)
  "For each w, the window that window number w is associated with.")  

(defvar listen-peek-yank-window nil
  "The peek window for the kill ring.")  

(defvar listen-search-window
  "The peek window for search strings.")

(defvar listen-peek-to-window-num
  (make-vector listen-peek-number-colors nil)
  "For each p, the window number that peek buffer p is shown in.")

(defvar listen-peek-numb-to-peek 
  (make-vector listen-peek-number-colors nil)
  "For each w, the peek buffer number shown in w.")

(defun listen-peek-to-window-update ()
  (loop for w from 0 below listen-peek-number-colors do
	(aset listen-peek-to-window-num
	      (aref listen-peek-numb-to-peek w) w)))

;;; COLOR TO WINDOW
(defvar listen-peek-map nil "Color to peek buffer mapping.")  
(defun listen-peek-from-color (color) 
  (cdr (assq color listen-peek-map)))
(defun listen-peek-color-from-peek (peek) 
  (car (rassq peek listen-peek-map)))

;;; PEEK BUFFERS
(defun listen-peek-buffer-name (i) (concat "listen-peek-"
					     (prin1-to-string i)))

(defun listen-peek-numb (buf)
  "Given a peek buffer BUF, return the corresponding number."  
  (assert (string-match "[0-9]+" (buffer-name buf)))
  (car (read-from-string  (buffer-name buf)
			  (match-beginning 0)
			  (match-end 0))))

(defvar listen-peek-buffers 
  (make-vector listen-peek-number-colors nil)
  "The peek buffers.")  

(defvar listen-peek-yank-buffer nil
  "The buffer that holds the text corresponding to the first entries
in the kill ring.")  

(defvar listen-peek-search-buffer nil
  "The buffer that holds the text of the last searches.")

(defvar listen-peek-shown (make-vector listen-peek-number-colors
					 nil)
  "The entry for peek buffer i is a pair (buffer pos) or nil")

;;; MISCELLANEOUS VARIABLES

(defvar listen-peek-kill-ring-yank-pointer nil
  "The last value of the kill-ring-yank-pointer for which the peek
display was updated.")

(defvar listen-peek-listen-search-history nil)

;;; MODE  

(defun listen-peek-display-blank-in-peek-buffer (p)
  (with-current-buffer (aref listen-peek-buffers p)
    (loop repeat (window-height) do
	  (loop repeat (window-width) do
		(insert " "))
	  (insert "\n"))
    (beginning-of-buffer)
    (move-overlay (listen-peek-overlay p)
		  (point-min) (point-max))))

(defun listen-peek-turn-on ()
  (add-hook 'post-command-hook
	    'listen-peek-update-peek-window)
  (add-hook 'kill-buffer-hook 'listen-peek-kill-buffer)
  (let ((old-frame (selected-frame)))
    (setq listen-peek-used-colors 
      (subseq listen-peek-colors 0 listen-peek-number-colors))
    (setq listen-peek-frame
	  (make-frame 
	   (nconc 
	    (list
	     (cons 'width listen-peek-frame-width)
	     ;; below, we add 2 to account for the yank and the search
	     ;; peek windows 
	     (cons 'height (* (+ listen-peek-number-colors 2)
			      listen-peek-window-height)))
	    '((name . "PEEK")(left . -1) (top . 0) 
	      (cursor-type . bar) (vertical-scroll-bars . nil)
	      (tool-bar-lines .  nil)
	      (minibuffer . nil) (menu-bar-lines . 0)
	      (font . "-*-Courier-New-*-*-*-*-12-*-*-*-*-*-iso8859-1")))))
    (raise-frame old-frame)
    (select-frame old-frame)
    ;; create yank window and yank buffer
    (setq listen-peek-yank-window
	  (frame-first-window listen-peek-frame))
    (setq listen-peek-yank-buffer (get-buffer-create "*Yank*"))
    (with-current-buffer listen-peek-yank-buffer
      (setq truncate-lines t))
    (set-window-buffer listen-peek-yank-window
		       listen-peek-yank-buffer)
    ;; create search window and search buffer
    (setq listen-search-window
	  (split-window listen-peek-yank-window
			listen-peek-window-height))
    (setq listen-peek-search-buffer
	  (get-buffer-create "*Search*"))
    (with-current-buffer listen-peek-search-buffer
      (setq truncate-lines t))
    (set-window-buffer listen-search-window listen-peek-search-buffer)
    ;; create peek windows
    (aset listen-peek-windows 0
	  (split-window listen-search-window listen-peek-window-height))
    (loop for i from 1 below listen-peek-number-colors do
	(aset listen-peek-windows
	      i
	      (split-window 
	       (aref listen-peek-windows (1- i))
	       listen-peek-window-height)))
    ;; make faces
    (loop for color in listen-peek-used-colors do
	  (make-face (listen-peek-face color))
	  (set-face-background (listen-peek-face color) 
			       (cdr (assoc color
					   listen-peek-real-colors))))
    ;; create map
    (setq listen-peek-map nil)
    (let ((i 0) (c listen-peek-used-colors))
      (while c
	(setq listen-peek-map
	      (cons (cons (car c) i) listen-peek-map))
	(setq c (cdr c) i (1+ i))))
    ;; create buffers, associate them with the right window, and put
    ;; in spaces and overlays
    (loop for p from 0 below listen-peek-number-colors do
	  (aset listen-peek-buffers
		p 
		(get-buffer-create (listen-peek-buffer-name p)))
	  (aset listen-peek-numb-to-peek p p)
	  (aset listen-peek-shown p nil)
	  (set-window-buffer (aref listen-peek-windows p)
			     (aref listen-peek-buffers p))
	  (with-current-buffer 
	      (aref listen-peek-buffers p)
	    (text-mode)
	    (use-local-map listen-peek-keymap)
	    (setq truncate-lines t)
	    (setq mode-line-format "")
	    (delete-region (point-min) (point-max))
            (set-window-display-table (aref listen-peek-windows p)
	     				      listen-peek-display-table)
	    (set (listen-peek-overlay-symbol p) 
		 (make-overlay (point-min) (point-max)
			       (aref listen-peek-buffers p) 
			       t t))
	    (listen-peek-display-blank-in-peek-buffer p)
	    (bury-buffer (current-buffer))))

    (listen-peek-to-window-update)
    ;; set the 'face property of overlays
    (loop for color in listen-peek-used-colors do
	  (overlay-put (listen-peek-overlay 
			(listen-peek-from-color color)) 
		       'face 
		       (listen-peek-face color))))) 

(defun listen-peek-turn-off ()
  (if listen-peek-frame
      (delete-frame listen-peek-frame))
  (loop for p from 0 below listen-peek-number-colors do
	(if (buffer-live-p (aref listen-peek-buffers p))
	    (kill-buffer (aref listen-peek-buffers p))))
  (remove-hook 'post-command-hook
	       'listen-peek-update-peek-window)
  (remove-hook 'kill-buffer-hook 'listen-peek-kill-buffer))

(defun listen-peek-mode (&optional arg)
  "Listen peek mode (not a traditional mode)"
  (interactive "P")
  (setq listen-peek-mode
        (if (null arg)
            (not listen-peek-mode)
          (> (prefix-numeric-value arg) 0)))
  (if listen-peek-mode
      (listen-peek-turn-on)
    (listen-peek-turn-off)))

(defun listen-peek-color-select (color &optional pre-select-action)
  (let ((c (if (stringp color) (intern color) color)))
    (assert (member c listen-peek-colors))
    (listen-peek-select 
     (listen-peek-from-color c) 
     pre-select-action)))
			       
(defun listen-peek-mouse-select (event &optional pre-select-action)
  (interactive "e")
  (listen-peek-select 
   (listen-peek-numb (window-buffer (nth 0 (nth 1 event))))
   pre-select-action))

(defun listen-peek-free-up (peek-w)
  (let ((p (aref listen-peek-numb-to-peek peek-w)))
    (with-current-buffer
	(aref listen-peek-buffers p)
      (setq mode-line-format "")
      (delete-region (point-min) (point-max)))
      (listen-peek-display-blank-in-peek-buffer p)
    ;; shift colors below p window up one step
    (loop for w from peek-w below (1- listen-peek-number-colors) do
	  (aset listen-peek-numb-to-peek w
		(aref listen-peek-numb-to-peek (1+ w))))
    ;; last peek window is assigned to buffer formerly corresponding to
    ;; w
    (aset listen-peek-numb-to-peek 
	  (1- listen-peek-number-colors)
	  p)
    (aset listen-peek-shown p nil)
    (listen-peek-update-display)))

(defun listen-peek-select (p &optional pre-select-action)
  "Create a new peek window that tracks the current window and
buffer."
  (let* ((last-frame listen-peek-main-frame)
	 (indication (aref listen-peek-shown p))
	 (buffer-indicated (nth 0 indication))
	 (win-indicated (nth 1 indication))
	 (pos-indicated (nth 2 indication))
	 (w-start-indicated (nth 3 indication))
	 (last-window (frame-selected-window
		       last-frame))
	 (peek-w (aref listen-peek-to-window-num p)))
    (if  (buffer-live-p buffer-indicated)
	(progn
	  ;; shift colors above p's window down one step
	  (loop for w downfrom peek-w to 1 do
		(aset listen-peek-numb-to-peek w
		      (aref listen-peek-numb-to-peek (1- w))))
	  (aset listen-peek-numb-to-peek 0 p)
	  (listen-peek-update-display)
	  (select-frame last-frame)
	  (raise-frame)
	  (if pre-select-action (funcall pre-select-action))
	  (set-window-buffer (selected-window) buffer-indicated)
	  (goto-char pos-indicated)
	  (set-window-start (selected-window) w-start-indicated)
	  ;; peek p now corresponds to a new window
	  (aset listen-peek-shown p
		(list buffer-indicated (selected-window) 
		      (point) (window-start)))
	  
	  p)
      ;; buffer-indicated has been killed
      (listen-peek-free-up peek-w)
      nil)))

(defun listen-peek-kill-buffer ()
  (loop for p from 0 below listen-peek-number-colors do
	(let* ((indication (aref listen-peek-shown p))
	       (buffer-indicated (nth 0 indication))
	       (peek-w (aref listen-peek-to-window-num p)))
	  (when (eq (current-buffer) 
		    (nth 0 (aref listen-peek-shown p)))
	    (listen-peek-free-up peek-w)))))

(defun  listen-peek-kill-select (event)
  "Bound to mouse event in a peek buffer."
  (interactive "e") 
  (let* ((p (listen-peek-numb 
	     (window-buffer (nth 0 (nth 1 event)))))
	 (indication (aref listen-peek-shown p))
	 (buffer-indicated (nth 0 indication))
	 (peek-w (aref listen-peek-to-window-num p)))
    (when (buffer-live-p buffer-indicated)
      (kill-buffer buffer-indicated)
      (select-frame listen-peek-main-frame)
      (raise-frame listen-peek-main-frame))))

 (defun listen-peek-mouse-split-select (e)
  "Bound to mouse event in a peek buffer."  
  (interactive "e")
  (listen-peek-mouse-select 
   e 
   (function (lambda ()
	       (select-window 
		(split-window-vertically))))))

(defun listen-peek-mouse-other-select (e)
  "Bound to mouse event in a peek buffer."  
  (interactive "e")
  (listen-peek-mouse-select 
   e 
   (function (lambda ()
	       (if (eq (selected-window) (next-window nil 'ignore nil))
		   ;; then, create another window
		   (split-window))
	       (select-window (next-window nil 'ignore nil))))))



(defun listen-peek-mouse-full-select (e)
  "Bound to mouse event in a peek buffer."  
  (interactive "e")
  (listen-peek-mouse-select e) 
  (delete-other-windows))

(defun listen-peek-find-peek (&optional current-buffer 
					  selected-window)
  "Look for the first (top-most) peek window whose description matches
that of the current window (or the parameters is given explicitly).
Such a peek window may not exist, in which case nil is returned."
  (if (null current-buffer) (setq current-buffer (current-buffer)))
  (if (null selected-window) (setq selected-window (selected-window)))
  (loop for w from 0 below listen-peek-number-colors do
	(let* ((p (aref listen-peek-numb-to-peek w))
	       (indication (aref listen-peek-shown p))
	       (buffer-indicated (nth 0 indication))
	       (win-indicated (nth 1 indication)))
	  (if (and (eq current-buffer buffer-indicated)
		   (eq selected-window win-indicated))
	      (return p)))))

(defun listen-peek-update-yank-window ()
  "Show entries in the kill ring, but only if kill-ring-yank-pointer
has changed since the last time the function was called.  This
condition is for efficiency reasons only.  It means that successive
kills by keyboard may not be displayed correctly."
  (if (not (eq listen-peek-kill-ring-yank-pointer
	       kill-ring-yank-pointer))
      (let ((local-kill-ring-yank-pointer kill-ring-yank-pointer))
	(with-current-buffer listen-peek-yank-buffer
	  (delete-region (point-min) (point-max))
	  (let ((pointer local-kill-ring-yank-pointer)
		(i 1)) 
	    (while (< i listen-peek-window-height)
	      (let ((yank-string (car-safe pointer)))
		(if yank-string
		    (insert
		     (number-to-string i)
		     ": "
		     (let ((string
			    (subst-char-in-string
			     ?\^j  ?\^m
			     (substring
			      yank-string 0 
			      (min 255 (length yank-string)))
			     t)))
		       (if (string-match "^[ \t][ \t][ \t][ \t\n]*" string)
			   (setq string
				 (concat "..."
					 (substring string (match-end 0)))))
		       string)
		     "\n")))
	      (setq pointer (cdr-safe pointer))
	      (incf i))))
	(setq listen-peek-kill-ring-yank-pointer
	      kill-ring-yank-pointer))))

(defun listen-peek-update-search-window ()
  (if (not (eq listen-peek-listen-search-history
	       listen-search-history))
	(with-current-buffer listen-peek-search-buffer
	  (delete-region (point-min) (point-max))
	  (let ((pointer listen-search-history)
		(i 1)) 
	    (while (< i listen-peek-window-height)
	      (let ((search-string (car-safe pointer)))
		(if search-string
		    (insert
		     (number-to-string i)
		     ": "
		     search-string
		     "\n")))
	      (setq pointer (cdr-safe pointer))
	      (incf i))))
	(setq listen-peek-listen-search-history
	      listen-search-history)))

(defun listen-peek-update-peek-window ()
  "This function, to be used in the post-command-hook, attempts to
track changes to the current buffer in the corresponding peek window
if it exists."    
  (let ((deactivate-mark))
    ;; first, update the yank peek window
  (listen-peek-update-yank-window)
  (listen-peek-update-search-window)
  (condition-case x
      (progn
	;; make sure listen-peek-frame is on top---this is inefficent but
	;; there is no way to detect that a frame has reacquired focus
	;; [actually, this doesn't work since it raises frame in between mouse
	;; down and up events and that seems to mess up things (lost mouse up events)
	;;  (if (eq (selected-frame) listen-peek-frame)
	;;      (raise-frame listen-peek-main-frame))

	(let*  ((p (listen-peek-find-peek))
		(buffer (current-buffer))
		(snippet-b (save-excursion 
			    (forward-line 
			     (- (1- 
				 (/ listen-peek-window-height 2))))
			    (point)))
		(snippet-e (save-excursion 
			     (forward-line 
			      (1+ (/ listen-peek-window-height 2)))
			     (point)))
		(snippet-point (- (point) snippet-b))
		(peek-w (if p (aref listen-peek-to-window-num p))))
	  (when (not (eq (selected-frame) listen-peek-frame))
	    ;; if not shown in top peek window, then move it to the top window
	    (when (and p (/= (aref listen-peek-to-window-num p) 0))
	      ;; shift colors above p down one step
	      (loop for w downfrom peek-w to 1 do
		    (aset listen-peek-numb-to-peek w
			  (aref listen-peek-numb-to-peek (1- w))))
	      (aset listen-peek-numb-to-peek 0 p)
	      (listen-peek-update-display))
	    ;; now p is shown in top window if non-nil, and if nil then 
	    ;; try to register
	    (if (not p)
		(progn
		  (setq p (listen-peek-register))))
	    (when p
	      (with-current-buffer 
		  (aref listen-peek-buffers p)
		(delete-region (point-min) (point-max))
		(goto-char (point-max))
		(insert-buffer-substring buffer
					 snippet-b 
					 snippet-e)
		(goto-char snippet-point)
		(save-excursion
		  (let ((no-spaces 1000))
		    (loop for p = (point-min) then (forward-line 1) do
			  (if (not (looking-at "^\\s-*$"))
			      ;; not line all blank
			      (setq no-spaces 
				    (min no-spaces (current-indentation)))
			    ;; looking at blank line, make it long so as to
			    ;; color the whole window)
			    (loop repeat (window-width) do
				  (insert " ")))
			  (if (eobp) (return)))
		    (indent-rigidly (point-min) (point-max) (- no-spaces))))
		;; fill out last line
		(loop repeat (window-width) do
		      (insert " "))
		(beginning-of-line)
		(move-overlay (listen-peek-overlay p)
			      (point-min) (point-max)))
	      (aset listen-peek-shown p
		    (list buffer (selected-window) 
			  (point) (window-start)))))))
    (error (message "lost it %s" x)))))
 
(defun listen-peek-register ()
"Register the current buffer as a peek window (unless excluded).
Return peek buffer number if registration succeeds."
  (interactive)
  (let* ((buffer (current-buffer))
	 (name (buffer-name buffer))
	 (pos (point))
	 (win (selected-window))
	 (w-start (window-start))
	 (p (aref listen-peek-numb-to-peek 
		  (1- listen-peek-number-colors))))
    ;; next line: emergency fix
    (if (buffer-live-p (aref listen-peek-buffers p))
	
	(when (not (string-match listen-peek-excluded-buffers name))
	  (aset  listen-peek-shown p (list buffer win pos w-start))
	  ;; show p in window 0 
	  (loop for w downfrom (1- listen-peek-number-colors) to 1 do
		(aset listen-peek-numb-to-peek w
		      (aref listen-peek-numb-to-peek (1- w))))
	  (aset listen-peek-numb-to-peek 0 p)
	  (listen-peek-update-display)
	  (with-current-buffer (aref listen-peek-buffers p)
	    (setq mode-line-format name)
	    (force-mode-line-update))
	  p))))

; (global-set-key [f12] 'listen-peek-register)
;; (add-hook 'find-file-hooks 'listen-peek-register)

(defun listen-peek-update-display ()
  "Update the peek frame according to the the assignment of peek
numbers to window numbers in listen-peek-numb-to-peek."
  (listen-peek-to-window-update)
  (loop for w from 0 below listen-peek-number-colors do
	(let ((peek-buffer 
	       (aref listen-peek-buffers
		(aref listen-peek-numb-to-peek w))))     
	  (when (and (window-live-p (aref listen-peek-windows w))
		     (buffer-live-p peek-buffer))
	    ;; this test should not fail, unless someone has messed
	    ;; with the peek frame
	    (set-window-buffer 
	     (aref listen-peek-windows w)
	     peek-buffer)
	    ;; let them show up at the bottom of the buffer list
	    (bury-buffer peek-buffer)))))


(defun listen-select-window-by-color ()
  (interactive)
  (let ((c 
	 (listen-argument listen-peek-colors-list
			  (listen-event-command-words
			   last-command-event)))
	(management 
	 (listen-argument listen-window-management
			  (listen-event-command-words
			   last-command-event) t)))  
    (cond  ((eq management 'split)
	    (split-window-vertically))
	   ((eq management 'full)
	    (delete-other-windows)))
    (listen-peek-color-select c)))
      
(global-set-key  [down-mouse-1] 'mouse-drag-region)
(global-set-key  [down-mouse-1] 
		 (function
		  (lambda (e)
		    (interactive "e") 
		    (call-interactively 'mouse-drag-region)
		    (let ((f (selected-frame)))
		      (if (frame-live-p listen-peek-main-frame)
			  (raise-frame listen-peek-main-frame))
		      (if (frame-live-p listen-peek-frame)
			  (raise-frame listen-peek-frame))
		      (if (frame-live-p listen-input-frame)
			  (raise-frame listen-input-frame))
		      (raise-frame f)))))

(provide 'listen-peek)