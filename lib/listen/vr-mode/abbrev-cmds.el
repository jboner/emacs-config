;; $Id: abbrev-cmds.el,v 1.1 2004/09/06 03:29:09 klarlund Exp $

;; (This file is based on pbvlisp.el, Copyright April 2000 Hans
;; van Dam <hans_van_dam@hetnet.nl>)

;; This file is not part of GNU Emacs.
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation.

(defvar pbv-else-abbrevs nil)
(setq pbv-else-abbrevs  '(
			  ;;move to the next placeholder
			  ("nextplace" "" (lambda ()  (deferred-execution 'else-next-placeholder)) 0)
			  ;; move to the previous placeholder
			  ("previousplace" "" (lambda ()  (deferred-execution 'else-previous-placeholder)) 0)
			  ;; expand the placeholder (the current or the next if not currently in a placeholder)
			  ;; this one is just a little more convenient to pronounce then expandplace
			  ("takeplace" "" (lambda ()  (deferred-execution 'else-expand-placeholder)) 0)
			  ;; expand the placeholder (the current or the next if not currently in a placeholder
			  ("expandplace" "" (lambda ()  (deferred-execution 'else-expand-placeholder)) 0)
			  ;; kill the placeholder (the current order next if not currently in the placeholder)
			  ("killplace" "" (lambda () (deferred-execution 'else-kill-placeholder)) 0)))
;;please set the following custom variables for ELSE to the best values for programming by voice:
;; '(else-prompt-time 0)
;; '(else-move-and-execute t)
;; '(else-set-lineno t)
;; '(else-kill-proceed-to-next-placeholder t)
;; '(else-expand-or-move t)


(defvar pbv-lisp-abbrevs
  '(
    ("isassignedvalue" "" (lambda () (electric-erase-word-boundaries) (lisp-is-assigned-value))  0)
    ("shortjump" "" (lambda () (electric-erase-word-boundaries) (following))  0)    
    ("cacheandassign" "" (lambda () (electric-erase-word-boundaries) (deferred-insert-cache-pad) (lisp-is-assigned-value)) 0)
    ) )

(defvar pbv-global-abbrevs nil)
(setq pbv-global-abbrevs 
  '(
    ("dash" "" (lambda () (electric-erase-word-boundaries) (dash-function)) 0)
    ("expandword" "" (lambda ()  (deferred-execution 'expand-a-word))
     0)
;    ("forwardchar" "" (lambda ()  (forward-char))     0)
    ("forwardchar" "" (lambda ()  (deferred-execution 'forward-char t))
     0)
    ("backwardchar" "" (lambda ()  (deferred-execution 'backward-char t))
     0)
    ("deletechar" "" (lambda ()  (deferred-execution 'delete-char))
     0)
    ("beginningofline" "" (lambda ()  (deferred-execution
					'beginning-of-line t))
     0)
    ("endofline" "" (lambda ()  (deferred-execution 'end-of-line t))
     0)
    ("forwardword" "" (lambda ()  (deferred-execution 'forward-word t))
     0)
    ("backwardword" "" (lambda ()  (deferred-execution 'backward-word t))
     0)
    ("killword" "" (lambda ()  (deferred-execution 'kill-word))
     0)
    ("backwardkillword" "" (lambda ()  (deferred-execution 'backward-kill-word))
     0)
    ("nextline" "" (lambda ()  (deferred-execution 'next-line t))
     0)
    ("previousline" "" (lambda ()  (deferred-execution 'previous-line t))
     0)
    ("killline" "" (lambda ()  (deferred-execution 'kill-line))
     0)
    ("templateparameter" "" (lambda () (deferred-execution 'c++-template-parameter))
     0)
    
    ))

(defun append-to-abbrev-table (table abbrev-list)
  "appends list of abbreviations to an abbreviation table."
  (while abbrev-list
    (let ((abbrev-element (car abbrev-list)))
      (define-abbrev table (car abbrev-element) (nth 1 abbrev-element) (nth 2 abbrev-element))
      (setq abbrev-list (cdr abbrev-list)))))

;; (append-to-abbrev-table global-abbrev-table pbv-else-abbrevs)
(append-to-abbrev-table global-abbrev-table pbv-global-abbrevs)
;; (append-to-abbrev-table lisp-mode-abbrev-table pbv-lisp-abbrevs)

(defvar deferred-function nil
  "function of which the execution is deferred until after the first self-insert.")

(defvar no-defer nil)

(defun lisp-cache-and-assign ()
  "lisp version of discrete cache and assign"
  (interactive)
  (insert-cache-pad)
  (lisp-is-assigned-value))

(defun lisp-is-assigned-value ()  
  "insert (setq before just uttered word."
  (interactive)
  (backward-word 1)
  (insert "(setq ")
  (forward-word 1)
  (insert " )")
  (backward-char)
  )

(defun deferred-execution (local-deferred-function &optional tricky)
  "defers the execution of deferred-function until after the next self-insert-command"
  (if no-defer
      (progn
	(call-interactively local-deferred-function)
	(setq no-defer nil))
    (setq deferred-function local-deferred-function)
    (setq tricky-deferred-function tricky)
;    (add-hook 'post-command-hook 'execute-deferred-execution nil t)))
    (add-hook 'after-change-functions 'execute-deferred-execution nil t)))

(defun execute-deferred-execution (beg end length)            
  (if (and deferred-function 
	   (not (and (boundp 'vr-mode) vr-mode 
		     (eq vr-buffer (current-buffer))))
	   ;; if this buffer is VR-activated, this function shouldn't
	   ;; do anything.  The deferred-function will be executed by
	   ;; VR mode.
	   )
      (progn
	;(vr-log "in execute-deferred-execution\n")
	(delete-backward-char 2)
	(call-interactively deferred-function)
	(setq deferred-function nil)))
)

(add-hook 'pre-abbrev-expand-hook 'handle-abbrev-expansion)

(defun handle-abbrev-expansion ()
  "makes ELSE behaved properly in combination with abbreviation expansions."
  (if (not (and (boundp 'vr-mode) vr-mode 
		     (eq vr-buffer (current-buffer))))
  (let ((remember-position (point)))
    (if (re-search-backward "\*\\(.*\\)" (line-beginning-position) t)
	(let ((the-word (match-string 1)))
					;	  (message the-word)
	  (if (and (abbrev-symbol the-word) (string= (symbol-value (abbrev-symbol the-word)) ""))
	      (progn
		(if (and (assoc the-word pbv-else-abbrevs)
			 (equal (point) else-placeholder-start)
			 (not (equal (point) 1))
			 )
		    (progn
		      (kill-word 1)
		      (if else-please-duplicate
			  (let ((separator (get  (else-look-up else-deleted-string ?p) else-separator-ref))
				thisblank)
			    (if (> (length separator) 0)
				(progn
				  (delete-char (length separator))
					;				  (message (concat "separator is: *" separator "*"))
				  )
			      )
			    ;;take care of vertical duplication:
			    (save-excursion
			      (beginning-of-line)
			      (setq thisblank (looking-at "[ \t]*$")))
			    (if thisblank
				(progn
				  (beginning-of-line)
				  (kill-line)
				  ))
			    (else-next-placeholder)				  
			    )
			(insert "[" else-deleted-string "]")
			(else-previous-placeholder))
		      (setq no-defer t)
		      (funcall (abbrev-symbol the-word))
		      ;;after executing the hook,and removing the word terminate theexpansion:
		      (error "don't worry: on purpose error in handle-abbrev-expansion.")))))
	  (goto-char remember-position))))))

(defun fix-else-abbrev-expansion ()
  "makes ELSE behaved properly in combination with abbreviation
expansions.  Specifically, reinserts the placeholder that's been
deleted.  "
(if (and (boundp 'else-mode) else-mode)
    ;; we should only do this if else mode is enabled
    (let ((remember-position (point)))
    (if (and (equal (point) else-placeholder-start)
	     (not (equal (point) 1))
	     )
	(progn
	  (if else-please-duplicate
	      (let ((separator (get  (else-look-up else-deleted-string ?p) else-separator-ref))
		    thisblank)
		(if (> (length separator) 0)
		    (progn
		      (delete-char (length separator))
					;				  (mess age (concat "separator is: *" separator "*"))
		      )
		  )
		;;take care of vertical duplication:
		(save-excursion
		  (beginning-of-line)
		  (setq thisblank (looking-at "[ \t]*$")))
		(if thisblank
		    (progn
		      (beginning-of-line)
		      (kill-line)
		      ))
		(else-next-placeholder)				  
		) 
	    (insert else-entire-deleted-string)
	    (vr-log "reinserting placeholder\n")
	    (else-previous-placeholder)) 
	  (goto-char else-placeholder-point))))))


