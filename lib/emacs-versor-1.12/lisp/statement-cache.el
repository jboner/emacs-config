;;;; statement-cache.el -- cache statement data for statement-navigation.el
;;; Time-stamp: <2007-08-23 11:32:48 jcgs>

;; Copyright (C) 2007, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: around 2004
;; Keywords: convenience

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

(defvar statements-known nil
  "List of known statement positions (buffer-local).
Each element is
  (startpos endpos type (part start . end) (part start . end) ...)
and the list is sorted by starting position.
Whenever we change something in the buffer, we invalidate
all the data referring to things further down the buffer.")

(defvar latest-statement-known nil
  "The latest-used entry in statements-known.
This is kept as a handy cache.")

(mapcar 'make-variable-buffer-local
	'(statements-known
	  latest-statement-known))

(defun languide-after-change-function (start end length)
  "After a change, some of languide's cached information may be wrong, so throw it away."
  (let ((statements-prev statements-known)
	(statements statements-known))
    (while statements
      (let* ((statement (car statements))
	     (stastart (first statement)))
	(when (>= stastart start)
	  (rplacd statements-prev nil)
	  (setq statements nil)))
      (setq statements-prev statements
	    statements (cdr statements)))))

(defun statement-remember (start type &optional end)
  "Remember that there is a statement starting at START, of TYPE.
See the variable statements-known."
  ;; (message "remembering %S at %S..%S" type start end)
  ;; (message "Backtrace from statement-remember: %s" (with-output-to-string (backtrace)))
  (if statements-known
      (if (< start (first (car statements-known)))
	  ;; I think I can do the rest more clearly if I get this
	  ;; case out of the way first
	  (push (setq latest-statement-known (list start end type))
		statements-known)
	(let ((statements-prev statements-known)
	      (statements statements-known)
	      (done nil))
	  (while (and statements
		      (not done))
	    (let* ((statement (car statements))
		   (stastart (first statement)))
	      (cond
	       ((and (= start stastart)
		     ;; if the other information has not changed,
		     ;; don't replace it -- we want to keep the
		     ;; statement part information if possible
		     (or (not (eq (second statement) end))
			 (not (eq (third statement) type))))
		;; (message "Updating statement (%S %S..%S) to be (%S %S..%S)" (caddr statement) stastart (cadr statement) type stastart end)
		(rplacd statement (list end type))
		(setq latest-statement-known statement
		      done t))
	       ((> stastart start)
		;; (message "Found statement at %d which is beyond %d, so adding new one before that" stastart start)
		(rplacd statements-prev
			(setq statements
			      (cons (setq latest-statement-known (list start end type))
				    statements)))
		(setq done t))
	       (t
		)))
	    (setq statements-prev statements
		  statements (cdr statements)))
	  (unless done
	    ;; (message "reached end of statements list, so adding new one at end")
	    (rplacd statements-prev
		    (list
		     (setq latest-statement-known (list start end type)))))))
    ;; no statements known so far, so start the list
    (setq statements-known
	  (list
	   (setq latest-statement-known (list start end type)))))
  latest-statement-known)

(defvar statement-cache-enabled t
  "*Whether to use the statement cache.
Turn this off for debugging statement navigation.")

(defun statement-find (start)
  "Find the statement starting at START."
  (if statement-cache-enabled
      (if (and (consp latest-statement-known)
	       (= start (car latest-statement-known)))
	  latest-statement-known
	(assoc start statements-known))
    nil))

(defun statement-remember-part (statement-start type part selections)
  "Remember that for the statement starting at STATEMENT-START and of TYPE, the PART runs from the car to the cdr of each of SELECTIONS."
  (when part
    (let ((statement (statement-find statement-start)))
      (when (null statement)
	(setq statement (statement-remember statement-start type)))
      (when (null statement)
	(error "Null statement in statement-remember-part"))
      (let ((old-part (assoc part (cdddr statement))))
	;; (message "statement-remember-part got statement=%S part=%S" statement old-part)
	(if old-part
	    (rplacd old-part (cons part selections))
	  (rplacd (cddr statement)
		  (cons (cons part selections)
			(cdddr statement))))))))

(defun statement-forget-cache ()
  "Empty the statement cache."
  (interactive)
  (setq statements-known nil)
  (message "Statement cache cleared"))

(defun statement-find-part (start part)
  "For the statement at START, find cached PART as a list of conses of (start . end)."
  ;; (message "statement-find-part %S %S" start part)
  (let ((statement (statement-find start)))
    ;; (message "statement-find-part got statement %S" statement)
    (if statement
	(if (eq part 'whole)
	    (list (cons (car statement) (cadr statement)))
	  (cdr (assoc part (cdddr statement))))
      nil)))

(defun statement-display-cache ()
  "Display the cached statement data. Meant for debugging."
  (interactive)
  (with-output-to-temp-buffer (format "*Cached statement data for %s*" (buffer-name))
    (dolist (statement statements-known)
      (princ (format "%5S-%5s: %-30S%s\n"
		     (car statement)
		     (if (numberp (cadr statement))
			 (int-to-string (cadr statement))
		       "?")
		     (caddr statement)
		     (if (cdddr statement)
			 (format " %S" (cdddr statement))
		       "")
		     )))))

(defvar statement-cache-display-overlays nil
  "For debugging.")

(defun cancel-overlays ()
  (interactive)
  (mapcar 'delete-overlay statement-cache-display-overlays)
  (setq statement-cache-display-overlays nil))

;; (add-hook 'versor-pre-command-hook 'cancel-overlays)
;; (remove-hook 'versor-pre-command-hook 'cancel-overlays)
;; (add-hook 'versor-pre-command-hook 'statement-forget-cache)
;; (remove-hook 'versor-pre-command-hook 'statement-forget-cache)
;; (add-hook 'versor-post-command-hook 'statement-display-cache-as-overlays)
;; (remove-hook 'versor-post-command-hook 'statement-display-cache-as-overlays)

(defun statement-display-cache-as-overlays ()
  "Display the cached statement data. Meant for debugging."
  (interactive)
  (cancel-overlays)
  (dolist (statement statements-known)
    (let* ((start (car statement))
	   (end (cadr statement))
	   (overlay (make-overlay start (if end 
					    end
					  (1+ start))))
	   (details (cdddr statement))
	   (type (caddr statement))
	   (type-name (upcase (symbol-name type)))
	   )
      (overlay-put overlay 'before-string (format "[%s[" type-name))
      (overlay-put overlay 'after-string (format "]%s]" type-name))
      (overlay-put overlay 'face (cons 'background-color "cyan"))
      (push overlay statement-cache-display-overlays)
      (dolist (detail details)
	(let* ((detail-type (car detail))
	       (detail-start (cadr detail))
	       (detail-end (cddr detail))
	       (o2 (make-overlay detail-start detail-end))
	       (detail-type-name (upcase (symbol-name detail-type))))
	  (overlay-put o2 'before-string (format "[%s[" detail-type-name))
	  (overlay-put o2 'after-string (format "]%s]" detail-type-name))
	  (overlay-put o2 'face
		       (cons 'background-color
			     (cdr (assoc detail-type
					 '((head . "red")
					   (body . "green")
					   (tail . "blue"))))))
	  (push o2 statement-cache-display-overlays))))))

(provide 'statement-cache)

;;; end of statement-cache.el
