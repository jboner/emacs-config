;;;; versor-local.el -- select navigation dimensions per mode or per buffer
;;; Time-stamp: <2007-08-28 16:11:50 jcgs>
;; This file is NOT part of GNU Emacs.

;; Copyright (C) 2004, 2006, 2007  John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2004
;; Keywords: convenience
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
;; emacs-versor -- versatile cursors for GNUemacs

(require 'buffer-select-hooks)
(require 'cl)

;;;; our variables

;;; Code:
(defvar versor-this-buffer-meta-level nil
  "The remembered meta-level for this buffer.")

(defvar versor-this-buffer-level nil
  "The remembered level for this buffer.")

(make-variable-buffer-local 'versor-this-buffer-meta-level)
(make-variable-buffer-local 'versor-this-buffer-level)

;;;; entry points

(defun versor-mode-change-function (old-mode &optional even-if-per-buffer)
  "Select dimension if necessary, as a hook on changing modes.
Given OLD-MODE because of calling convention, but doesn't use it.
With optional second arg EVEN-IF-PER-BUFFER, set the dimensions
according to mode even if they are normally per-buffer -- this
is used in setting the dimensions from a `find-file' hook."
  (when (and versor-auto-change-for-modes
	     (or even-if-per-buffer
		 (not versor-per-buffer)))
    ;; now get the meta-level and level for the new major mode
    (condition-case evar
	(let* ((new-pair (assoc
			  (if (and versor-text-in-code
				   (versor-am-in-text))
			      (symbol-name major-mode)
			    major-mode)
			  versor-mode-current-levels))
	       (new-meta-level (cadr new-pair))
	       (new-level (cddr new-pair)))
	  (when (and new-pair
		     (numberp new-meta-level)
		     (numberp new-level))
	    (setq versor-meta-level new-meta-level
		  versor-level new-level
		  ;; indicate that we don't have a different
		  ;; per-buffer setting for this buffer
		  versor-this-buffer-meta-level new-meta-level
		  versor-this-buffer-level new-level)
	    (versor-set-status-display t)))
      (error nil))))

(defun versor-buffer-change-function (old-buffer)
  "Select dimension if necessary, as a hook on changing buffers.
Given OLD-BUFFER as a calling convention, but doesn't use it."
  (when versor-per-buffer
    (condition-case evar
	(progn
	  (save-excursion
	    (set-buffer old-buffer)
	    (setq versor-this-buffer-meta-level versor-meta-level
		  versor-this-buffer-level versor-level))
	  (when (numberp versor-this-buffer-meta-level)
	    (setq versor-meta-level versor-this-buffer-meta-level)
	    (versor-trim-meta-level))
	  (when (numberp versor-this-buffer-level)
	    (setq versor-level versor-this-buffer-level)
	    (versor-trim-level))
	  (versor-set-status-display nil nil nil t))
      (error (message "error %S in versor-buffer-change-function" evar)
	     nil))))

(defun versor-display-modal-levels (&optional label marked)
  "Display which levels are currently used for each mode.
Optional LABEL names the display buffer.
Optional MARKED gives a major mode to asterisk -- the current
one is used if none is specified."
  (interactive)
  (when (null marked) (setq marked (list major-mode)))
  (let* ((lengths (mapcar 'length
			  (mapcar (lambda (name)
				    (if (stringp name)
					name
				      (symbol-name name)))
				  (mapcar 'car
					  versor-mode-current-levels))))
	 (format-string (format "%%c %% %ds: %%s%%s:%%s%%s\n"
				(if (> (length lengths) 1)
				    (apply 'max lengths)
				  20))))
    (setq versor-mode-current-levels
	  (sort versor-mode-current-levels
		#'(lambda (a b)
		    (if (string= (car a) (car b))
			(stringp (car b))
		      (string< (car a) (car b))))))
    (with-output-to-temp-buffer (if label label "*Modal levels*")
      (dolist (level versor-mode-current-levels)
	(princ (format format-string
		       (if (member (car level) marked) ?* ? )
		       (car level)
		       (if (stringp (car level))
			   "\""
			 "<")
		       (aref (aref moves-moves (cadr level)) 0)
		       (first (aref (aref moves-moves (cadr level))
				    (cddr level)))
		       (if (stringp (car level))
			   "\""
			 ">"))))
      (princ (if versor-am-in-text-in-code
		 "\nIn text\n"
	       "In code\n"))
      (princ (format "post-command-hook=%S\n" post-command-hook))
      )))

(defun versor-popup-modal-levels (&optional label marked)
  "Briefly display the modal levels.  Mostly for debugging.
Optional LABEL names the display buffer.
Optional MARKED gives a major mode to asterisk -- the current
one is used if none is specified."
  (save-window-excursion
    (sit-for 2)
    (versor-display-modal-levels label marked)
    (sit-for 4)))

;;;; setup

(add-hook 'mode-selection-hook 'versor-mode-change-function)

(add-hook 'buffer-selection-hook 'versor-buffer-change-function)

;; give the buffer the right initial dimensions
(add-hook 'find-file-hook (lambda ()
			    (versor-mode-change-function nil t)))

(provide 'versor-local)

;;; versor-local.el ends here
