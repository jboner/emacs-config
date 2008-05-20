;;; versor-status.el -- versatile cursor
;;; Time-stamp: <2007-08-27 17:18:38 jcgs>
;;
;; emacs-versor -- versatile cursors for GNUemacs
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

(require 'versor-dimensions)

;; Display the current meta-dimension and dimension etc

(defun versor-gather-level-attributes (level attributes)
  "For LEVEL, gather the ATTRIBUTES.
The result is in the form of a property list."
  (let ((result nil))
    (while attributes
      (let* ((attr (car attributes))
	     (value (versor-action level attr)))
	(when value
	  (setq result
		(cons attr
		      (cons value
			    result)))))
      (setq attributes (cdr attributes)))
    result))

(defun versor-set-status-display (&optional one of-these explicit no-message)
  "Indicate the state of the versor system.
Optional args: you can mark ONE OF-THESE; if OF-THESE is not given, just
show ONE.

EXPLICIT means pop up a multi-line display even if this is not normally done.

NO-MESSAGE means work quietly, for example when we are changing
the cursor and selection colour because of having switched
buffer."
  ;; get the level names, and colour them in if told to
  (setq versor-current-over-level-name
	(if versor-color-dimension-indicators
	    (propertize (versor-level-name
			 (min (1+ versor-level)
			      (1- (length (versor-current-meta-level)))))
			'face
			(cons 'foreground-color
			      (cdr (assoc 'color (versor-current-level 1)))))
	  (versor-level-name
	   (min (1+ versor-level)
		(1- (length (versor-current-meta-level))))))
	versor-current-level-name
	(if versor-color-dimension-indicators
	    (propertize (versor-level-name versor-level)
			'face
			(cons 'foreground-color
			      (cdr (assoc 'color (versor-current-level)))))
	  (versor-level-name versor-level))
	versor-current-meta-level-name
	(versor-meta-level-name versor-meta-level))
  (when (and (eq major-mode 'emacs-lisp-mode)
	     (eq versor-level 1)
	     (eq versor-meta-level 1))
    (message "Warning! wrong dimensions for emacs-lisp-mode: %d:%d" versor-level versor-meta-level)
    (backtrace))
  ;; pop up a message (possibly multi-line)
  (if (and versor-multi-line-level-display explicit)
      (versor-display-current-dimensions)
    (unless no-message
      (if one
	  (if of-these
	      (versor-display-highlighted-choice one of-these)
	    (if (stringp one)
		(message one)))
	(message (first (versor-current-level))))))
  ;; set up the mode line indicators
  (if versor-reversible
      (setq versor-mode-line-begin-string (if versor-reversed " <==" " <")
	    versor-mode-line-end-string (if versor-reversed ">" "==>"))
    (setq versor-mode-line-begin-string " <"
	  versor-mode-line-end-string ">"))
  (force-mode-line-update t)
  ;; set the cursor colour
  (when versor-change-cursor-color
    ;; done: allow for a second choice of colour, to be used if the main one is the same as the background colour
    (let ((color (versor-action (versor-current-level) 'color)))
      (when (and (eq color (frame-parameter nil 'foreground-color))
		 (versor-action (versor-current-level) 'other-color))
	(setq color (versor-action (versor-current-level) 'other-color)))
      (set-cursor-color color)))
  ;; set the value for the face attribute to be used to indicate the
  ;; selection; we normally use background-color, but others can be
  ;; specified by the user
  (when (and window-system
	     versor-item-attribute
	     (fboundp 'set-face-attribute))
    (cond
     ((symbolp versor-item-attribute)
      (let ((attr-value (versor-action (versor-current-level)
				       versor-item-attribute)))
	(when attr-value
	  (set-face-attribute 'versor-item-face nil
			      versor-item-attribute
			      attr-value))))
     ((consp versor-item-attribute)
      (apply 'set-face-attribute 'versor-item-face nil
	     (versor-gather-level-attributes (versor-current-level)
					     versor-item-attribute)))))
  ;; confirmation by voice for those who want it
  (versor-speak "Moving by %s %s%s%s"
		versor-current-meta-level-name
		versor-current-level-name
		(if (and versor-text-in-code
			 versor-am-in-text-in-code)
		    " in embedded strings"
		  "")
		(if versor-auto-change-for-modes
		    (format " in %s mode" mode-name)
		  ""))
  ;; remember what dimension we were last in, mode-by-mode
  ;;  (require 'versor-local) (versor-popup-modal-levels "Just set levels")
  (let* ((mode (if (and versor-text-in-code
			versor-am-in-text-in-code)
		   (symbol-name major-mode)
		 major-mode))
	 (old-pair (assoc mode versor-mode-current-levels)))
    (if (null old-pair)
	(push (cons mode (cons versor-meta-level versor-level))
	      versor-mode-current-levels)
      (rplaca (cdr old-pair) versor-meta-level)
      (rplacd (cdr old-pair) versor-level))))

(defun versor-highlighted-string (string)
  "Return a highlighted version of STRING."
  (if versor-use-face-attributes
      (let ((strong
	     (if versor-highlight-with-brackets
		 (format "[%s]" string)
	       (copy-sequence string))))
	(put-text-property 0 (length strong)
			   'face 'versor-item-face
			   strong)
	strong)
    (if versor-highlight-with-brackets
	(format "[%s]" string)
      string)))

(defun versor-unhighlighted-string (string)
  "Return an unhighlighted version of STRING."
  (if versor-highlight-with-brackets
      string
    ;; allow space where the brackets would go in the highlighted version
    (format " %s " string)))

(defun versor-display-highlighted-choice (one of-these-choices)
  "Display, with ONE highlighted, the members of OF-THESE-CHOICES"
  (let* ((msg (mapconcat
	       (lambda (string)
		 (if (string= string one)
		     (versor-highlighted-string string)
		   (versor-unhighlighted-string string)))
	       of-these-choices
	       ", ")))
    (message msg)
    (sit-for versor-display-highlighted-choice-time)))

(defvar versor-max-meta-name-length nil
  "The length of the longest meta-level name.
Used for display purposes, and cached here.")

(defun versor-display-current-dimensions ()
  "Indicate the current meta-level and level, in a multi-line message.
The message goes away as soon as you enter any input."
  (interactive)
  (unless versor-max-meta-name-length
    (setq versor-max-meta-name-length
	  (apply 'max
		 (mapcar 'length
			 (mapcar 'car
				 (versor-meta-level-names))))))
  (message
   (let ((meta-levels-name-format-string (format "%% %ds" versor-max-meta-name-length)))
     (mapconcat
      'identity
      (let ((meta (1- (length moves-moves)))
	    (formats (reverse (versor-all-names-grid-formats)))
	    (result nil))
	(while (>= meta 1)
	  (let* ((meta-data (aref moves-moves meta))
		 (meta-name (aref meta-data 0))
		 (inner-result nil)
		 (row-formats formats)
		 (level 1)
		 (n-level (length meta-data)))
	    (when (or (eq meta-name
			  (aref (aref moves-moves versor-meta-level) 0))
		      (versor-meta-dimension-valid-for-mode meta-name major-mode))
	      (while row-formats
		(let* ((level-name-raw 
			(if (< level n-level)
			    (first (aref meta-data level))
			  ""))
		       (level-name (format (car row-formats) level-name-raw)))
		  (push
		   (if (and (= meta versor-meta-level)
			    (= level versor-level))
		       (versor-highlighted-string level-name)
		     level-name)
		   inner-result)
		  (setq row-formats (cdr row-formats))
		  (incf level)))
	      (push
	       (concat
		(format meta-levels-name-format-string
			(if (= meta versor-meta-level)
			    (versor-highlighted-string meta-name)
			  meta-name))
		": "
		(mapconcat 'identity inner-result " "))
	       result))
	    (decf meta)))
	result)
      "\n"))))

(provide 'versor-status)

;;;; end of versor-status.el
