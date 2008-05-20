;;; versor-text-in-code.el -- versatile cursor handling of strings and comments
;;; Time-stamp: <2007-06-25 14:32:14 jcgs>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2006  John C. G. Sturdy
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

(require 'versor-names)
(require 'versor-dimensions)

(defvar versor-non-text-meta-level (versor-find-meta-level-by-name "cartesian")
  "The meta-level used when last not in a comment or string.")

(defvar versor-non-text-level (cdr (versor-find-level-by-double-name "cartesian" "chars"))
  "The level used when last not in a comment or string.")

(defvar versor-text-meta-level (versor-find-meta-level-by-name "text")
  "The meta-level used when last in a comment or string.")

(defvar versor-text-level (cdr (versor-find-level-by-double-name "text" "chars"))
  "The level used when last in a comment or string.")

(mapcar 'make-variable-buffer-local
	'(versor-am-in-text-in-code
	  versor-non-text-meta-level
	  versor-non-text-level
	  versor-text-meta-level
	  versor-text-level))

(defun versor-am-in-text ()
  "Return whether we are in text within code."
  (let* ((face-here (get-text-property (point) 'face))
	 (face-before (get-text-property (max (1- (point))
					      (point-min)) 'face))
	 (am-in-text (and (memq face-here versor-text-faces)
			  (memq face-before versor-text-faces))))
    am-in-text))

(defun versor-text-in-code-function ()
  "Detect whether we have landed in a comment or string, and set versor up accordingly.
This piggy-backs onto font-lock-mode.
Meant to go on post-command-hook."
  (when versor-text-in-code
    (condition-case error-var
	(let* ((am-in-text (versor-am-in-text)))
	  ;; has it changed?
	  (unless (eq am-in-text versor-am-in-text-in-code)
	    (setq versor-am-in-text-in-code am-in-text)
	    (if am-in-text
		(progn
		  (setq versor-non-text-meta-level versor-meta-level
			versor-non-text-level versor-level
			versor-meta-level versor-text-meta-level
			versor-level versor-text-level)
		  (versor-set-status-display t)
		  (when versor-announce-text-in-code
		    (message "Arrived in comment or string, using %s:%s"
			     (versor-meta-level-name versor-meta-level)
			     (versor-level-name versor-level))))
	      (progn
		(setq versor-text-meta-level versor-meta-level
		      versor-text-level versor-level
		      versor-meta-level versor-non-text-meta-level
		      versor-level versor-non-text-level)
		(versor-set-status-display t)
		(when versor-announce-text-in-code
		  (message "Left comment or string, returning to %s:%s"
			   (versor-meta-level-name versor-meta-level)
			   (versor-level-name versor-level)))))))
      (error (message "Error in versor-text-in-code")))))

(add-hook 'post-command-hook 'versor-text-in-code-function)

(provide 'versor-text-in-code)

;;; end of versor-text-in-code.el
