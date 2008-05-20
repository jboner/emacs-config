;;;; versor-help.el -- help for versor
;;; Time-stamp: <2007-07-19 12:15:11 jcgs>

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

(defadvice describe-key (after versor (key untranslated up-event) activate)
  "Further help for versor functions."
  (let ((pre-versor-def (lookup-key versor-original-bindings-map key)))
    (when (and pre-versor-def
	       (symbolp pre-versor-def))
      (with-current-buffer "*Help*"
	(goto-char (point-max))
	(let ((standard-output (current-buffer))
	      (buffer-read-only nil))
	  (princ "\n\nThis key has been redefined by Versor.\n")
	  (princ  "\nWhen versor-mode is disabled:\n  It runs the command ")
	  (describe-sub-function pre-versor-def)
	  )))))

(defun describe-sub-function (base)
  "Helper function for describing commands that Versor commands build on."
  (let ((base-doc (if base (documentation base) nil))
	(base-file-name (symbol-file base)))
    (princ (format "`%s'" (symbol-name base)))
    (save-excursion
      (re-search-backward "`\\([^`']+\\)'" nil t)
      (help-xref-button
       1
       (if (< emacs-major-version 22)
	   #'(lambda (fun file)
	       (require 'find-func)
	       ;; Don't use find-function-noselect because it follows
	       ;; aliases (which fails for built-in functions).
	       (let* ((location (find-function-search-for-symbol
				 fun nil file)))
		 (pop-to-buffer (car location))
		 (goto-char (cdr location))))
	 'help-function)
       (list base base-file-name)
       "mouse-2, RET: find function's definition"))

    (if (null base-doc)
	(princ ".")
      (princ ", which is described as follows:\n\n  ")
      (princ (mapconcat 'identity
			(split-string base-doc "\n")
			"\n    ")))))

(defadvice describe-function-1 (after versor (function
					      ;; emacs23 refuses these two:
					      &optional parens interactive-p
					      ) activate)
  "Further help for versor functions."
  (when (versor-command-p function)
    (with-current-buffer "*Help*"
      (princ "\n\nThis function is part of versor.")
      (let* ((offset (if (memq function '(versor-over-start
					  versor-over-prev
					  versor-over-next
					  versor-over-end))
			 1
		       nil))
	     (base (versor-get-action
		    (cdr (assoc function
				'((versor-start . first)
				  (versor-next . next)
				  (versor-prev . previous)
				  (versor-end . last)
				  (versor-over-start . first)
				  (versor-over-next . next)
				  (versor-over-prev . previous)
				  (versor-over-end . last)
				  (versor-kill . delete))))
		    offset)))
	(if (memq function '(versor-insert-before
			     versor-insert-around
			     versor-replace
			     versor-insert-after))
	    (let* ((kinds (setq versor-insertion-kind-alist
				(sort versor-insertion-kind-alist
				      (function
				       (lambda (a b)
					 (let ((ca (car a))
					       (cb (car b)))
					   (if (symbolp ca)
					       (if (symbolp cb)
						   (string< (symbol-name ca) (symbol-name cb))
						 nil)
					     (if (symbolp cb)
						 t
					       (< ca cb))))))))))
	      (princ "\n\nThis can be followed by any of the following ways of getting an\n")
	      (princ "insertable string or strings (where N is 2 if inserting around the\n")
	      (princ "selection, else 1):")
	      (while kinds
		(let ((kd (documentation (cdar kinds))))
		  (princ (format "\n  % 12s: " (key-description (vector (caar kinds)))))
		  (princ (if kd (car (split-string kd "\n")) "not documented")))
		(setq kinds (cdr kinds))))
	  (when base
	    (princ (format "\n\nWhen versor-mode is enabled:\n  In the current versor dimension, which is <%s:%s>,"
			   versor-current-meta-level-name versor-current-level-name))
	    (princ "\n  it is implemented by ")
	    (describe-sub-function base)))))))

(provide 'versor-help)

;;; end of versor-help.el
