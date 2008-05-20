;;;; versor-dwim.el -- move between code, comments, and strings, etc
;;; Time-stamp: <2007-03-18 18:45:20 jcgs>

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

(defun versor-type-of-place (&optional place)
  "Return what type of place PLACE (default is point) is."
  (let* ((bod (save-excursion (if place (goto-char place))
			      (beginning-of-defun)
			      (point)))
	 (partial-parse (save-excursion (parse-partial-sexp bod (or place (point)))))
	 (depth (nth 0 partial-parse))
	 (in-string (nth 3 partial-parse))
	 (in-comment (nth 4 partial-parse))
	 (string-or-comment-start (nth 8 partial-parse)))
    (if (zerop depth)
	(if in-comment
	    (cons 'top-level-comment nil)
	  (cons 'top-level nil))
      (if in-string
	  (cons 'string string-or-comment-start)
	(if in-comment
	    (cons (if (numberp in-comment)
		      'nested-comment
		    (if (save-excursion (if place (goto-char place))
					(beginning-of-line)
					(re-search-forward "\\S-" string-or-comment-start t))
			'trailing-comment
		      'line-comment))
		  string-or-comment-start)
	  (cons 'code nil))))))

(defun versor-dwim-textually (&optional tried)
  "DWIM function for modes with words, phrases etc.
Returns a description of what it has done.
The optional argument says which functions have already been tried."
  (cond
   ((looking-at "\\<")
    (versor-end-of-word 1)
    "Moved from start of word to end of word")
   ((looking-at "\\>")
    (backward-word 1)
    "Moved from end of word to start of word")
   ((not (memq 'versor-dwim-lispishly tried))
    (message "versor-dwim-textually handing over to versor-dwim-lispishly")
    (versor-dwim-lispishly (cons 'versor-dwim-textually tried)))
   (t
    "versor-dwim-textually could not think of anything to do here")))

(defun versor-dwim-lispishly (&optional tried)
  "DWIM function for modes with brackets etc.
Returns a description of what it has done.
The optional argument says which functions have already been tried."
  (let* ((loc (versor-type-of-place))
	 (loc-type (car loc))
	 (loc-details (cdr loc)))
    (cond
     ((eq loc-type 'nested-comment)
      (goto-char loc-details)
      "Went out of nested comment")
     ((eq loc-type 'trailing-comment)
      (goto-char loc-details)	 ; and here is a comment to test it on
      (backward-sexp 1)
      "Went from trailing comment to the thing it trails")
     ((eq loc-type 'line-comment)
      ;; condition only!
      ;; no body to this "while" loop
      ;; while I'm at it, might as well make big multi-line
      ;; comment to test this code, which should run to the
      ;; thing just after the comment,
      ;; if called anywhere within it
      (while (progn
	       (forward-line 1)
	       (looking-at "^\\s-*\\s<")))
      (skip-to-actual-code)
      "Went from whole-line comment to the code it precedes")
     ((eq loc-type 'string)
      (goto-char loc-details)
      (forward-sexp 1)
      "Went out of string")
     ((eq loc-type 'code)
      (cond
       ;; if before multiple spaces, go into them
       ((looking-at "\\s-\\s-")
	(forward-char 1))
       ;; try going into the next string or comment
       (t (let ((nearest-string (save-excursion
				  (re-search-forward "\\s\"" (point-max) t)
				  (point)))
		(nearest-comment (save-excursion
				   (re-search-forward "\\s<" (point-max) t))))
	    (if (numberp nearest-string)
		(if (numberp nearest-comment)
		    (progn
		      (goto-char (min nearest-string nearest-comment))
		      "Went into nearest string or comment")
		  (goto-char nearest-string)
		  "Went into nearest string")
	      (if (numberp nearest-comment)
		  (progn
		    (goto-char nearest-comment)
		    "Went into nearest comment")
		;; else don't really know where to go, anyone got any good ideas?
		"Couldn't think of anywhere sensible to go")))))
      )
     ((eq loc-type 'top-level-comment)
      ;; not sure what to do with this, try going into the nearest function
      (versor-down-list)
      "In top level comment, tried entering top-level form")
     ((eq loc-type 'top-level)
      (re-search-backward "^\\s<" (point-min) t)
      "At top level, tried finding top-level form"))))

(defvar versor-dwim-successive-commands 0
  "The number of consecutive commands which are versor-dwim.")

(defvar versor-dwim-starting-items nil
  "The selection with which versor-dwim was called.")

(defvar versor-dwim-starting-position nil
  "The buffer offset at which versor-dwim was called.")

(defun versor-dwim ()
  "Move to another sort of place that the user has in mind,
switching as necesssary between comment, code, string literal, etc.
This does many kind of movement that cannot easily be done using
the more regular versor commands.
This function reads the user's mind, using the following algorithm:
  Tell the user what has been done each time
  Eventually, the user's mind will come to expect the behaviour of this function
  Reading the user's mind will then be trivial.
Aspects of mental state not necessary for figuring out where to leave point are
factored out of the calculations."
  (interactive)
  (versor-as-motion-command current-item
    (if (eq last-command 'versor-dwim)
	(setq versor-dwim-successive-commands
	      (1+ versor-dwim-successive-commands))
      (setq versor-dwim-successive-commands 1
	    versor-dwim-starting-items (versor-get-current-items)
	    versor-dwim-starting-position (point)))
    ;; (message "dwim count %d, items %S" versor-dwim-successive-commands versor-dwim-starting-items)
    (if (<= versor-dwim-successive-commands versor-dwim-other-end-count)
	(versor-other-end-of-item)
      (let ((dwim-function (or (versor-get-action 'dwim)
			       (get major-mode 'versor-dwim))))
	(goto-char versor-dwim-starting-position)
	(message
	 (if dwim-function
	     (funcall dwim-function)
	   "No dwim function defined for dimension %s:%s" versor-current-meta-level-name versor-current-level-name))))))

(defconst open-bracket (string-to-char "(")
  "Get this out-of-line to avoid confusing indenter when editing functions that use it.")

(defconst close-bracket (string-to-char ")")
  "Get this out-of-line to avoid confusing indenter when editing functions that use it.")

(defun versor-set-dwim-for-modes (dwim-function &rest modes)
  "Set DWIM-FUNCTION to be the dwim-function for the modes given as the remaining args."
  (mapcar (lambda (mode)
	    (put mode 'versor-dwim dwim-function))
	  modes))

(versor-set-dwim-for-modes 'versor-dwim-lispishly
			   'emacs-lisp-mode
			   'lisp-mode
			   'lisp-interaction-mode
			   'c-mode
			   'java-mode
			   'perl-mode)

(versor-set-dwim-for-modes 'versor-dwim-textually
			   'text-mode
			   'tex-mode
			   'latex-mode
			   'texinfo-mode
			   'mail-mode)

(provide 'versor-dwim)

;;; end of versor-dwim.el
