;;;; nested-blocks.el
;;; Time-stamp: <2006-11-04 20:45:24 jcgs>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2005, 2006  John C. G. Sturdy
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

;; very pleasing -- lots of this worked first time
;; less pleasing -- the rest was really fiddly



;; We define each part of the syntax for each major mode by an alist
;; of major mode name to regular expression. An alternative would be
;; to make these buffer-local variables, and set them up in a hook for
;; each major mode. I think that would be clumsier than this, and
;; harder to find the necessary data if you want to make any changes.

(defvar nested-blocks-mode-starts
  ;; todo: update to include TeX family and sh
  '((latex-mode . "\\(\\\\begin{\\([a-z]+\\)}\\)\\|{")
    (html-mode . "\\(<\\([^/>][^>]*\\)>\\)\\|\\((\\)\\|\\(``\\)")
    (html-helper-mode . "\\(<\\([^!/>][^>]*\\)>\\)\\|\\([[({]\\)\\|\\(``\\)")
    (sgml-mode . "\\(<\\([^/>][^>]*\\)>\\)\\|\\((\\)\\|\\(``\\)")
    (c-mode . "[{(]")
    (java-mode . "[{(]")
    (perl-mode . "[{(]")
    (bcpl-mode . "\\$(")
    (t . "[[{(]"))
  "Alist showing how nested blocks start in each mode")

(defvar nested-blocks-mode-ends
  ;; todo: update to include TeX family and sh
  '((latex-mode . "\\(\\\\end{\\([a-z]+\\)}\\)\\|}")
    (html-mode . "\\(</\\([^>]+\\)>\\)\\|\\()\\)\\|\\(''\\)")
    (html-helper-mode . "\\(</\\([^>]+\\)>\\)\\|\\([])}]\\)\\|\\(''\\)")
    (sgml-mode . "\\(</\\([^>]+\\)>\\)\\|\\()\\)\\|\\(''\\)")
    (c-mode . "[})]")
    (java-mode . "[})]")
    (perl-mode . "[})]")
    (bcpl-mode . "\\$)")
    (t . "[]})]"))
  "Alist showing how nested blocks finish in each mode")

(defvar nested-blocks-mode-comment-starts
  '((html-helper-mode . "<!--"))
  "Alist showing how comments start in each mode.")

(defvar nested-blocks-mode-comment-ends
  '((html-helper-mode . "-->"))
 "Alist showing how comments end in each mode.")

(defvar nested-blocks-mode-ignorables
  ;; todo: update to include TeX family and sh
  '((html-mode . "</?\\(\\(br\\)\\|\\(img\\)\\(meta\\)\\)")
    (html-helper-mode . "</?\\(\\(li\\)\\|\\(d[dt]\\)\\|\\([hb]r\\)\\|\\(img\\)\\|\\(meta\\)\\|\\(![a-z]\\)\\)")
    (sgml-mode . "</?\\(\\(li\\)\\|\\(d[dt]\\)\\|\\(br\\)\\|\\(img\\)\\)")
    (t .  ";"))
  "Alist showing things that look like nested block structure but are not.")

(defvar nested-blocks-mode-list-items
  '((html-mode . "<\\(\\(li\\)\\|\\(d[dt]\\)\\)[^>]*>")
    (html-helper-mode . "<\\(\\(li\\)\\|\\(d[dt]\\)\\)[^>]*>")
    (latex-mode . "\\\\\\(item\\|chapter\\|\\(sub\\)*section\\)"))
  "Alist showing list item separators for each mode.
This is for any markup syntax that does not have a beginning and an end, but occurs in a sequence of the same type.")

(defvar nested-blocks-mode-unitary-items
  '((html-mode . "<[^/>]+/>")
    (html-helper-mode . "<[^/>]+/>")
    ;; (latex-mode . "")
    )
  "Alist showing list item separators for each mode.
This is for markup syntax that is a complete block in its own right.
These regexps may be quite general, and it's safe to make them catchalls that
will accidentally catch the other forms of syntax, as they're tested for last.")

(defvar nested-blocks-mode-any
  ;; todo: update to include TeX family and sh
  '((latex-mode . "\\\\\\(\\(\\(begin\\|end\\){\\([a-z]+\\)}\\)\\|item\\|chapter\\|\\(sub\\)*section\\)")
    (c-mode . "[({})]")
    (html-mode . "</?\\([^>]+\\)>")
    (html-helper-mode . "\\(<!--\\)\\|\\(</?\\([^>]+\\)>\\)\\|\\(-->\\)")
    (sgml-mode . "</?\\([^>]+\\)>")
    (bcpl-mode . "\\$[()]")
    (t . "[][}{)(]"))
  "Alist showing how nested blocks start or end in each mode.
This should match anything that either of the above match.
It's a bit clumsy for the programmer to have to declare it explicitly
but the performance should be a lot better than combining them
automatically, given the nature, for example, of HTML blocks.")

;; Now some functions to access the elements of the alists above:

(defun nested-blocks-start ()
  "For this major mode, look up the starter for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-starts))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-starts)))))

(defun nested-blocks-end ()
  "For this major mode, look up the ender for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-ends))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-ends)))))

(defun nested-blocks-comment-start ()
  "For this major mode, look up the starter for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-comment-starts))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-comment-starts)))))

(defun nested-blocks-comment-end ()
  "For this major mode, look up the ender for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-comment-ends))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-comment-ends)))))

(defun nested-blocks-ignore ()
  "For this major mode, look up the \"ignorables\" regexp for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-ignorables))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-ignorables)))))

(defun nested-blocks-list-item ()
  "For this major mode, look up the list-item regexp for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-list-items))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-list-items)))))

(defun nested-blocks-any ()
  "For this major mode, look up the start-or-ender-or-ignorable regexp for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-any))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-any)))))

;;; Movement commands

(defun nested-blocks-enter ()
  "Into the next nested block forwards."
  (interactive)
  (nested-blocks-forward 1 1))

(defun nested-blocks-leave ()
  "Move forwards out of current nested block."
  (interactive)
  (nested-blocks-forward 1 -1))

(defun nested-blocks-leave-backwards ()
  "Move backwards out of current nested block."
  (interactive)
  (nested-blocks-backward 1 -1))

(defvar nested-blocks-number-colours (vector "red" "pink" "orange" "yellow" "green"
			       "cyan" "sky blue" "deep sky blue" "dodger blue"
			       "purple" "white" "grey" "brown")
  "For debugging nested-blocks.")

(defvar nested-blocks-debug nil
  "*Whether to output messages while moving around nested blocks.")

(defvar nested-blocks-debug-show-structure nil
  "*Whether to fill in colours to show the block structure of the text.")

(defun nested-blocks-forward (&optional n d)
  "Move forward over a nested block. Like forward-sexp but more general.
Optional argument says how many blocks to move over.
Second argument says what level to stop at."
  ;; ought to make this handle things in comments and strings
  (interactive "p")
  (skip-to-actual-code)
  (let ((starting-point (point))
	(start (nested-blocks-start))
	(end (nested-blocks-end))
	(comment-start (nested-blocks-comment-start))
	(comment-end (nested-blocks-comment-end))
	(list-item (nested-blocks-list-item))
	(any (nested-blocks-any))
	(ignore (nested-blocks-ignore))
	(depth 0)
	(comment-depth 0))
    (unless (numberp n) (setq n 1))
    (unless (numberp d) (setq d 0))
    (when nested-blocks-debug
      (message "trying to classify tags, using any=\"%s\" start=\"%s\" end=\"%s\" list-item=\"%s\" ignore=\"%s\""
	       any start end list-item ignore))
    (while (> n 0)
      (let ((on-list (save-match-data
		       (if (looking-at list-item)
			   (match-string-no-properties 1)
			 nil))))
	(message "on-list=%S, match-end=%S" on-list (match-end 0))
	(when on-list
	  (goto-char (match-end 0)))
	(goto-char
	 ;; todo: get this working when already past the last thing it could usefully handle
	 (catch 'found
	   (while (re-search-forward any (point-max) t)
	     (save-match-data
	       (let ((starting (match-beginning 0))
		     (ending (match-end 0))
		     (old-depth depth))
		 (when nested-blocks-debug
		   (message "At %d:\"%s\"" (point) (buffer-substring-no-properties starting ending)))
		 (save-excursion
		   (message "starting=%S" starting)
		   (goto-char starting)
		   (cond
		    ((and comment-start (looking-at comment-start))
		     (setq comment-depth (1+ comment-depth))
		     (when nested-blocks-debug
		       (message "deeper comment: %d" comment-depth)))
		    ((and comment-end (looking-at comment-end))
		     (setq comment-depth (1- comment-depth))
		     (when nested-blocks-debug
		       (message "shallower comment: %d" comment-depth)))
		    ((and list-item (looking-at list-item))
		     (when nested-blocks-debug
		       (message "list item at %d (%s)" depth (if on-list "working on list" "not on list"))))
		    ((and ignore (looking-at ignore))
		     (when nested-blocks-debug
		       (message "ignoring: %d" depth))
		     t)
		    ((looking-at start)
		     (when (zerop comment-depth)
		       (setq depth (1+ depth)))
		     (when nested-blocks-debug
		       (message "deeper: %d%s" depth (if (zerop comment-depth) "" " but in comment"))))
		    ((looking-at end)
		     (when (zerop comment-depth)
		       (setq depth (1- depth)))
		     (when nested-blocks-debug
		       (message "shallower: %d%s" depth (if (zerop comment-depth) "" " but in comment"))))
		    (t (error
			"inconsistency in nested-block patterns: \"%s\" matched \"any\" but nothing specific"
			(buffer-substring-no-properties starting ending))))
		   (when nested-blocks-debug-show-structure
		     (put-text-property (if (< depth old-depth) ending starting) (point-max)
					'face (cons 'background-color
						    (aref nested-blocks-number-colours depth))))
		   (if on-list
		       (when (or (< depth d)
				 (and (looking-at list-item)
				      (string= (match-string-no-properties 1) on-list)
				      (= depth d)))
			 (throw 'found starting))
		     (when (= depth d) 
		       (throw 'found ending))))))
	     nil))))
      (setq n (1- n)))
    (when nested-blocks-debug (message "nested-blocks-forward selecting %d..%d" starting-point (point)))
    (versor-set-current-item starting-point (point))))

(defun nested-blocks-backward (&optional n d)
  "Move back over a nested block. Like backward-sexp but more general.
Optional argument says how many blocks to move over.
Second argument says what level to stop at."
  ;; ought to make this handle things in comments and strings
  (interactive "p")
  (let ((start (nested-blocks-start))
	(end (nested-blocks-end))
	(list-item (nested-blocks-list-item))
	(comment-start (nested-blocks-comment-start))
	(comment-end (nested-blocks-comment-end))
	(any (nested-blocks-any))
	(ignore (nested-blocks-ignore))
	(depth 0)
	(comment-depth 0)
	)
    (unless (numberp n) (setq n 1))
    (unless (numberp d) (setq d 0))
    (while (> n 0)
      (save-match-data
	(catch 'found
	  (while (re-search-backward any (point-min) t)
	    (let ((starting (match-beginning 0))
		  (ending (match-end 0)))
	      (when nested-blocks-debug
		(message "At %d:\"%s\"" (point) (buffer-substring-no-properties starting ending)))
	      (save-excursion
		(cond
		 ((and comment-end (looking-at comment-end))
		  (setq comment-depth (1+ comment-depth))
		  (when nested-blocks-debug
		    (message "deeper comment: %d" comment-depth)))
		 ((and comment-start (looking-at comment-start))
		  (setq comment-depth (1- comment-depth))
		  (when nested-blocks-debug
		    (message "shallower comment: %d" comment-depth)))
		 ((and list-item (looking-at list-item))
		  (when nested-blocks-debug
		    (message "list item at %d" depth)))
		 ((and ignore (looking-at ignore))
		  (when nested-blocks-debug
		    (message "ignoring: %d" depth))
		  t)
		 ((looking-at start)
		  (when (zerop comment-depth)
		    (setq depth (1- depth)))
		  (when nested-blocks-debug
		    (message "shallower: %d%s" depth (if (zerop comment-depth) "" " but in comment"))))
		 ((looking-at end)
		  (when (zerop comment-depth)
		    (setq depth (1+ depth)))
		  (when nested-blocks-debug
		    (message "deeper: %d%s" depth (if (zerop comment-depth) "" " but in comment"))))
		 (t (error
		     "inconsistency in nested-block patterns: \"%s\" matched \"any\" but nothing specific"
		     (buffer-substring-no-properties starting ending))))
		(if (= depth d)
		    (throw 'found (point))))))))
      (setq n (1- n)))))

;;; Use of blocks as templates

(defun nested-blocks-template (start end)
  "Make a nested block template from the buffer between START and END.
This is a list of strings."
  ;; todo: have the option of preserving the kind of white space around each template element, such as whether it was at the start of the line; see yank-whitespace.el for code for this
  (save-excursion
    (goto-char start)
    (let ((any (nested-blocks-any))
	  (template nil))
      (while (re-search-forward any end t)
	(push (buffer-substring-no-properties (match-beginning 0) (match-end 0))
	      template))
      (nreverse template))))

(defun nested-blocks-another ()
  "Use the nested block behind point as a template to insert a new one."
  (interactive)
  (let ((template (save-excursion
		    (nested-blocks-backward)
		    (let ((start (point)))
		      (nested-blocks-forward)
		      (nested-blocks-template start (point))))))
    (apply 'insert template)))

(provide 'nested-blocks)

;; end of nested-blocks.el
