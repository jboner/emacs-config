;;; versor-base-moves.el -- versatile cursor
;;; Time-stamp: <2008-01-17 16:35:20 jcgs>
;;
;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2004
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2005, 2006, 2007, 2008  John C. G. Sturdy
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

;; Some internal functions for operations that aren't typically there already,
;; or that normal Emacs does slightly differently from what we want.

(require 'cl)
(require 'modal-functions)

;;; Code:

(defun versor-forward-paragraph (&optional n)
  "As `forward-paragraph' with versor wrapping, but don't end on a blank line.
Optional argument N is how many paragraphs to move forward."
  (interactive "p")
  (versor-as-motion-command current-item
    (if (and current-item
	     (versor-overlay-end current-item))
	(goto-char (versor-overlay-end current-item))
      (forward-paragraph n))
    ;; (message "forward-paragraph %d; then forward over blank lines" n)
    (if (> n 0)
	(while (and (looking-at "^\\s-*\\(\\s<.*\\)?$")
		    (not (eobp)))
	  ;; (message "skipping blank line at start of paragraph")
	  (forward-line 1)))
    (skip-to-actual-code)
    (let ((start (point)))
      (versor-complete-paragraph)
      (versor-set-current-item start
			       (save-excursion
				 (end-of-paragraph-text)
				 (versor-adjust-end-of-paragraph 1))))))

(defmodel versor-complete-paragraph ()
  "Handle false incomplete paragraphs.
For example, a LaTeX \\begin{whatever} on a line of its own will
look like a paragraph in its own right, but in fact there is more
to come.")

(defmodal versor-complete-paragraph (fundamental-mode) ()
  "Does nothing.")

(defmodal versor-complete-paragraph (latex-mode) ()
  "Handles \\begin to stop it looking like a whole paragraph."
  (while (looking-at "\\\\begin{.+}")
    (beginning-of-line 2)))

(defun non-comment-between (a b)
  "Return whether there is any non-comment between A and B."
  (save-excursion
    (goto-char a)
    (while (and (< (point) b)
		(looking-at "^\\s-*\\(\\s<.*\\)?$"))
      (beginning-of-line 2))
    (< (point) b)))

(defun versor-backward-paragraph (&optional n)
  "As `backward-paragraph' with versor wrapping, but don't end on a blank line.
Optional argument N is how many paragraphs to go back."
  (interactive "p")
  ;; todo: handle start-of-paragraph markup
  (versor-as-motion-command current-item
    ;; first, move back into the separator before the current paragraph
    (backward-paragraph)
    ;; now, keep going back a real (non-comment) paragraph
    (let ((apparent-end (point))
	  (got-some-real-text nil))
      (while (> n 0)
	(backward-paragraph)
	;; if it's just a comment, or some skippable markup, go back further
	(while (and (not (bobp))
		    (or (setq got-some-real-text (versor-on-false-paragraph-end))
			(not (non-comment-between (point) apparent-end))))
	  (unless got-some-real-text
	    (setq apparent-end (point)))
	  (backward-paragraph))
	(setq n (1- n)
	      apparent-end (point)))
      (skip-to-actual-code)
      ;; todo: move to handle false beginnings
      (while (versor-on-false-paragraph-start)
	(beginning-of-line 0))
      (skip-to-actual-code)
      (versor-set-current-item (point)
			       (save-excursion
				 (versor-complete-paragraph)
				 (end-of-paragraph-text)
				 (versor-adjust-end-of-paragraph 1))))))

(defmodel versor-on-false-paragraph-end ()
  "Return whether we are on text that looks like a paragraph end, but isn't.")

(defmodal versor-on-false-paragraph-end (fundamental-mode) ()
  "Return whether we are on text that looks like a paragraph end, but isn't."
  nil)

(defmodal versor-on-false-paragraph-end (latex-mode) ()
  "Return whether we are on text that looks like a paragraph end, but isn't."
  (looking-at "\\\\end{.+}"))

(defmodel versor-on-false-paragraph-start ()
  "Return whether we are on text that looks like a paragraph start, but isn't.")

(defmodal versor-on-false-paragraph-start (fundamental-mode) ()
  "Return whether we are on text that looks like a paragraph start, but isn't."
  nil)

(defmodal versor-on-false-paragraph-start (latex-mode) ()
  "Return whether we are on text that looks like a paragraph start, but isn't."
  (looking-at "\\\\begin{.+}"))

(defmodel versor-adjust-end-of-paragraph (&optional arg)
  "Adjust to the end of the paragraph.")

(defmodal versor-adjust-end-of-paragraph (fundamental-mode) (&optional arg)
  "Adjust to the end of the paragraph."
  (point))

(defmodal versor-adjust-end-of-paragraph (latex-mode) (&optional arg)
  "Adjust to the end of the paragraph."
  (beginning-of-line 2)
  (while (versor-on-false-paragraph-end)
    (beginning-of-line 2))
  (point))

;; todo: add tex, latex, bibtex ideas of paragraphs, perhaps going via generic-text, or building on the built-in ones in-place?

;;;; characters

(defun versor-backward-char (n)
  "Like backward-char, but with a versor wrapper.
This wrapper is required to prevent versor's command framework from
adjusting point afterwards. It normally does this to keep at the start
of a sensible item, but this doesn't make sense for character
movements."
  (interactive "p")
  (backward-char n)
  (versor-set-current-item (point) (1+ (point))))

(defun versor-forward-char (n)
  "Like forward-char, but with a versor wrapper.
This wrapper is required to prevent versor's command framework from
adjusting point afterwards. It normally does this to keep at the start
of a sensible item, but this doesn't make sense for character
movements."
  (interactive "p")
  (forward-char n)
  (versor-set-current-item (point) (1+ (point))))

;;;; lines

(defun versor-start-of-line (n)
  "Move to first non-space on line, or to start of Nth line from here."
  (interactive "p")
  (if (or (= n 1)
	  (not (eq last-command 'versor-start-of-line)))
      (back-to-indentation)
    (beginning-of-line n)))

(defun move-beginning-of-this-line ()
  "Wrapper to call move-beginning-of-line without needing args."
  (move-beginning-of-line 1))

(defmacro versor-following-margins (&rest body)
  "Perform BODY while sticking to the same margin if on one."
  `(let* ((at-left-edge (bolp))
	  (at-right-margin (eolp))
	  (at-left-margin
	   (or				; don't calculate this if either of the others hold
	    at-right-margin		; as it is slower, and is not used if the others are
	    at-left-edge
	    (save-excursion (skip-syntax-backward "\\s-") (bolp)))))
     ,@body
     (cond
      (at-left-edge (beginning-of-line 1))
      (at-right-margin (end-of-line 1))
      (at-left-margin (back-to-indentation)))))

(defun versor-previous-line (n)
  "Move to the previous line, following margins if on one."
  (interactive "p")
  (versor-following-margins
   (versor-backward-without-sticking-at-comments
    previous-line n)))

(defun versor-next-line (n)
  "Move to the next line, following margins if on one."
  (interactive "p")
  (versor-following-margins (next-line n)))

;;;; sexps

(defun safe-up-list (&rest args)
  "Like up-list, but returns nil on error."
  (condition-case error-var
      (progn
	(apply 'up-list args)
	t)
    (error nil)))

(defun safe-backward-up-list (&rest args)
  "Like backward-up-list, but returns nil on error."
  (condition-case error-var
      (progn
	(apply 'backward-up-list args)
	t)
    (error nil)))

(defun safe-backward-sexp (&rest args)
  "Like backward-sexp, but returns nil on error."
  (condition-case error-var
      (progn
	(apply 'backward-sexp args)
	t)
    (error nil)))

(defun safe-down-list (&rest args)
  "Like down-list, but returns nil on error."
  (condition-case error-var
      (progn
	(apply 'down-list args)
	t)
    (error nil)))

(defun safe-scan-sexps (from count)
  "Like scan-sexps, but returns nil on error."
  (condition-case error-var
      (scan-sexps from count)
    (error nil)))

(defun safe-scan-lists (from count depth)
  "Like scan-lists, but returns nil on error."
  (condition-case
      error-var
      (scan-lists from count depth)
    (error nil)))

(defun safe-forward-sexp (n)
  "Like forward-sexp, but returns point on success and nil on error."
  (condition-case error-var
      (progn
	(forward-sexp n)
	(point))
    (error nil)))

;;;; sexps, extended to handle bracketting by larger things than
;;;; characters, e.g. markup tags

(defmodel versor-backward-up-list (arg)
  "Like backward-up-list, but with some versor stuff around it.
Makes a two-part selection, of opening and closing brackets."
  (interactive "p"))

(defun current-depth (where &optional msg)
  "Return the depth of nesting of WHERE."
  (interactive "d")
  (let ((depth 0))
    (condition-case evar
	(while where
	  (setq where (scan-lists where -1 1)
		depth (1+ depth)))
      (error (setq where nil)))
    (when (or msg (interactive-p))
      (message "depth %d" depth))
    depth))

(defmodal versor-backward-up-list (fundamental-mode) (arg)
  "Like backward-up-list, but with some versor stuff around it.
Makes a two-part selection, of opening and closing brackets."
  (interactive "p")
  (safe-backward-up-list arg)
  (when versor-reformat-automatically
    (save-excursion
      (condition-case evar
	  (let ((start (point)))
	    (forward-sexp 1)
	    (let ((end (point-marker)))
	      (indent-region start end nil)
	      (save-restriction
		(narrow-to-region start end)
		(delete-trailing-whitespace))))
	(error nil))))
  (when versor-show-depth
    (current-depth (point) t))
  (let ((start (point))
	(end (save-excursion (safe-forward-sexp 1) (point))))
    (versor-set-current-item start (1+ start))
    (when end
      (versor-add-to-current-item(1- end) end))
    (cons start end)))

(defmodal versor-backward-up-list (lisp-mode emacs-lisp-mode lisp-interaction-mode) (arg)
  "Like backward-up-list, but with some versor stuff around it.
Makes a two-part selection, of opening and closing brackets."
  (interactive "p")
  (safe-backward-up-list arg)
  (let ((start (point))
	(end (save-excursion (safe-forward-sexp 1) (point-marker))))
    (when (and versor-reformat-automatically
	       (buffer-modified-p))
      (condition-case evar
	  (progn
	    ;; todo: use (indent-region start end nil) instead
	    (indent-sexp)
	    (save-restriction
	      (narrow-to-region start end)
	      (delete-trailing-whitespace)))
	(error nil)))
    (when versor-show-depth
      (current-depth (point) t))
    (versor-set-current-item start (1+ start))
    (when end
      (versor-add-to-current-item (1- end) end))
    (prog1
	(cons start (marker-position end))
      (move-marker end nil))))

(defmodal versor-backward-up-list (html-mode html-helper-mode latex-mode tex-mode) (arg)
  "Like backward-up-list, but with versor stuff around it, and for HTML blocks."
  (nested-blocks-leave-backwards)
  (when (looking-at (nested-blocks-start))
    (let ((open-start (match-beginning 0))
	  (open-end (match-end 0)))
      (nested-blocks-forward)
      (when (re-search-backward (nested-blocks-end) open-end t)
	(let ((close-start (match-beginning 0))
	      (close-end (match-end 0)))
	  (make-versor-overlay open-start open-end)
	  (versor-extra-overlay close-start close-end)
	  (goto-char open-start))))))

(defmodal versor-backward-up-list (sgml-mode xml-mode) (arg)
  "Like backward-up-list, but with versor stuff around it, and for SGML/XML blocks."
  (while (> arg 0)
    (sgml-backward-up-element)
    (setq arg (1- arg)))
  (let* ((start-start (point))
	 (start-end (search-forward ">" (point-max) t))
	 (end-end (progn
		    (goto-char start-start)
		    (sgml-forward-element)
		    (point)))
	 (end-start (search-backward "<" (point-min) t)))
    (make-versor-overlay start-start start-end)
    (versor-extra-overlay end-start end-end)
    (goto-char start-start)))

 (defvar py-statement-tail-parts-regexp "^\\s-*\\(else:\\|elif\\)"
  "Regexp for things that show there is a tail to this python statement.")

(defun py-end-of-statement ()
  "Move to the end of the python statement starting at point."
  (interactive)
  (let ((this-indent (current-indentation)))
    (if (save-excursion
	  (forward-line 1)
	  (= (current-indentation) this-indent))
	(end-of-line)
      (py-mark-block nil t)
      (while (looking-at py-statement-tail-parts-regexp)
	(py-mark-block nil t)))))

(defmodal versor-backward-up-list (python-mode) (arg)
  "Like backward-up-list, but with versor stuff around it, and for python indentation.
This breaks the normal behaviour for versor-backward-up-list when there is no actual bracketry."
  (if (save-excursion
	(re-search-backward "[[({]" (line-beginning-position) t))
      (progn
	(safe-backward-up-list arg)
	(let ((start (point))
	      (end (save-excursion (forward-sexp 1) (point))))
	  (versor-set-current-item start (1+ start))
	  (versor-add-to-current-item (1- end) end)
	  (cons start end)))
    (py-goto-block-up)
    (let ((start (point)))
      (py-end-of-statement)
      (versor-set-current-item start (point)))))

(defmodel versor-down-list (arg)
  "Like down-list, but with some versor stuff around it."
  (interactive "p"))

(defmodal versor-down-list (fundamental-mode) (arg)
  "Like down-list, but with some versor stuff around it."
  (interactive "p")
  (unless (safe-down-list arg)
    ;; if we were at the end of the last expression, try going to the start of it
    (previous-sexp 1)
    (safe-down-list args))
  (if (looking-at "\\s(")
      (save-excursion
	(versor-set-current-item (point) (1+ (point)))
	(when (safe-forward-sexp 1)
	  (versor-add-to-current-item (1- (point)) (point))))
    (when versor-show-depth
      (current-depth (point) t))    (let ((start (point)))
      (when (safe-forward-sexp 1)
	(versor-set-current-item start (point))
	(goto-char start)))))

(defmodal versor-down-list (html-mode html-helper-mode latex-mode tex-mode) (arg)
  "Like down-list, but with versor stuff around it, and for HTML block structure."
  (cond
   ((at-paragraph-start-p (point))
    (forward-word))
   (t
    (nested-blocks-enter)
    (let ((start (point)))
      (next-texp 1)
      (versor-set-current-item start (point))
      (goto-char start)))))

(defmodal versor-down-list (sgml-mode xml-mode) (arg)
  "Like down-list, but with versor stuff around it, and for SGML/XML block structure."
  (while (> arg 0)
    (sgml-down-element)
    (setq arg (1- arg)))
  (versor-sgml-select-element))

 (defmodal versor-down-list (python-mode) (arg)
  "Like down-list, but with some versor stuff around it, and understanding python."
  (interactive "p")
  (if (save-excursion
	(re-search-forward "[[{(]" (line-end-position) t))
      (progn
	(unless (safe-down-list arg)
	  ;; if we were at the end of the last expression, try going to the start of it
	  (previous-sexp 1)
	  (safe-down-list args))
	(if (looking-at "\\s(")
	    (save-excursion
	      (versor-set-current-item (point) (1+ (point)))
	      (when (safe-forward-sexp 1)
		(versor-add-to-current-item (1- (point)) (point))))
	  (let ((start (point)))
	    (when (safe-forward-sexp 1)
	      (versor-set-current-item start (point))
	      (goto-char start)))))
    (let ((initial-indent (current-indentation)))
      (while (and (not (eobp))
		  (= initial-indent (current-indentation)))
	(forward-line))
      (while (and (not (bobp))
		  (< (current-indentation) initial-indent))
	(forward-line -1))
      (let ((start (point)))
	(py-end-of-statement)
	(versor-set-current-item start (point))))))

(defmodel first-sexp ()
  "Move back by sexps until you can go back no more."
  (interactive))

(defmodal first-sexp (fundamental-mode) ()
  "Move back by sexps until you can go back no more."
  (interactive)
  (let ((backto (safe-scan-sexps (point) -1)))
    (while backto
      (goto-char backto)
      (setq backto (safe-scan-sexps backto -1)))))

(defmodel last-sexp ()
  "Move forward by sexps until you can go no more."
  (interactive))

(defmodal last-sexp (fundamental-mode) ()
  "Move forward by sexps until you can go no more."
  (interactive)
  (let ((onto (safe-scan-sexps (point) 1))
	(prev (point))
	(prevprev (point)))
    (while onto
      (goto-char onto)
      (setq prevprev prev
	    prev onto
	    onto (safe-scan-sexps onto 1)))
    (parse-partial-sexp prevprev prev
			0 t)))

(defmodel next-sexp (n)
  "Move forward N sexps.
  Like forward-sexp but moves to the start of the next sexp rather than the
  end of the current one, and so skips leading whitespace etc.
  See versor-allow-move-to-end-of-last for some finer control."
  (interactive "p"))
    
(defmodal next-sexp (fundamental-mode) (n)
  "Move forward N sexps.
Like forward-sexp but moves to the start of the next sexp rather than the
end of the current one, and so skips leading whitespace etc.
See versor-allow-move-to-end-of-last for some finer control.
This is the default implementation (defined as fundamental-mode) and it
should be used for normal programming language modes, including the various
kinds of Lisp modes."
  (interactive "p")
  (let* ((where (safe-scan-sexps (point) n))
	 (one-more (safe-scan-sexps where 1)))
    ;; (message "where=%S one-more=%S" where one-more)
    (if where
	(progn
	  (goto-char where)
	  (let ((limit (save-excursion (last-sexp) (point))))
	    (when (> n 0)
	      (parse-partial-sexp where limit
				  0	; targetdepth
				  t	; stopbefore
				  )))
	  (when (and (not one-more) versor-allow-move-to-end-of-last)
	    ;; (message "not one-more; where=%d, point=%d" where (point)) ;
	    ;; This is the special case where we move to the end of
	    ;; the last regexp in a list. (The normal case is that
	    ;; we move to the start of a regexp, and let the surrounding
	    ;; macro versor-as-motion-command in versor-next call
	    ;; versor-set-current-item for us.
	    (versor-set-current-item (safe-scan-sexps (point) -1)
				     ;; where
				     (if (eq versor-allow-move-to-end-of-last t)
					 where
				       (progn
					 (goto-char where)
					 ;; (message "Looking forward from %d" where)
					 ;; (skip-to-actual-code)
					 (skip-syntax-forward "^)")
					 (point)
					 ))
				     )))
      (if versor-move-out-when-at-end
	  (progn
	    (safe-up-list)
	    (skip-to-actual-code))
	(message "No more sexps")))))

(defvar debug-texp nil
  "*Whether texp movement should explain what it's doing.")

(defun at-paragraph-start-p (start)
  "Return whether we are at the start of a paragraph.
START indicates the earliest place to look back to."
  ;; todo: make this understand various start-of-paragraph markup
  (and (bolp)
       (save-excursion
	 (goto-char (min (save-excursion
			   (beginning-of-line 0)
			   (back-to-indentation)
			   (point))
			 start))
	 (looking-at paragraph-start))))

(defun next-texp (n)
  "Move to the Nth next text expression.
Treat tagged markup blocks like bracketted expressions.
Treat paragraphs (in languages where they are marked by blank lines) as though
they had markup tags.  Likewise, treat sentences as blocks."
  ;; todo: skip comments
  (while (> n 0)
    (when debug-texp (message "next-texp(%d)" n))
    (let ((start (point)))
      (cond
       ;; tagged blocks
       ((save-excursion
	  (skip-to-actual-code)
	  (cond
	   ((memq major-mode '(html-helper-mode html-mode))
	    (looking-at "<[^/]"))
	   ((eq major-mode 'latex-mode)
	    (looking-at "\\\\begin"))))
	(when debug-texp (message "next-texp: tagged block %s" (match-string 0)))
	(skip-to-actual-code)
	(nested-blocks-forward))
       ;; other tags
       ((save-excursion
	  (skip-to-actual-code)
	  (cond
	   ((eq major-mode 'latex-mode)
	    (looking-at "\\\\[a-z]+"))))
	(when debug-texp (message "next-texp: tag %s" (match-string 0)))
	(skip-to-actual-code)
	(when (looking-at "\\\\\\([a-z]+\\)")
	  (goto-char (match-end 0))
	  (while (looking-at "[\[\{]")
	    (forward-sexp 1))))
       ;; paragraphs
       ((at-paragraph-start-p start)
	(when debug-texp (message "next-texp: paragraph"))
	(forward-paragraph)
	(skip-to-actual-code-backwards))
       ;; sentences
       ((save-excursion
	  (skip-syntax-backward " " start)
	  (looking-at (or (and (fboundp 'sentence-end)
			       ;; it's not a function before 23.
			       (sentence-end))
			  sentence-end)))
	(when debug-texp (message "next-texp: sentence"))
	(skip-to-actual-code)
	(forward-sentence))
       ;; brackets
       ((save-excursion
	  (skip-to-actual-code)
	  (looking-at "[[({]"))
	(when debug-texp (message "next-texp: brackets %s" (match-string 0)))
	(skip-to-actual-code)
	(forward-sexp 1))
       ;; if nothing else, take a word as an expression
       (t
	(when debug-texp (message "next-texp: word"))
	(forward-word 1))))
    (setq n (1- n))))

(defmodal next-sexp (html-mode html-helper-mode tex-mode latex-mode) (n)
  "next-sexp for markup languages.
Treats paired tags as brackets, and tries to do sensible
things with natural language punctuation."
  (next-texp n)
  ;; (message "next-sexp skipping from %d" (point))
  (skip-syntax-forward " .>")
  ;; (message "next-sexp skipped to %d" (point))
  (let ((start (point)))
    (next-texp 1)
    (versor-set-current-item start (point))
    (goto-char start)))

(defun versor-sgml-select-element ()
  "Set the versor selection to the element starting at point"
  (skip-syntax-forward "-")
  (condition-case evar
      (let* ((start (point))
	     (end (progn
		    (sgml-forward-element)
		    (skip-syntax-backward "-")
		    (point))))
	(versor-set-current-item start end)
	(goto-char start))
    (error nil)))

(defmodal next-sexp (sgml-mode xml-mode) (n)
  "next-sexp for the SGML group of languages."
  (if (catch 'hit-end
	(condition-case evar
	    (while (> n 0)
	      (sgml-forward-element)
	      (setq n (1- n)))
	  (error (throw 'hit-end t)))
	nil)
      (sgml-up-element))
  (versor-sgml-select-element))

(defmodal next-sexp (python-mode) (n)
  "next-sexp for python blocks."
  (if (save-excursion
	(skip-to-actual-code)
	(looking-at "[[({]"))
      (progn
	(skip-to-actual-code)
	(forward-sexp n)
	(let ((start (point)))
	  (next-sexp 1)
	  (versor-set-current-item start (point))
	  (goto-char start)))
    (skip-syntax-forward ".>")
    (while (> n 0)
      (py-end-of-statement)
      (setq n (1- n)))
    (skip-syntax-forward ".>")
    (skip-to-actual-code)
    (let ((start (point)))
      (py-end-of-statement)
      (skip-to-actual-code-backwards)
      (versor-set-current-item start (point))
      (goto-char start))))

(defmodal next-sexp (prolog-mode) (n)
  "next-sexp for prolog."
  (skip-to-actual-code)
  (let ((last (point)))
    (cond
     ((eq (point) (prolog-pred-start))
      (while (> n 0)
	(setq last (point))
	(prolog-end-of-predicate)
	(setq n (1- n))))
     ((eq (point) (prolog-clause-start))
      (while (> n 0)
	(setq last (point))
	(prolog-end-of-clause)
	(setq n (1- n))))
     (t
      (while (> n 0)
	(setq last (point))
	(prolog-forward-list)
	(setq n (1- n)))))
    (versor-set-current-item last (point))))

(defmodel previous-sexp (n)
  "Move backward N sexps.
Like backward-sexp but stops without error on reaching the start."
  (interactive "p"))

(defmodal previous-sexp (fundamental-mode) (n)
  "Move backward N sexps.
Like backward-sexp but stops without error on reaching the start.
This is the default implementation (defined as fundamental-mode) and it
should be used for normal programming language modes, including the various
kinds of Lisp modes."
  (interactive "p")
  (let* ((parse-sexp-ignore-comments t)
	 (where (safe-scan-sexps (point) (- n))))
    (if where
	(goto-char where)
      (if versor-move-out-when-at-end
	  (safe-backward-up-list)
	(message "No more previous sexps")))))

(defun previous-texp (n)
  "Move to the Nth previous text expression.Treat tagged markup blocks like bracketted expressions.
Treat paragraphs (in languages where they are marked by blank lines) as though
they had markup tags. Likewise, treat sentences as blocks."
  ;; todo: skip comments -- I think skip-to-actual-code-backwards should be doing this, but I'm not convinced that it really is
  (while (> n 0)
    (when debug-texp (message "previous-texp(%d)" n))
    (cond
     ;; tagged blocks
     ((save-excursion
	(skip-syntax-backward " .")
	(cond
	 ((memq major-mode '(html-helper-mode html-mode))
	  (backward-char 1)
	  (looking-at ">"))
	 ((eq major-mode 'latex-mode)
	  (skip-chars-backward "\\\\a-z{}")
	  (looking-at "\\\\end{[a-z]+}"))))
      (when debug-texp (message "previous-texp: tagged block %s" (match-string 0)))
      (skip-to-actual-code-backwards)
      (nested-blocks-backward))
    ;; paragraphs 
     ((save-excursion
	(beginning-of-line 0)
	(back-to-indentation) 
	(looking-at paragraph-start))
      (when debug-texp (message "previous-texp: paragraph"))
      (backward-paragraph 2)
      (skip-to-actual-code))
     ;; sentences
     ((save-excursion
	(skip-syntax-backward " ")
	(backward-char 1)
	(looking-at (or (and (fboundp 'sentence-end)
			     ;; it's not a function before 23.
			     (sentence-end))
			sentence-end)))
      (when debug-texp (message "previous-texp: sentence"))
      (skip-to-actual-code-backwards)
      (backward-sentence))
     ;; brackets
     ((save-excursion
	(skip-to-actual-code-backwards)
	(backward-char 1)
	(looking-at "[])}]"))
      (when debug-texp (message "previous-texp: brackets %s" (match-string 0)))
      (skip-to-actual-code-backwards)
      (backward-sexp 1))
     ;; words, but if it's really a tag, go back to the start of the tag syntax
     (t
      (when debug-texp (message "previous-texp: word"))
      (backward-word 1)
      (when (or (eq (char-before) ?\\)
		(eq (char-before) ?<))
	(when debug-texp (message "previous-texp: word markup adjustment"))
	(backward-char 1))))
    (setq n (1- n))))

(defmodal previous-sexp (html-mode html-helper-mode tex-mode latex-mode) (n)
  "Move backward N markup blocks."
  (when debug-texp (message "previous-sexp starting from %d" (point)))
  (previous-texp n)
  (when debug-texp (message "previous-texp took us to %d" (point)))
  (let ((start (point)))
    (next-texp 1)
    (when debug-texp (message "next-texp to find other end took us to %d" (point)))
    (versor-set-current-item start (point))
    (goto-char start)))

(defmodal previous-sexp (sgml-mode xml-mode) (n)
  "previous-sexp for the SGML group of languages."
  (if (catch 'hit-start 
	(condition-case evar 
	    (while (> n 0)
	      (sgml-backward-element)
	      (setq n (1- n)))
	  (error (throw 'hit-start t)))
	nil)
      (sgml-backward-up-element))
  (versor-sgml-select-element))

(defmodal previous-sexp (python-mode) (n)
  "previous-sexp for python blocks."
  (if (save-excursion
	(skip-to-actual-code)
	(looking-at "[]})]"))
      (progn
	(skip-to-actual-code-backwards)
	(backward-sexp n)
	(let ((start (point)))
	  (next-sexp 1)
	  (versor-set-current-item start (point))
	  (goto-char start)))
    (let ((initial-indent (current-indentation)))
      (while (> n 0)
	(py-previous-statement 1)
	(while (or (> (current-indentation) initial-indent)
		   (looking-at py-statement-tail-parts-regexp))
	  (py-previous-statement 1))
	(setq n (1- n))))
    (skip-to-actual-code)
    (let ((start (point)))
      (py-end-of-statement)
      (versor-set-current-item start (point))
      (goto-char start))))

(defmodal previous-sexp (prolog-mode) (n)
  "previous-sexp for prolog."
  (let ((last (point)))
    (cond
     ((or (eq (point) (prolog-pred-start))
	  (eq (point) (prolog-pred-end)))
      (while (> n 0)
	(setq last (point))
	(prolog-beginning-of-predicate)
	(setq n (1- n))))
     ((or (eq (point) (prolog-clause-start))
	  (eq (point) (prolog-clause-end)))
      (while (> n 0)
	(setq last (point))
	(prolog-beginning-of-clause)
	(setq n (1- n))))
     (t
      (while (> n 0)
	(setq last (point))
	(prolog-backward-list)
	(setq n (1- n)))))
    (versor-set-current-item (point) 
			     (save-excursion
			       (goto-char last)
			       (skip-to-actual-code-backwards)))))

(defmodel innermost-list ()
  "Move in by sexps until you can go in no more."
  (interactive))

(defmodal innermost-list (fundamental-mode) ()
  "Move in by sexps until you can go in no more."
  (interactive)
  (let ((p (point))
	(n nil))
    (while (setq n (safe-scan-lists p 1 -1))
      (setq p n))
    (goto-char p)))

;;;; words

(defun versor-previous-word (n)
  "Move backward a word, or, with argument, that number of words.
Like backward-word but skips comments."
  (interactive "p")
  (let ((was-in-comment (in-comment-p)))
    (while (> n 0)
      (backward-word 1)
      (if (in-comment-p)
	  (unless was-in-comment
	    (while (in-comment-p)
	      (backward-out-of-comment)
	      (backward-word 1)))
	(when was-in-comment
	  (comment-search-backward)))
      (setq n (1- n)))))

(defun versor-next-word (n)
  "Move forward a word, or, with argument, that number of words.
Like `forward-word' but leaves point on the first character of the word,
and never on the space or punctuation before it; and skips comments.
Argument N is the number of words to move forward."
  (interactive "p")
  (let ((was-in-comment (in-comment-p)))
    (while (> n 0)
      (forward-word 1)
      (if (in-comment-p)
	  (unless was-in-comment
	    (forward-comment 1))
	(when was-in-comment
	  (comment-search-forward (point-max))))
      (setq n (1- n))))
  (skip-syntax-forward "^w"))

(defun versor-end-of-word (n)
  "Move to the end of the current word, or, with argument, that number of words.
Argument N is the number of words to move forward."
  (interactive "p")
  (when (looking-at "\\>")
    (forward-word 1))
  (forward-word (1- n))
  (skip-syntax-forward "w"))

(defun versor-delete-word (n)
  "Delete N words."
  (interactive "p")
  (versor-as-motion-command item
    (let* ((spaced (and (= (char-before (car item)) ? )
			(= (char-after (cdr item)) ? ))))
      (delete-region (car item) (cdr item))
      (if spaced (just-one-space)))))

;;;; Phrases

(defun forward-phrase (n)
  "Move forward a phrase, or, with argument, that number of phrases.
Stay within the current sentence."
  (interactive "p")
  (let ((sentence (save-excursion
		    (forward-sentence 1)
		    (point))))
    (while (> n 0)
      (unless (re-search-forward phrase-end sentence 'stay)
	(setq n 0))
      (decf n))))

(defun versor-forward-phrase (n)
  "Like forward-phrase, but with some versor wrapping."
  (interactive "p")
  ;; todo: set the selection explicitly
  (forward-phrase n))

(defun end-of-phrase (&rest ignore)
  "Move to the end of the current phrase."
  (interactive)
  (let ((sentence (save-excursion
		    (forward-sentence 1)
		    (point))))
    (if (re-search-forward phrase-end sentence t)
      (goto-char (match-beginning 0))
      (goto-char (1- sentence)))))

(defun backward-phrase (n)
  "Move backward a phrase.
Stay within the current sentence."
  (interactive "p")
  (let* ((old-point (point))
	 (sentence (save-excursion
		     (backward-sentence 1)
		     (point)))
	 (using-sentence-start nil)
	 (in-end-area (save-excursion
			(when (bolp)
			  ;; phrase-end recognition can be confused by
			  ;; being just after a newline that is just
			  ;; after a phrase end
			  (end-of-line 0)
			  (setq old-point (point)))
			(re-search-backward phrase-end sentence t)
			(if (>= (match-end 0) old-point)
			    (match-beginning 0)
			  nil))))
    (when in-end-area
      (goto-char in-end-area))
    (while (> n 0)
      (unless (re-search-backward phrase-end sentence 'stay)
	(setq n 0
	      using-sentence-start t))
      (decf n))
    (unless using-sentence-start
      (goto-char (match-end 0)))))

(defun versor-backward-phrase (n)
  "Like backward-phrase, but with some versor wrapping."
  ;; todo: set the selection explicitly
  (interactive "p")
  (versor-as-motion-command item
    (versor-backward-without-sticking-at-comments
     backward-phrase n)))

(defun versor-backward-sentence (n)
  "Like backward-sentence, but with some versor wrapping."
  ;; todo: set the selection explicitly
  (interactive "p")
  (versor-as-motion-command item
    (versor-backward-without-sticking-at-comments
     backward-sentence n)))

(defun versor-forward-sentence (n)
  "Like `forward-sentence', but with some versor wrapping.
Argument N is how many times to do it."
  ;; todo: set the selection explicitly
  (interactive "p")
  (versor-as-motion-command item
    backward-sentence n))

;;;; tempo markers

(defun tempo-first-mark (n)
  "Go to the first tempo marker."
  (interactive "p")
  (goto-char (point-min))
  (tempo-forward-mark))

(defun tempo-previous-mark ()
  "Go to the previous tempo marker."
  (interactive "p")
  (while (> n 0)
    (tempo-backward-mark)
    (setq n (1- n))))

(defun tempo-next-mark (n)
  "Go to the next tempo marker."
  (interactive "p")
  (while (> n 0)
    (tempo-forward-mark)
    (setq n (1- n))))

(defun tempo-last-mark (n)
  "Go to the last tempo marker."
  (interactive "p")
  (goto-char (point-max))
  (tempo-backward-mark))

;;;; tabular text

(defun versor-set-mode-properties (mode properties)
  "Set the mode-specific versor properties for MODE to be PROPERTIES.
PROPERTIES is given as an alist."
  (mapcar (lambda (pair)
	    (put mode (car pair) (cdr pair)))
	  properties))

(versor-set-mode-properties
 'html-mode
 '((table-starter . "<table[^>]*>")
   (table-ender .  "</table[^>]*>")
   (row-starter . "<tr[^>]*>")
   (row-ender . "</tr[^>]*>")
   (cell-starter . "<t[dh][^>]*>")
   (cell-ender . "</t[dh][^>]*>")
   ))

(versor-set-mode-properties
 'html-helper-mode
 '((table-starter . "<table[^>]*>")
   (table-ender .  "</table[^>]*>")
   (row-starter . "<tr[^>]*>")
   (row-ender . "</tr[^>]*>")
   (cell-starter . "<t[dh][^>]*>")
   (cell-ender . "</t[dh][^>]*>")
   ))

(versor-set-mode-properties
 'texinfo-mode
 '((table-starter . "@multitable")
   (table-ender . "@end multitable")
   (row-starter . "@item")
   (cell-starter . "@tab")))

(versor-set-mode-properties
 'csv-mode
 '((table-starter . (lambda (a b)
		      (goto-char (point-min))
		      (set-match-data (list (point) (point)))))
   (table-ender . (lambda (a b)
		    (goto-char (point-max))
		    (set-match-data (list (point) (point)))))
   (row-starter . (lambda (a b)
		    (beginning-of-line 1)
		    (set-match-data (list (point) (point)))))
   (row-ender . (lambda (a b)
		  (end-of-line 1)
		  (set-match-data (list (point) (point)))))
   (next-row . (lambda (n)
		 (beginning-of-line (1+ n))))
   (previous-row . (lambda (n)
		     (beginning-of-line (- 1 n))))
   (cell-starter . (lambda (a b)
		     (csv-forward-field 1)
		     (set-match-data (list (point) (point)))))
   (cell-ender . (lambda (a b)
		   (csv-backward-field 1)
		   (set-match-data (list (point) (point)))))
   (next-cell . (lambda (n)
		  (csv-forward-field n)))
   (previous-cell . (lambda (n)
		      (csv-backward-field n)))))

(defun versor-table-starter ()
  "Return the table starter regexp for the current major mode."
  (or (get major-mode 'table-starter)
      (error "No table starter for %S" major-mode)))

(defun versor-table-ender ()
  "Return the table ender regexp for the current major mode."
  (or (get major-mode 'table-ender)
      (error "No table ender for %S" major-mode)))

(defun versor-row-starter ()
  "Return the row starter regexp for the current major mode."
  (or (get major-mode 'row-starter)
      (error "No row starter for %S" major-mode)))

(defun versor-row-ender ()
  "Return the row ender regexp for the current major mode."
   (get major-mode 'row-ender))

(defun versor-row-next ()
  "Return the next row function for the current major mode."
   (get major-mode 'next-row))

(defun versor-row-previous ()
  "Return the previous row function for the current major mode."
   (get major-mode 'previous-row))

(defun versor-cell-starter ()
  "Return the cell starter regexp for the current major mode."
  (or (get major-mode 'cell-starter)
      (error "No cell starter for %S" major-mode)))

(defun versor-cell-ender ()
  "Return the cell ender regexp for the current major mode."
  (get major-mode 'cell-ender))

(defun versor-cell-next ()
  "Return the next cell function for the current major mode."
   (get major-mode 'next-cell))

(defun versor-cell-previous ()
  "Return the previous cell function for the current major mode."
   (get major-mode 'previous-cell))


(defun re-search-forward-callable (regexp &optional bound noerror)
  "Like re-search-forward, but REGEXP can be a function.
If so, it is called on the other two arguments."
  (if (functionp regexp)
      (funcall regexp bound noerror)
    (re-search-forward regexp bound noerror)))

(defun re-search-backward-callable (regexp &optional bound noerror)
  "Like re-search-backward, but REGEXP can be a function.
If so, it is called on the other two arguments."
  (if (functionp regexp)
      (funcall regexp bound noerror)
    (re-search-backward regexp bound noerror)))

(defun versor-first-cell ()
  "Move to the first cell of the current row."
  (interactive)
  (if (and (re-search-backward-callable (versor-row-starter) (point-min) t)
	   (re-search-forward-callable (versor-cell-starter) (point-max) t))
      t
    (error "Could not locate first cell.")))

(defun versor-previous-cell (n)
  "Move to the previous cell."
  ;; todo: limit to within this row? or at least not include the row markup line
  (interactive "p")
  (let ((direct-action (versor-cell-previous)))
    (if direct-action
	(funcall direct-action n)
      (let* ((cell-starter (versor-cell-starter)))
	(while (> n -1)
	  (backward-char 1)
	  (re-search-backward-callable cell-starter (point-min) t)
	  (setq n (1- n)))
	(let ((start-starter (point))
	      (start-ender (match-end 0))
	      (ender (versor-cell-ender)))
	  (save-excursion
	    (if ender
		(re-search-forward-callable ender (point-max) t)
	      (forward-char 1)
	      (re-search-forward-callable (versor-cell-starter) (point-max) t)
	      (goto-char (1- (match-beginning 0))))
	    (versor-set-current-item start-starter (point)))
	  (goto-char start-ender)
	  (skip-to-actual-code))))))

(defun versor-next-cell (n)
  "Move to the Nth next cell."
  ;; todo: limit to within this row? or at least not include the row markup line
  (interactive "p")
  (let ((direct-action (versor-cell-next)))
    (message "versor-next-cell: direct=%S" direct-action)
    (if direct-action
	(funcall direct-action n)
      (forward-char 1) ; so we can search for next starter even if already on one
      (let* ((starter (versor-cell-starter))
	     (ender (versor-cell-ender)))
	(message "starter=%S ender=%S" starter ender)
	(while (> n 0)
	  (message "Searching from %d for starter %S" (point) starter)
	  (if (not (re-search-forward-callable starter (point-max) t))
	      (error "No more next cells")
	    (decf n)))
	(let ((starter-start (match-beginning 0))
	      (starter-end (point)))
	  (message "starter runs %d..%d, ender is %S" starter-start starter-end ender)
	  (save-excursion
	    (if ender
		(re-search-forward-callable ender (point-max) t)
	      (message "No ender, improvising by going just before next %S" starter)
	      (forward-char 1)
	      (re-search-forward-callable (versor-cell-starter) (point-max) t)
	      (goto-char (1- (match-beginning 0))))
	    (message "got %d as end position" (point))
	    (versor-set-current-item
	     starter-end ;;starter-start
	     (point)))
	  (goto-char starter-end)
	  (skip-to-actual-code)
	  (message "went to starter-end %d and skipped to %d" starter-end (point)))))))

(defun versor-last-cell ()
  "Move to the last cell of the current row."
  (interactive)
  ;; todo: finish this properly
  (if (and (re-search-forward-callable (versor-row-ender) (point-max) t)
	   (re-search-backward-callable (versor-cell-starter) (point-min) t))
      (goto-char (match-end 0))
    (error "Could not locate last cell")))

(defun versor-first-row ()
  "Move to the first row of the current table."
  (interactive)
  ;; todo: finish this properly
  (if (and (search-backward (versor-table-starter) (point-min) t)
	   (re-search-forward-callable (versor-row-starter) (point-max) t))
      t
    (error "Could not locate first row")))

(defun versor-previous-row (n)
  "Move to the previous row."
  (interactive "p")
  (let ((direct-action (versor-row-previous)))
    (if direct-action
	(funcall direct-action n)
      (let* ((limit (save-excursion
		      (re-search-backward-callable (versor-table-starter) (point-min) t)
		      (match-end 0)))
	     (row-starter (versor-row-starter))
	     (ender (versor-row-ender)))
	(while (> n -1)
	  (backward-char 1)
	  (re-search-backward-callable row-starter limit t)
	  (setq n (1- n)))
	(let ((start-starter (point))
	      (start-ender (match-end 0)))
	  (save-excursion
	    (if ender
		(re-search-forward-callable ender (point-max) t)
	      (forward-char 1)
	      (re-search-forward-callable (versor-row-starter) (point-max) t)
	      (goto-char (1- (match-beginning 0))))
	    (versor-set-current-item start-starter (point)))
	  (goto-char start-ender)
	  (skip-to-actual-code))))))

(defun versor-next-row (n)
  "Move to the Nth next row."
  (interactive "p")
  (let ((direct-action (versor-row-next)))
    (message "versor-next-row: direct=%S" direct-action)
    (if direct-action
	(funcall direct-action n)
      (forward-char 1)
      (let* ((ender (versor-row-ender))
	     (limit (save-excursion
		      (re-search-forward-callable (versor-table-ender) (point-max) t)
		      (match-beginning 0)))
	     (row-starter (versor-row-starter)))
	(while (> n 0)
	  (if (re-search-forward-callable row-starter limit t)
	      (decf n)
	    (setq n 0)))
	(let ((start-ender (point))
	      (start-starter (match-beginning 0)))
	  (save-excursion
	    (if ender
		(re-search-forward-callable ender limit t)
	      (forward-char 1)
	      (if (re-search-forward-callable (versor-row-starter) limit t)
		  (goto-char (1- (match-beginning 0)))
		(goto-char limit)))
	    (versor-set-current-item start-starter (point)))
	  (goto-char start-ender)
	  (skip-to-actual-code))))))

(defun versor-last-row ()
  "Move to the last row of the current table."
  (interactive)
  ;; todo: finish this properly
  (if (and (search-forward (versor-table-ender) (point-max) t)
	   (re-search-backward-callable (versor-row-starter) (point-min) t))
      (goto-char (match-end 0))
    (error "Could not locate last row")))

;;;; defuns

(defun versor-first-defun ()
  "Move to the start of the first function definition in the buffer."
  (interactive)
  (goto-char (point-min))
  (beginning-of-defun -1))

(defun versor-previous-defun (n)
  "Move to the start of the previous function definition."
  (interactive "p")
  (beginning-of-defun n))

(defun versor-next-defun (n)
  "Move to the start of the next function definition."
  (interactive "p")
  (beginning-of-defun (- n)))

(defun versor-last-defun ()
  "Move to the start of the last function definition in the buffer."
  (interactive)
  (goto-char (point-max))
  (beginning-of-defun 1))

;;;; ELSE

(defun versor-else-first-placeholder ()
  "Move to the first placeholder."
  (interactive)
  (goto-char (point-min))
  (else-next-placeholder))

(defun versor-else-last-placeholder ()
  "Move to the last placeholder."
  (interactive)
  (goto-char (point-max))
  (else-previous-placeholder))

;;;; marks

(defvar versor-latest-mark 0
  "The latest mark versor's \"mark\" dimension has gone to in this buffer.")

(make-variable-buffer-local 'versor-latest-mark)

(defvar versor-sorted-marks nil
  "The marks in this buffer, sorted for versor to use.")

(make-variable-buffer-local 'versor-sorted-marks)

(defvar versor-latest-sorted-mark 0
  "The latest mark versor's \"sorted-mark\" dimension has gone to in this buffer.")

(make-variable-buffer-local 'versor-latest-sorted-mark)

(defadvice push-mark (after versor () activate)
  "Invalidate versor's mark-related data when a new mark is made."
  (setq versor-latest-mark 0
	versor-sorted-marks nil
	versor-latest-sorted-mark 0))

(defadvice pop-mark (after versor () activate)
  "Invalidate versor's mark-related data when a old mark is popped."
  (setq versor-latest-mark 0
	versor-sorted-marks nil
	versor-latest-sorted-mark 0))

(defun first-mark ()
  "Move to the first-mark"
  (interactive)
  (setq versor-latest-mark 0)
  (goto-char (nth versor-latest-mark mark-ring)))

(defun previous-mark (&optional n)
  "Move to the previous-mark"
  (interactive "p")
  (setq versor-latest-mark (min (1+ versor-latest-mark)
				(1- (length mark-ring))))
  (goto-char (nth versor-latest-mark mark-ring)))

(defun next-mark (&optional n)
  "Move to the next-mark"
  (interactive "p")
  (setq versor-latest-mark (max (1- versor-latest-mark)
				0))
  (goto-char (nth versor-latest-mark mark-ring)))

(defun last-mark ()
  "Move to the last-mark"
  (interactive "p")
  (setq versor-latest-mark (1- (length mark-ring)))
  (goto-char (nth versor-latest-mark mark-ring)))

(defun versor-make-sorted-marks ()
  "Make a sorted copy of the mark ring."
  (when (null versor-sorted-marks)
    (setq versor-sorted-marks
	  (sort (copy-alist mark-ring)
		'<)
	  versor-latest-sorted-mark 0)))

(defun first-sorted-mark ()
  "Move to the first-sorted-mark"
  (interactive)
  (versor-make-sorted-marks)
  (setq versor-latest-sorted-mark 0)
  (goto-char (nth versor-latest-sorted-mark versor-sorted-marks)))

(defun previous-sorted-mark (&optional n)
  "Move to the previous-sorted-mark"
  (interactive "p")
  (versor-make-sorted-marks)
  (setq versor-latest-sorted-mark (min (1+ versor-latest-sorted-mark)
				(1- (length versor-sorted-marks))))
  (goto-char (nth versor-latest-sorted-mark versor-sorted-marks)))

(defun next-sorted-mark (&optional n)
  "Move to the next-sorted-mark"
  (interactive "p")
  (versor-make-sorted-marks)
  (setq versor-latest-sorted-mark (max (1- versor-latest-sorted-mark)
				0))
  (goto-char (nth versor-latest-sorted-mark versor-sorted-marks)))

(defun last-sorted-mark ()
  "Move to the last-sorted-mark"
  (interactive "p")
  (versor-make-sorted-marks)
  (setq versor-latest-sorted-mark (1- (length versor-sorted-marks)))
  (goto-char (nth versor-latest-sorted-mark versor-sorted-marks)))

;;;; property changes

(defun goto-first-property-change (n)
  "Go to the first property change."
  (interactive "p")
  (goto-char (next-property-change (point-min) (current-buffer))))

(defun goto-next-property-change (n)
  "Go to the next property change."
  (interactive "p")
  (goto-char (next-property-change (point) (current-buffer))))
		       
(defun goto-previous-property-change (n)
  "Go to the previous property change."
  (interactive "p")
  (goto-char (previous-property-change (point) (current-buffer))))
		       
(defun goto-last-property-change (n)
  "Go to the last property change."
  (interactive "p")
  (goto-char (previous-property-change (point-max) (current-buffer))))

(provide 'versor-base-moves)

;;;; end of versor-base-moves.el

(provide 'versor-base-moves)

;;; versor-base-moves.el ends here
