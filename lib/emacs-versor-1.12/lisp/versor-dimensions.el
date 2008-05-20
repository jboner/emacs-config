;;; versor-dimensions.el -- versatile cursor
;;; Time-stamp: <2008-01-23 12:27:44 jcgs>
;;
;; Copyright (C) 2007, 2008, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2004
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2006, 2007  John C. G. Sturdy
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

(require 'versor-names)

;;; Code:
(mapcar 'makunbound '(versor-current-level-name moves-moves versor-meta-level versor-level))

(mapcar (lambda (nb-command) (autoload nb-command "nested-blocks" nil t))
	'(nested-blocks-backward nested-blocks-forward
	  nested-blocks-leave-backwards nested-blocks-enter))

(defvar versor-meta-level 1
  "The current versor meta-level, as an index into `moves-moves'.")

(defvar versor-level 1
  "The current versor level, as an index into (aref moves-moves versor-meta-level).")

(defvar versor-meta-level-shadow nil
  "If non-nil, the value to use instead of `versor-meta-level'.
Bound in `versor-do-dynamic-menu' because otherwise we end up with the
wrong meta-level, as we have just come out of some menuing code.
Other uses for this might be found.")

(defvar versor-level-shadow nil
  "If non-nil, the value to use instead of `versor-level'.
Bound in `versor-do-dynamic-menu' because otherwise we end up with the
wrong level, as we have just come out of some menuing code.
Other uses for this might be found.")

(defmacro versor-level-name (level)
  "Return the name of LEVEL."
  `(first (aref (versor-current-meta-level) ,level)))

(defmacro versor-meta-level-name (meta-level)
  "Return the name of META-LEVEL."
  `(aref (aref moves-moves ,meta-level) 0))

(defun versor-make-movemap-set (name &rest movemaps)
  "Make a set of move-maps called NAME from the remaining args MOVEMAPS..
The lowest-level (finest-grain) movemap should come first.
Any nil elements are not included.
A movemap-set represents a metalevel of movements.
Also, all the movemap-sets are grouped together using another movemap-set,
called `moves-moves', which is the root variable of the versor system."
  (if (memq nil movemaps)
      (let ((mm nil))
	(while movemaps
	  (when (car movemaps)
	    (setq mm (cons (car movemaps) mm)))
	  (setq movemaps (cdr movemaps)))
	(apply 'vector name (nreverse mm)))
    (apply 'vector name movemaps)))

(defun versor-make-movemap (name)
  "Make a movemap called NAME.
A move map is a list whose head is the name of the map,
and whose tale is an alist of moves and the commands which execute them.
Moves are named by the following symbols, and possibly others added since
this documentation was written:
  previous
  next
  first
  last
  mark
  delete
  end-of-item
  color
The pseudo-move \"color\" gives the cursor colour to use when this move map
is current.
you can fill in the contents of a move map by using versor-define-move and
versor-define-moves.
Move maps are grouped together by versor-make-movemap-set."
  (list name))

(defun versor-define-move (movemap move command)
  "In MOVEMAP define MOVE to do COMMAND.  Analogous to `define-key'.
See the definition of `versor-make-movemap' for details of move maps."
  (let ((pair (assoc move movemap)))
    (if pair
	(rplacd pair command)
      (rplacd movemap
	      (cons (cons move
			  command)
		    (cdr movemap))))))

(defun versor-define-moves (movemap move-command-specs)
  "In MOVEMAP define each of MOVE-COMMAND-SPECS.
We can't just splice MOVE-COMMAND-SPECS into the map because that would
not interact properly with any existing definitions in the map.
See the definition of `versor-make-movemap' for details of move maps."
  (mapcar
   (function
    (lambda (k-c)
      (versor-define-move movemap (first k-c) (second k-c))))
   move-command-specs))

(mapcar (function
	 (lambda (name)
	   (set (intern (concat "movemap-" name))
		(versor-make-movemap name))))
	'("default-chars"
	  "default-lines"
	  "default-pages"
	  "chars"
	  "lines"
	  "pages"
	  "exprs"
	  "depth"
	  "statement-parts"
	  "statements"
	  "defuns"
	  "words"
	  "phrases"
	  "sentences"
	  "paragraphs"
	  ;; "blocks"
	  ;; "block-depth"
	  "cells"
	  "rows"
	  "mark-ring"
	  "sorted-mark-ring"
	  "tempo"
	  "else"
	  "property-changes"
	  "references"))

(versor-define-moves movemap-default-chars
		      '((color "purple")
			(first beginning-of-line)
			(previous versor-backward-char)
			(next versor-forward-char)
			(last end-of-line)
			;; (delete delete-char)
			(transpose transpose-chars)
			))

(versor-define-moves movemap-default-lines
		     '((color "gray")
		       (:background "gray")
		       (start-of-item move-beginning-of-this-line)
		       (first beginning-of-buffer)
		       (previous previous-line)
		       (next next-line)
		       (last end-of-buffer)
		       (end-of-item end-of-line)
		       ;; (delete kill-line)
			(transpose transpose-lines)
		       ))

(versor-define-moves movemap-default-pages
		     '((color "black")
		       (other-color "gray")
		       (start-of-item back-to-indentation)
		       (first beginning-of-buffer)
		       (previous
			;; scroll-down
			forward-page
			)
		       (next
			;; scroll-up
			forward-page
			)
		       (start-of-item back-to-indentation)
		       (last end-of-buffer)))

(versor-define-moves movemap-chars
		      '((color "purple")
			(first versor-start-of-line)
			(previous versor-backward-char)
			(next versor-forward-char)
			(last end-of-line)
			;; (delete delete-char)
			(transpose transpose-chars)
			))

(versor-define-moves movemap-lines
		     '((color "gray")
		       (:background "black")
		       (:foreground "white")
		       (first beginning-of-buffer)
		       (previous versor-previous-line)
		       (next versor-next-line)
		       (last end-of-buffer)
		       (start-of-item back-to-indentation)
		       (end-of-item end-of-line)
		       ;; (delete kill-line)
			(transpose transpose-lines)
		       ))

(versor-define-moves movemap-pages
		     '((color "white")
		       (other-color "gray")
		       (first beginning-of-buffer)
		       (previous
			;; scroll-down
			forward-page
			)
		       (next
			;; scroll-up
			forward-page
			)
		       (start-of-item move-beginning-of-this-line)
		       (last end-of-buffer)))

(versor-define-moves movemap-exprs
		     '((color "green")
		       (:underline "dark green")
		       (:background "pale green")
		       (first first-sexp)
		       (previous previous-sexp)
		       (next next-sexp)
		       (end-of-item forward-sexp)
		       (last last-sexp)
		       (mark mark-sexp)
		       ;; (delete kill-sexp)
		       (start-of-item skip-to-actual-code)
		       (transpose transpose-sexps)
		       (dwim versor-dwim-lispishly)
		       ))

(versor-define-moves movemap-depth
		     '((color "orange")
		       (:background "orange")
		       (:foreground "green")
		       (first beginning-of-defun)
		       (previous versor-backward-up-list)
		       (next versor-down-list)
		       (last innermost-list)
		       (dwim versor-dwim-lispishly)))

(versor-define-moves movemap-statement-parts
		     '((color "cyan")
		       (:background "cyan")
		       (:foreground "red")
		       (first navigate-this-head)
		       (previous statement-navigate-parts-previous)
		       (next statement-navigate-parts-next)
		       (last navigate-this-body)
		       (start-of-item skip-to-actual-code)
		       (end-of-item latest-statement-navigation-end)
		       (surround locate-this-container)
		       (extension-offset 1)
		       (dwim versor-dwim-lispishly)))

(versor-define-moves movemap-statements
		     '((color "cyan")
		       (:background "cyan")
		       (:foreground "green")
		       (first beginning-of-defun)
		       (previous previous-statement)
		       (next next-statement)
		       (last end-of-defun) ;;;;;;;;;;;;;;;; make this go back one statement from the end of the defun
		       (end-of-item latest-statement-navigation-end)
		       (start-of-item skip-to-actual-code)
		       (surround locate-this-container)
		       (dwim versor-dwim-lispishly)))

(versor-define-moves movemap-defuns
		     '((color "yellow")
		       (:background "yellow")
		       (:foreground "black")
		       (first versor-first-defun)
		       (previous versor-previous-defun)
		       (next versor-next-defun)
		       (start-of-item skip-to-actual-code)
		       (end-of-item end-of-defun)
		       (last versor-last-defun)
		       ;; (transpose transpose-sexps); would only work for lisp?
		       (dwim versor-dwim-lispishly)
		       ))

(versor-define-moves movemap-words
		     '((color "grey")
		       (other-color "green")
		       (:background "light gray")
		       (:foreground "black")
		       (:underline "dark slate gray")
		       ;; things like this (notionally the wrong
		       ;; dimension) still work OK, because of how
		       ;; versor-indicate-current-item works when the
		       ;; things it calls don't explicitly set the item
		       ;; boundaries for it:
		       (first versor-backward-phrase)
		       (previous versor-previous-word)
		       (next versor-next-word)
		       (start-of-item skip-to-actual-code)
		       (end-of-item versor-end-of-word)
		       (last versor-forward-phrase)
		       ;; (delete versor-delete-word)
		       (transpose transpose-words)
		       (dwim versor-dwim-textually)))

(versor-define-moves movemap-phrases
		     '((color "blue")
		       (:background "cornflower blue")
		       (:foreground "black")
		       (first versor-backward-sentence)
		       (previous versor-backward-phrase)
		       (next versor-forward-phrase)
		       (start-of-item skip-to-actual-code)
		       (end-of-item end-of-phrase)
		       (last forward-sentence)
		       (dwim versor-dwim-textually)))

(versor-define-moves movemap-sentences
		     '((color "cyan")
		       (:background "light sky blue")
		       (:foreground "black")
		       (first versor-backward-paragraph)
		       (previous versor-backward-sentence)
		       (next forward-sentence)
		       (last versor-forward-paragraph)
		       (start-of-item skip-to-actual-code)
		       (transpose transpose-sentences)
		       (dwim versor-dwim-textually)))

(versor-define-moves movemap-paragraphs
		     '((color "yellow")
		       (:background "yellow")
		       (:foreground "red")
		       (first beginning-of-buffer)
		       (previous versor-backward-paragraph)
		       (next versor-forward-paragraph)
		       (start-of-item skip-to-actual-code)
		       (end-of-item versor-end-of-paragraph)
		       (last end-of-buffer)
		       (transpose transpose-paragraphs)
		       (dwim versor-other-end-of-item ;; versor-dwim-textually
			     )))

;; (versor-define-moves movemap-blocks
;; 		     '((color "green")
;; 		       (:underline "dark green")
;; 		       (:foreground "white")
;; 		       (:background "pale green")
;; 		       (previous nested-blocks-backward)
;; 		       (next nested-blocks-forward)))

;; (versor-define-moves movemap-block-depth
;; 		     '((color "orange")
;; 		       (:foreground "black")
;; 		       (:background "orange")
;; 		       (previous nested-blocks-leave-backwards)
;; 		       (next nested-blocks-enter)))

(versor-define-moves movemap-cells
		     '((color "blue")
		       (:background "cyan")
		       (first versor-first-cell)
		       (previous versor-previous-cell)
		       (next versor-next-cell)
		       (last versor-last-cell)
		       (start-of-item skip-to-actual-code)))

(versor-define-moves movemap-rows
		     '((color "cyan")
		       (:background "light sky blue")
		       (:foreground "black")
		       (first versor-first-row)
		       (previous versor-previous-row)
		       (next versor-next-row)
		       (last versor-last-row)
		       (start-of-item skip-to-actual-code)))

(versor-define-moves movemap-mark-ring
		     '((color "orange")
		       (:background "orange")
		       (:foreground "blue")
		       (first first-mark)
		       (previous previous-mark)
		       (next next-mark)
		       (last last-mark)))

(versor-define-moves movemap-sorted-mark-ring
		     '((color "orange")
		       (:background "orange")
		       (:foreground "blue")
		       (first first-sorted-mark)
		       (previous previous-sorted-mark)
		       (next next-sorted-mark)
		       (last last-sorted-mark)))

(versor-define-moves movemap-tempo
		     '((color "orange")
		       (:background "orange")
		       (:foreground "blue")
		       (first (tempo-first-mark))
		       (previous tempo-previous-mark)
		       (next tempo-next-mark)
		       (last (tempo-last-mark))))

(versor-define-moves movemap-else
		     '((color "orange")
		       (:background "orange")
		       (:foreground "blue")
		       (first versor-else-first-placeholder)
		       (previous else-previous-placeholder)
		       (next else-next-placeholder)
		       (last versor-else-last-placeholder)))

(versor-define-moves movemap-property-changes
		     '((color "orange")
		       (:background "orange")
		       (:foreground "blue")
		       (first (goto-first-property-change))
		       (previous (goto-next-property-change))
		       (next (goto-previous-property-change))
		       (last (goto-last-property-change))))

(versor-define-moves movemap-references
		     '((color "orange")
		       (:background "orange")
		       (:foreground "blue")
		       (first )
		       (previous )
		       (next )
		       (last )))

;; See versor-make-movemap-set for details of movemap-sets
		       
(setq moves-default (versor-make-movemap-set "default"
					     movemap-default-chars
					     movemap-default-lines
					     movemap-default-pages)

      moves-cartesian (versor-make-movemap-set "cartesian"
					       movemap-chars
					       movemap-lines
					       movemap-pages)

      moves-structural (versor-make-movemap-set "structural"
						movemap-chars
						movemap-exprs
						movemap-depth
						movemap-defuns)

      moves-text (versor-make-movemap-set "text"
					  movemap-chars
					  movemap-words
					  movemap-phrases
					  movemap-sentences
					  movemap-paragraphs)

      moves-structured-text (versor-make-movemap-set "structured text"
						     movemap-chars
						     movemap-words
						     movemap-exprs
						     movemap-depth
						     movemap-sentences
						     movemap-paragraphs)

      moves-tables (versor-make-movemap-set "tables"
					    movemap-chars
					    movemap-cells
					    movemap-rows)

      moves-program (versor-make-movemap-set "program"
					     movemap-chars
					     movemap-exprs
					     movemap-statement-parts
					     movemap-statements
					     movemap-defuns)

      moves-markers (versor-make-movemap-set "markers"
					     movemap-mark-ring
					     movemap-sorted-mark-ring
					     (if (featurep 'tempo) movemap-tempo nil)
					     (if (featurep 'else) movemap-else nil)
					     movemap-property-changes))

(defvar moves-moves
  (versor-make-movemap-set "metamoves"
			   moves-default
			   moves-cartesian
			   moves-structural
			   moves-text
			   moves-structured-text
			   moves-tables
			   moves-program
			   ;; moves-markers
			   )
  "The map of meta-moves.
See `versor-make-movemap-set' for the description of move map sets.
Note that this is a reuse of that data type at a different level.")

(defmacro versor-current-meta-level ()
  "The current meta-level, as an array."
  '(aref moves-moves (or versor-meta-level-shadow versor-meta-level)))

(defun versor-current-level (&optional level-offset)
  "Return the current level, as an array.
With optional LEVEL-OFFSET, add that to the level first."
  (if (integerp level-offset)
      (let ((meta (versor-current-meta-level)))
	(aref meta (min (+ (or versor-level-shadow versor-level) level-offset)
			(1- (length meta)))))
    (aref (versor-current-meta-level) (or versor-level-shadow versor-level))))

(defun versor-action (level action)
  "From LEVEL get ACTION, which will be a move such as next or previous."
  (cdr (assoc action level)))

(defvar versor-current-over-level-name (first (versor-current-level 1))
  "The name of the current versor level, for display in the `global-mode-string'.")

(defvar versor-current-level-name (first (versor-current-level))
  "The name of the current versor level, for display in the `global-mode-string'.")

(defvar versor-current-meta-level-name (aref (versor-current-meta-level) 0)
  "The name of the current versor meta-level, for display in the `global-mode-string'.")

(defun versor-trim-level ()
  "Ensure that `versor-level' is in range."
  (let ((max (1- (length (versor-current-meta-level)))))
    (when (> versor-level max)
      (setq versor-level
	    (if versor-level-wrap 1 max)))
    (when (< versor-level 1)
      (setq versor-level
	    (if versor-level-wrap max 1)))))

(defun versor-trim-meta-level ()
  "Ensure that `versor-meta-level' is in range."
  (let ((max (1- (length moves-moves))))
    (when (> versor-meta-level max)
      (setq versor-meta-level
	    (if versor-meta-level-wrap 1 max)))
    (when (< versor-meta-level 1)
      (setq versor-meta-level
	    (if versor-meta-level-wrap max 1)))))

(defvar versor-meta-dimensions-valid-for-modes
  '(((emacs-lisp-mode lisp-mode scheme-mode lisp-interaction-mode)
     t "cartesian" "structural" "text" "program" "markers")
    ((texinfo-mode text-mode mail-mode tex-mode latex-mode html-mode html-helper-mode)
     t "cartesian" "structural" "text" "structured text" "tables" "markers")
    ((c-mode perl-mode java-mode)
     t "cartesian" "structural" "program" "text" "markers")
    (t nil))				; allow any
  "*Controls which meta-dimensions are valid for which major modes.
If t, all meta-dimensions are allowed in all major modes.
Otherwise, it is an alist mapping modes to sublists describing the
meta-dimensions allowed in that mode.
Each sublist (beyond the mode) should begin with t, to indicate that
only the meta-dimensions listed are to be allowed, or nil, to indicate
that all meta-dimensions except those listed are allowed.
The rest of the sublist is the meta-dimensions allowed or blocked for that mode.
The head of the node may also be a list of major modes for which this
rule applies.
A sublist for a major mode t gives the defaults.")

(defun assoc-multi-key (key list)
  "Look for KEY in the cars of LIST.
Like assoc, return the element of list for which it matches."
  (catch 'found
    (while list
      (if (if (consp (caar list))
	      (member key (caar list))
	    (equal key (caar list)))
	  (throw 'found (car list))
	(setq list (cdr list))))
    nil))

(defun versor-meta-dimension-valid-for-mode (meta-name mode)
  "Return whether the meta-dimension called META-NAME is allowed in MODE."
  ;; smug -- worked first time
  (cond
   ((eq versor-meta-dimensions-valid-for-modes t)
    t)
   ((consp versor-meta-dimensions-valid-for-modes)
    (let* ((descr (cdr (or (assoc major-mode versor-meta-dimensions-valid-for-modes)
			   (assoc-multi-key major-mode versor-meta-dimensions-valid-for-modes)
			   (assoc t versor-meta-dimensions-valid-for-modes))))
	   (allowing (car descr))
	   (mentioned (member meta-name (cdr descr))))
      (or (and allowing mentioned)
	  (and (not allowing) (not mentioned)))))
   (t t)))

(defun versor-mode-levels-triplet (spec)
  "Convert SPEC to the form needed for `versor-mode-current-levels'.

SPEC is a list of mode name (as symbol), meta-level and level
names (as strings).

The result is (mode . (meta . level)) with meta and level as
numbers.

This is a convenience function for use with mapcar for your
.emacs to produce a ready-made starting point for
`versor-mode-current-levels'."
  (let ((result
	 (cons (first spec)
	       (versor-find-level-by-double-name (second spec) (third spec)))))
    (message "versor-mode-levels-triplet %S --> %S" spec result)
    result))

(defvar versor-mode-current-levels
  (mapcar 'versor-mode-levels-triplet
	  '((emacs-lisp-mode "structural" "exprs")
	    ("emacs-lisp-mode" "text" "words")
	    (lisp-interaction-mode "structural" "exprs")
	    (c-mode "program" "statement-parts")
	    ("c-mode" "text" "words")
	    (text-mode "cartesian" "lines")
	    (html-helper-mode "structured text" "words")
	    ("html-helper-mode" "text" "chars")
	    (html-mode "structured text" "words")
	    ("html-mode" "text" "chars")
	    (latex-mode "structured text" "words")
	    ("latex-mode" "text" "chars")
	    (texinfo-mode "structured text" "words")
	    ("texinfo-mode" "text" "chars")
	    ))
  "Alist of mode name symbols to the current meta-level and level for that mode.
Used by versor-local, but defined in versor-dimensions.

To enable per-mode switching of versor dimensions, load the
library `versor-local' and set the variable
`versor-auto-change-for-modes' non-nil.

As well as mode name symbols, you can put strings naming the modes;
these specify the meta-level and level to use for embedded strings
within that mode (such as string literals, and comments).
To enable the separate handling of embedded text, set
`versor-text-in-code' non-nil.")

(require 'versor-local) (defvar versor-am-in-text-in-code nil) (versor-display-modal-levels)

(defvar versor-equivalent-commands
  '(
    ;; the first few are the ones for which versor has a near
    ;; equivalent
    (backward-paragraph . versor-backward-paragraph)
    (backward-up-list . versor-backward-up-list)
    ;; (kill-word . versor-delete-word)
    (down-list . versor-down-list)
    (forward-paragraph . versor-end-of-paragraph)
    (forward-word . versor-end-of-word)
    (forward-paragraph . versor-forward-paragraph)
    (end-of-defun . versor-next-defun)
    (next-line . versor-next-line)
    (forward-word . versor-next-word)
    (backward-word . versor-previous-word)
    (beginning-of-defun . versor-previous-defun)
    (previous-line . versor-previous-line)
    (beginning-of-line . versor-start-of-line)
    (forward-sexp . next-sexp)
    )
  "Alist mapping commands to their nearest Versor equivalents.")

(defvar versor-used-commands
  '(versor-backward-char
    versor-backward-phrase
    versor-backward-sentence
    backward-word
    beginning-of-buffer
    beginning-of-defun
    end-of-buffer
    end-of-defun
    end-of-line
    end-of-phrase
    first-sexp
    versor-forward-char
    forward-page
    versor-forward-phrase
    forward-sentence
    forward-sexp
    innermost-list
    last-sexp
    latest-statement-navigation-end
    mark-sexp
    navigate-this-body
    navigate-this-head
    nested-blocks-backward
    nested-blocks-enter
    nested-blocks-forward
    nested-blocks-leave-backwards
    next-sexp
    next-statement
    previous-sexp
    previous-statement
    statement-navigate-parts-next
    statement-navigate-parts-previous
    transpose-chars
    transpose-lines
    transpose-paragraphs
    else-next-placeholder
    else-previous-placeholder
    versor-else-first-placeholder
    versor-else-last-placeholder
    transpose-sentences
    transpose-sexps
    transpose-words
    versor-backward-paragraph
    versor-backward-up-list
    ;; versor-delete-word
    versor-down-list
    versor-dwim-lispishly
    versor-dwim-textually
    versor-end-of-paragraph
    versor-end-of-word
    versor-first-cell
    versor-first-defun
    versor-first-row
    versor-forward-paragraph
    versor-last-cell
    versor-last-defun
    versor-last-row
    versor-next-cell
    versor-next-defun
    versor-next-line
    versor-next-row
    versor-next-word
    versor-previous-cell
    versor-previous-defun
    versor-previous-line
    versor-previous-row
    versor-start-of-line
    )
  "Commands that Versor uses directly as actions.")

(defun versor-find-in-current-dimension (command &optional level-offset)
  "Return the versor command that would run COMMAND in the current dimension.
With optional LEVEL-OFFSET, add that to the dimension number to look in."
  (car (rassoc command (versor-current-level level-offset))))

(defun versor-command-for-action (action &optional next-level)
  "Return the versor command that would do ACTION in the current level.
With optional second arg NEXT-LEVEL non-nil, look in the next level up instead."
  (cdr (assoc action
	      (if next-level
		  '((first . versor-over-start)
		    (previous . versor-over-prev)
		    (next . versor-over-next)
		    (last . versor-over-end))
		'((first . versor-start)
		  (previous . versor-prev)
		  (next . versor-next)
		  (last . versor-end)
		  (end . versor-end-of-item)
		  (delete . versor-kill)
		  (copy . versor-copy)
		  (transpose . versor-transpose)
		  (mark . versor-mark))))))

(provide 'versor-dimensions)

;;;; end of versor-dimensions.el

(provide 'versor-dimensions)

;;; versor-dimensions.el ends here
