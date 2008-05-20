; /* ====================================================================
;  * Copyright (c) 2004 Carnegie Mellon University.  All rights
;  * reserved.
;  *
;  * Redistribution and use in source and binary forms, with or without
;  * modification, are permitted provided that the following conditions
;  * are met:
;  *
;  * 1. Redistributions of source code must retain the above copyright
;  *    notice, this list of conditions and the following disclaimer. 
;  *
;  * 2. Redistributions in binary form must reproduce the above copyright
;  *    notice, this list of conditions and the following disclaimer in
;  *    the documentation and/or other materials provided with the
;  *    distribution.
;  *
;  * This work was supported in part by AT&T Labs, CMU Sphinx Speech
;  * Consortium and Clairgrove, LLC.
;  *
;  * THIS SOFTWARE IS PROVIDED BY CARNEGIE MELLON UNIVERSITY ``AS IS'' AND 
;  * ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
;  * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;  * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY
;  * NOR ITS EMPLOYEES BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;  * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
;  * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
;  * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
;  * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
;  * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
;  * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;  *
;  * ====================================================================
;  *
;  */

;;; The author of this code is not particularly proud of it!
;;; Naturally, a kind of rule-based declarative specification
;;; mechanism should be invented.

; (defun listen-explicit-spacing (do-capitalization)
;   ;; The significance of do-capitalization has to be established :)
;   (or 
;    (and  
;     ;; Was the primary event that resulted in this call an
;     ;; listen-command?
;     (listen-command-event-p 
;      (listen-get-record-field 
;       (listen-primary-record listen-last-index)
;       event))
;     ;; And, was it marked as a 'nofix one?
;     (assq 'nofix 
; 	  (get 'alist 
; 	       (listen-lookup-command 
; 		listen-event-command-words))))
;    ;; Or, is there an instruction not to fix in 
;    ;; minibuffer?
;    listen-no-special-fix-in-minibuffer))

(defun listen-explicit-spacing ()
  ;; The significance of do-capitalization has to be established :)
  (or 
   (and  
    ;; Was the primary event that resulted in the second-last
    ;; even call an
    ;; listen-command?
    (listen-command-event-p 
     (listen-get-record-field 
      (listen-primary-record listen-last-index)
      event))
    (let ((event
	  (listen-get-record-field
	   (listen-primary-record 
	    (mod (1- (listen-primary-record listen-last-index))
		 listen-recent-records-size))
	   event)))
      (and event
	   (listen-event-p event)
	   ;; And, was it marked as a 'nofix one?
	   (assq 'nofix 
		 (listen-event-alist event)))))
   ;; Or, is there an instruction not to fix in 
   ;; minibuffer?
   listen-no-special-fix-in-minibuffer))

(defun listen-count-math-mode-dollar-signs (begin end) 
  (save-excursion
    (goto-char begin)
    (let ((cnt 0))
      (while (< (point) end)
	(if (and (not (member major-mode '(latex-mode tex-mode)))
		 (looking-at 
		  "[\\]?[$][0-9,.-]*[^$][\t\n]\\|[$][({]\\w*[)}][\t\n ]"))
	    ;; then some amount designation or maybe a variable in
	    ;; some programming language
	    (goto-char (match-end 0))
	  (if (looking-at "[\\][$]")
	      ;; that's a quoted dollar sign in latex, don't count
	      (goto-char (match-end 0))
	    ;; to count or not to count
	    (if (looking-at "[$]")
		;; we count that one
		(incf cnt))
	    (forward-char))))
      cnt)))

(defun listen-fix (pos &optional consider-explicit-spacing
		       old-stuff do-capitalization delete-mode)
  (interactive "d")
  (save-excursion 
    (goto-char pos)
    (let ((case-fold-search nil)
	  ;; this boundary bol is important if a field precedes pos
	  ;; (on the same line)
	  (bol    
	   (save-excursion
	     (beginning-of-line) (point)))
	  (fix-marker (point-marker)))
      ;; 
      (let ((old-stuff-marker 
	     (if old-stuff
		 (save-excursion (goto-char old-stuff)
				 (point-marker)))))

	(catch 'done 
	  (if consider-explicit-spacing
	      (if (listen-explicit-spacing)
		  (throw 'done nil)))
	  ;; do heuristics
	  ;;
	  (goto-char pos)
	  ;; if only spaces (2 or more) before point on line, don't do anything
	  (if (save-excursion (re-search-backward "^[ \t][ \t][ \t]*\\="
						  bol t))
	      (throw 'done 't))
	  (when 
	      ;; if "," or ":" before pos (but only at most a digit to the
	      ;; left or to the right of it, not both)
	      (and (or (re-search-backward "[^0-9:][,:;][ ]?\\=" bol  t)
		       (and (progn (goto-char pos)
				   (re-search-backward "[^:][,:;][ ]?\\="  bol t))
			    (progn (goto-char pos) (not (looking-at
							 "\\s-*[0-9]")))))
		   ;; there is something that is a word constituent
		   (progn (goto-char pos) (looking-at "[ \t]*\\w")))
	    ;; insert one space
	    (goto-char pos)
	    (delete-horizontal-space)
	    (insert-before-markers " ")
	    (throw 'done 't))
	  (goto-char pos) 
	  ;; require one space if certain punctuation signs follow point
	  (when (looking-at 
		 ;; before a short dash
		 "[ ]?[ ]?--[^-]")	    
	     (delete-horizontal-space)  
	     (insert-before-markers " ")
	     (throw 'done 't))
	  (goto-char pos)


	  ;; if embedded latex is possible, count $-signs
	  (goto-char pos)
	  (when (not (member major-mode '(rlogin-mode shell-mode)))
	    ;; if we are looking at a $ that doesn't seem to be the
	    ;; beginning of an amount designation, but could be the
	    ;; end of a math-mode expression
	    (when (looking-at "[ ]?[ ]?[$]")
	      (forward-paragraph -1)
	      (let ((p-pos (point)))
		(goto-char pos)
		(if (and (eq (mod 
			      (listen-count-math-mode-dollar-signs
			       p-pos pos)
			      2)
			     0)
			 ;; there is an even number of significant
			 ;; dollar signs
			 (save-excursion 
			   (re-search-backward "\\w" bol t)))
		    (just-one-space)
		  ;; an odd number
		  (delete-horizontal-space)))
	      (throw 'done 't))

	    ;; if after a $ 
	    (goto-char pos)
	    (when (re-search-backward "[$][ ]?[ ]?\\=" bol t)
	      (forward-paragraph -1)
	      (let ((p-pos (point)))
		(goto-char pos)
		(if (and (eq (mod 
			      (listen-count-math-mode-dollar-signs
			       p-pos pos) 
			      2)
			     0)
			 ;; there is an even number of significant
			 ;; dollar signs
			 (looking-at "[ \t]*\\w"))
		    (just-one-space)
		  ;; an odd number
		  (delete-horizontal-space)))
	      (throw 'done 't)))


	  ;; consider to delete space if things follow point
	  (when (looking-at 
		 (concat 
		  "[ \t]?[ \t]?[])},.;:!?]"
		  ;; a hyphen (or _) followed by a character or (
					; 		"\\|\\s-?[-_~][a-zA-Z{([/\\]"
		  "\\|[-_][a-zA-Z{(]"
		  ;; at then end of something quoted
		  "\\|\\s-?['\"]+\\([^a-zA-Z0-9([{]\\|$\\)"
		  ;; one character operator followed itself by no whitespace
		  "\\|[ ]?[ ]?[@^&*+=\-][^ !@#$%^&*+=|\-]"
		  ;; two-character operator followed itself by no whitespace
		  "\\|[ ]?[ ]?[!@#$%^&*+=|\][!@#$%^&*+=|\-]\\S-"
		  ;; a ---
		  "\\|[ ]?[ ]?---"))
	    (delete-horizontal-space) 
	    (throw 'done 't))

	  ;; before ">" or "/>" in XML tag
	  (when (and (looking-at "\\s-*[/]?[>]")
		     (re-search-backward "[<]\\w+.*\\=" bol t))
	    (goto-char pos)
	    (delete-horizontal-space) 
	    (throw 'done 't))
	  (goto-char pos) 
	  ;; before XML end tag
	  (if (looking-at 
	       "[ \t]?[ \t]?[< ]/[a-zA-Z_]")
	      (progn
		(delete-horizontal-space) 
		(goto-char pos)
		(throw 'done 't)))	


	  ;; check whether we are after a period (or ?!) that
	  ;; separates two sentences
	  (goto-char pos)
	  (if (and (not (member major-mode '(rlogin-mode shell-mode)))
		   (or;; if inside a comment then OK to
		    ;; fix dot as period
		    (not (member major-mode '(python-mode 
					      c-mode c++-mode
					      java-mode
					      elisp-mode
					      elisp-interaction-mode)))
		    (save-excursion
		      (beginning-of-line)
		      (looking-at comment-start-skip)))
		   (re-search-backward
		    "[.?!]['\"]?[])}]?\\([ \t\n]*\\)[(]?\\=" (- pos 20) t))
	      (let ((punctuation-pos (point))
		    (space-pos (match-beginning 1)))
		;; skip backwards over "." and word constituents
		(while (and (not (bolp))
			    (string-match 
			     "[.@]\\|\\w"
			     (buffer-substring (1- (point)) (point))))
		  (forward-char -1))
		(let ((before (buffer-substring 
			       (point)
			       punctuation-pos)))
		  (goto-char punctuation-pos)
		  (if (or (/= (char-after punctuation-pos) ?.)
			  ;; if not looking at something that has
			  ;; another "." in it, in addition to a "."
			  ;; at the end (because it could be an
			  ;; abbreviation, which are no treated as
			  ;; periods, or an ellipsis)---similar story
			  ;; for @
			  (and (not (string-match
				     "^\\w*[@.]\\(\\w\\|[@.]\\)*$"
				     before)) 
			       ;; not looking at a point in number (IP,
			       ;; real, hex)
			       (not (and (string-match
					  "^[0-9a-f.]+$" before)
					 (looking-at "[.]\\s-*[0-9a-f]*\\>")))))
		    
		      (progn 
			;; we have to figure out whether the period is on
			;; the same line as pos or was the last (modulo
			;; space) character on the preceding line
			(goto-char pos)
			(skip-syntax-forward "-")
			(assert (re-search-backward "^.*" nil t))
			(goto-char pos) 
			(if (string-match  
			     ;; test modulo (, {, whatever 
			     "\\(^ *$\\)\\|\\(^[ ][^.]+$\\)"
			     (buffer-substring (match-beginning 0)
					       (match-end 0)))
			  
			    (cond ((match-beginning 1)
				   ;; then just a single space on line
				   ;; after "."
				   (delete-horizontal-space))
				  ((match-beginning 2)
				   ;; then we are at the beginning of
				   ;; the line, modulo (,[,... and space
				   ;; stuff
				   nil))
			  ;; else, basically, we are in the middle of
			  ;; line, make two spaces here if not at end of
			  ;; line
			  (when (not (looking-at "[ \t]*$"))
			    (goto-char space-pos)
			    (skip-syntax-forward "-") 
			    (delete-region space-pos (point))
			    (if (save-excursion 
				  (re-search-backward 
				   (concat listen-mr-mrs-etc
					   "[ ]?\\=") nil t))
				(insert-before-markers " ")
			      (insert-before-markers "  "))))
			(goto-char pos)
			(when (and (looking-at "[ \t\n(]*\\w") 
				   (or (not (member major-mode
						    '(c-mode c++-mode
							     java-mode)))
				       ;; so, c-modes imply:
				       ;; (this code is insufficient for
				       ;; comments of the form /*...*/)
				       (save-excursion
					 (beginning-of-line)
					 (looking-at c-comment-start-regexp)))
				   (or (not (member major-mode
						    '(emacs-lisp-mode 
						      lisp-mode)))
				       ;; so, lisp-mode, etc: only if we
				       ;; are below a line beginning
				       ;; with a comment, do the
				       ;; capitalization
				       (save-excursion
					 (beginning-of-line)
					 (looking-at "[ \t]*\\s<"))))
			  (when (looking-at "[ \t\n(]*[a-z]")
			    (save-excursion 
			      (capitalize-word 1)))
			  (when old-stuff
			    (save-excursion
			      (goto-char old-stuff-marker)
			      (if (looking-at "\\s-*[A-Z]\\(\\W\\|[a-z]+\\W\\)")
				  (if (looking-at 
				       (concat
					"\\s-*\\("
					listen-mr-mrs-etc
					"\\|I\\)[ \n\t]"))
				      nil
				    (downcase-word 1))))))
			(throw 'done 't)))
		  ;; we have an abbreviation, ellipsis, ...
		  nil)))

	  ;; we are not between sentences; should we delete all white
	  ;; spaces according to what is before and after?
	  (goto-char pos)

	  ;; don't put space in front of "([" in certain mode
	  (if (member major-mode '(python-mode c-mode c++-mode
					       java-mode shell-script-mode))
	      (if (looking-at " *[[(]") 
		  (when
		      (or
		       (and (member major-mode '(c-mode c++-mode java-mode))
			    (not
			     (or 
			      (save-excursion;; look for //
				(re-search-backward 
				 "[ \t]*[/][/].*\\=" nil t))
			      (save-excursion;; look for /* without
				;; following */
				(re-search-backward 
				 "/[*]\\([^*]\\|[*][^/]\\)*\\=" nil t))))
			    (not 
			     (save-excursion;; look for common keywords
			       (re-search-backward 
				"\\<\\(return\\|for\\|switch\\|if\\|while\\)\ *\\=" nil t))))
		       (and (member major-mode '(python-mode
						 shell-script-mode))
			    (not
			     (save-excursion
			       (re-search-backward "[#].*\\=" nil
						   t)))))
		    (delete-horizontal-space) 
		    (throw 'done 't))))

	  (let ((buffer-read-only nil)
		;; for some reason, don't mess around with undo list
		(undo-list buffer-undo-list))
	    ;; very important, it is not always the case that an
	    ;; operation adds new element to the buffer-undo-list
	    ;; (instead, for example, a previous interval is augmented)
	    ;; -- force that to be the case
	    (undo-boundary)	
	    ;; we want to test the neighborhood around point, but
	    ;; unfortunately re-search-backward does not allow us to do
	    ;; so (neither does the forward search), so we use a trick
	    ;; of temporarily inserting a sequence of three characters,
	    ;; unlikely to be found in practice, that will denote point
	    (insert "!@#")
	    ;; move forward a little bit, then inspect backwards
	    (skip-chars-forward "^\n" (+ (point) 10))
	    (when (prog1
		      (re-search-backward
		       (concat
			;; whitespace before point and a , or ; etc. after
			"[^ ][ ]?[ ]!@#[,;]"
			;; a single whitespace before point and at beginning
			;; of line, or at beginning and a single
			;; whitespace after point
			"\\|^[ ]!@#\\|^!@#[ ]\\S-"
			;; a left parenthesis is the last thing before
			;; point (modulo spaces) 
			"\\|^.*[{[(][ ]?[ ]?!@#"
			;; a ~ or a \
			"\\|^.*[~\\]+[ ]?[ ]?!@#" 
			;;  a (some) ' or  " or `, which are preceded by
			;;  whitespace
			"\\|.*[ ]+['\"`]+[ ]?[ ]?!@#"
			;; one character operator preceeded itself by
			;; a symbol constituent
			"\\|.*\\(\\w\\|[_-]\\|^\\)['\"]?[~@^&*+=\\/-][ ]?[ ]?!@#" 
			;; two-character operator preceeded itself by no whitespace
			"\\|.*\\([^ ]\\|^\\)[~!@#$%^&*+=|\][~!@#$%^&*+=|\\/-][ ]?[ ]?!@#"
			;; a hyphenation character preceded by a string of alpha-numeric 
			;; hyphen characters 
			"\\|.*[a-zA-Z0-9]+[-][ ]?[ ]?!@#"
			;; underscore character
			"\\|.*[_][ ]?!@#"
			;; between a word and a hyphen or underscore or /
			"\\|.*[^ \t]+!@#[-_/]"		      
			;; three hyphenation characters, not two
			"\\|.*[^-][-][-][-][ ]?[ ]?!@#"
			;; a LaTeX command, followed by a LaTeX command
			"\\|.*[\\][a-zA-Z]+[ ]?[ ]?!@#[ ]?[ ]?[\\][a-zA-Z]"
			;; an non-word + $
			;;		"\\|\\B[$][ ]?[ ]?!@#"
			;; command line prompt, after $[ ]?!
			"\\|.*[$][ ]?[ ]?!!@#"
			;; a file name thing ~/ or ::
			"\\|.*\\([~][/\\]\\|[:][:]\\)[ ]*!@#"
			)
		       bol
		       t)
		    (goto-char pos)
		    (delete-char 3)
		    (setq buffer-undo-list undo-list))
	      (delete-horizontal-space)
	      (throw 'done 't)))

	  (goto-char pos)
	  ;; else we look at right parentheses and quotes before pos
	  ;; to see whether we should insist on exactly one white
	  ;; space
	  (if (string-match
	       (concat
		;; a right parenthesis is the last thing before
		;; point (modulo spaces) 
		"\\(^.*[])}][ \t]*$\\)"
		;; end tag
		"\\|^.*</[a-zA-Z0-9_-]+>[ \t]*$"
		;; after a word (followed perhaps by a . in which
		;; case it's an abbreviation one should think
		"\\|\\(^.*\\([-_a-zA-Z0-9][.]?\\)+[ \t]*$\\)"
		;; after an operator (or dash)
		"\\|\\(^.*[-+=!@#$%^&/\\* ]?[-+=!@#$%^&*/\\][
\t]*$\\)"
		;; after word followed by a possible comma and
		;; parentheses and quote
		"\\|^.*[-_a-zA-Z0-9]+[,.]?[]})]?['\"`]['`]?[ \t]*$"
		)
	       (buffer-substring bol pos))
	      (if (eolp)
		  ;; don't care about trailing spaces on a line
		  nil
		(when (or (not delete-mode) (and delete-mode (looking-at
							      " ")))
		  (just-one-space)
		  (skip-chars-backward " ")))))
	(if old-stuff-marker
	    (set-marker old-stuff-marker nil)))
      
      (if (eq major-mode 'emacs-lisp-mode)
	  (lisp-indent-line))
      (goto-char fix-marker)
      (set-marker fix-marker nil))))


(provide 'listen-fix)






