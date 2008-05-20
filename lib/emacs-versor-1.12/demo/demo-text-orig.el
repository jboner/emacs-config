;;;; demo-text.el -- elisp demo text for versor
;;; Time-stamp: <2006-05-09 16:36:47 john>

(versor-demo) ;; This is really a data file. Divert to the demo program if the user tries loading this file.

(defun find-next-lisp-binding-outwards (&optional allow-conversions)
  "Move to the next enclosing binding.
Returns the position if it found one, or nil otherwise."
  (interactive)
  (let ((binding-pattern (if allow-conversions
			     "\\((let\\*?\\\\)\\|\\((progn\\)>"
			   "(let\\*?\\>")))
    (while (and (outward-once)
		(not (looking-at binding-pattern))))
    (if (looking-at binding-pattern)
	(point)
      nil)))

(defmodal variables-in-scope (lisp-mode emacs-lisp-mode lisp-interaction-mode) (whereat)
  "Return the alist list of variables in scope at WHEREAT."
  ;; todo: make this spot lambda bindings too
  (save-excursion
    (goto-char whereat)
    (beginning-of-defun)
    (let ((bod (point))
	  next
	  (parse-sexp-ignore-comments t)
	  (variables nil))
      (goto-char whereat)
      ;; first, check we are not in the preamble of a let form:
      (when (or (looking-at "let\\*?")
		(save-excursion
		  (and (safe-backward-sexp)
		       (looking-at "let\\*?"))))
	(safe-backward-up-list 1))
      ;; now, see whether we are in the bindings list of a let* form:
      (let ((possible-current-binding (save-excursion
					(safe-backward-up-list 1)
					(safe-backward-sexp 1)
					(and (looking-at "let\\*?")
					     (match-string-no-properties 0)))))
	(cond
	 ((string= "let*" possible-current-binding)
	  ;; (message "Got some of our own")
	  (while (setq next (safe-scan-sexps (point) -1))
	    (goto-char next)
	    (when (looking-at "(") (forward-char) (skip-to-actual-code))
	    (let ((start (point)))
	      (forward-sexp)
	      (push (list (buffer-substring-no-properties start (point)))
		    variables))
	    (goto-char next))
	  ;; avoid getting the same lot again
	  (safe-backward-up-list 2)
	  )
	 ((string= "let" possible-current-binding)
	  ;; (message "Avoiding this bunch")
	  ;; the bindings we are among are not in scope here, so get out from among them
	  (safe-backward-up-list 2)))
	)
      ;; (message "Looking for bindings between %d and %d" bod whereat)
      (while (find-next-lisp-binding-outwards)
	(save-excursion
	  (down-list 2)
	  (skip-to-actual-code)
	  ;; (message "Entering binding list got us to %d" (point))
	  (while (setq next (safe-scan-sexps (point) 1))
	    (skip-to-actual-code)
	    ;; (message "Looking at %s as binding" (buffer-substring-no-properties (point) next))
	    (when (looking-at "(") (forward-char) (skip-to-actual-code))
	    (let ((start (point)))
	      (forward-sexp)
	      (push (list (buffer-substring-no-properties start (point)))
		    variables))
	    (goto-char next))))
      ;; got the let-forms, now get the funargs
      (goto-char bod)
      (down-list)
      (when (looking-at "def[um]")
	(when (looking-at "defmod[ae]l") (forward-sexp))
	(forward-sexp 2)
	(let ((limit (safe-scan-sexps (point) 1)))
	  (down-list)
	  (skip-to-actual-code)
	  ;; (message "Arglist starts at %d" (point))
	  (let ((old (point))
		new)
	    (while (and (setq new (safe-scan-sexps old 1))
			(< new limit))
	      (push (list (buffer-substring-no-properties old new)) variables)
	      (goto-char new)
	      (skip-to-actual-code limit)
	      (setq old (point))))))
      variables)))

(defmodal adapt-binding-point (lisp-mode emacs-lisp-mode lisp-interaction-mode) (&optional allow-conversions)
  "Make a binding point suitable to receive another binding."
  (let ((binding-place (point)))
    (save-excursion
      (outward-once) 
      (if (looking-at "(defun")
	  (progn
	    (goto-char binding-place)
	    (languide-insert-before-markers "(let (")
	    (save-excursion ; so we finish at new variable insertion point
	      (languide-insert ")\n")
	      (let ((old (point))
		    new)
		(while (setq new (safe-scan-sexps old 1)) ; move over body forms
		  (setq old new))
		(goto-char old))
	      (languide-insert ")")
	      (backward-up-list)
	      (indent-sexp)))
	(outward-once)
	(cond
	 ((looking-at "\\((let\\)[^*]")
	  ;; todo: add further condition, that one of the variables needed is bound in this set?
	  (save-excursion
	    (replace-match "\\1*" t nil nil 1)))
	 ((and allow-conversions
	       (looking-at "(\\(progn))"))
	  (save-excursion
	    (replace-match "let* ()" t t nil 1)))))))) 

(defmodal languide-region-type (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (from to)
  "Try to work out what type of thing the code between FROM and TO is.
Results can be things like if-then-body, if-then-else-tail, progn-whole,
while-do-head, defun-body, and so on. If one of these is returned, the
code must be exactly that (apart from leading and trailing
whitespace).
If it is not recognizable as anything in particular, but ends at the
same depth as it starts, and never goes below that depth in between,
that is, is something that could be made into a compound statement or
expression, return t. 
Otherwise return nil.
May set languide-region-detail-string to a string giving the user incidental
information; otherwise should clear it to nil."
  (setq languide-region-detail-string nil)
  (cond
   ((save-excursion (goto-char from)
		    (skip-to-actual-code to)
		    (and (not (looking-at "("))
			 (save-excursion
			   (backward-char)
			   (skip-to-actual-code-backwards)
			   (backward-char)
			   (not (looking-at "(")))
			 (progn
			   (forward-sexp)
			   (let ((expr-1-end (point)))
			     (goto-char to)
			     (skip-to-actual-code-backwards from)
			     (eq (point) expr-1-end)))))
    (let* ((start-char (char-after from))
	   (start-syntax (char-syntax start-char)))
      (cond
       ((eq start-char 34)
	'string-literal)
       ((eq start-char ??)
	'character-literal)
       ((and (>= start-char ?0) (<= start-char ?9))
	'numeric-constant)
       ((or (eq start-syntax ?w)
	    (eq start-syntax ?_))
	'symbol))))
   (t (let* ((pps (save-excursion (parse-partial-sexp from to))))
	(cond
	 ((and (zerop (nth 0 pps))	; same level at both ends
	       (>= (nth 6 pps) 0))	; no dip in level between ends
	  (let* ((n-parts (count-sexps from to))
		 (f-start (safe-scan-lists from 1 -1))
		 (f-end (and f-start
			     (safe-scan-sexps f-start 1)))
		 (functor (and f-end
			       (intern
				(buffer-substring-no-properties f-start f-end))))
		 (surrounding-start (safe-scan-lists from -1 1))
		 (surrounding-end (and surrounding-start
				       (safe-scan-sexps surrounding-start 1)))
		 (sf-start (and surrounding-start
				(safe-scan-lists surrounding-start 1 -1)))
		 (sf-end (and sf-start
			      (safe-scan-sexps sf-start 1)))
		 (surrounding-functor (and sf-end
					   (not (= (char-after sf-start) open-bracket))
					   (intern
					    (buffer-substring-no-properties sf-start sf-end))))
		 (s-members (and sf-start
				 surrounding-end
				 (count-sexps sf-start (1- surrounding-end))))
		 (which-s-member (and sf-start
				      (count-sexps sf-start from))))
	    ;; (message "functor %S; surrounding-functor %S, of which we are %S of %S" functor surrounding-functor which-s-member s-members)
	    (cond
	     ((eq surrounding-functor nil)
	      ;; could be an individual let binding, or something like
	      ;; that, but is likely not to be a common or garden code sexp
	      (let* ((sursurrounding-start (safe-scan-lists surrounding-start -1 1))
		     (ssf-start (and sursurrounding-start
				     (safe-scan-lists sursurrounding-start 1 -1)))
		     (ssf-end (and sf-start
				   (safe-scan-sexps ssf-start 1)))
		     (sursurrounding-functor (and ssf-end
						  (not (= (char-after ssf-start) open-bracket))
						  (intern
						   (buffer-substring-no-properties ssf-start ssf-end)))))
		(cond
		 ((eq sursurrounding-functor 'cond)
		  (if (eq which-s-member 0)
		      'cond-condition
		    ;; (message "cond body which=%S n=%S members=%S" which-s-member n-parts s-members)
		    (if (and (numberp which-s-member)
			     (= which-s-member 2)
			     (= n-parts (- s-members 1)))
			(progn
			  (setq languide-region-detail-string (format "%d parts" n-parts))
			  'cond-body)
		      t)))
		 ((memq sursurrounding-functor '(let let*))
		  'variable-binding)
		 (t nil))))
	     ((eq which-s-member 0)
	      'functor)
	     ((eq surrounding-functor 'cond)
	      'cond-clause)
	     ((eq functor 'defun) defun-body)
	     ((memq surrounding-functor '(let let*))
	      (cond
	       ((eq which-s-member 2)
		(if (eq n-parts 1)
		    (let ((this-binding f-start)
			  (n-bindings 0))
		      (while (setq this-binding (safe-scan-sexps this-binding 1))
			(setq n-bindings (1+ n-bindings)))
		      (setq languide-region-detail-string (format "%d bindings" n-bindings))
		      'let-bindings)
		  nil))
	       ((and (= which-s-member 3)
		     (= n-parts (- s-members 2)))
		(progn
		  (setq languide-region-detail-string (format "%d parts" n-parts))
		  'let-body))
	       (t t)))
	     ;; todo: lots more to do here
	     ((memq functor '(progn save-excursion save-window-excursion)) 'progn-whole)
	     ((memq surrounding-functor '(when unless))
	      (if (eq which-s-member 2)
		  'if-condition
		(if (and (= which-s-member 3)
			 (= n-parts (- s-members 2)))
		    (progn
		      (setq languide-region-detail-string (format "%d parts" n-parts))
		      'if-then-body)
		  t)))
	     ((and (eq surrounding-functor 'if)
		   (numberp which-s-member))
	      (cond 
	       ((= which-s-member 2)
		(if (= n-parts 1)
		    'if-condition
		  nil))
	       ((= which-s-member 3)
		(if (= n-parts 1)
		    'if-body
		  nil))
	       ((and (= which-s-member 4)
		     (= n-parts (- s-members 3)))
		(progn
		  (setq languide-region-detail-string (format "%d parts" n-parts))
		  'if-then-else-tail))
	       (t t)))
	     (t t))))
	 (t nil))))))

(defmodal variable-bindings-in-region (lisp-mode emacs-lisp-mode lisp-interaction-mode) (from to)
  "Return a list of the bindings between FROM and TO.
Each element is a list of:
  name
  type
  scope-begins scope-ends
  initial-value-as-string"
  (save-excursion
    (let ((result nil))
      (goto-char from)
      (while (re-search-forward "\\((lambda\\>\\)\\|\\((let\\*?\\)" to t)
	(let* ((keystart (safe-scan-lists (match-beginning 0) 1 -1))
	       (keyend (safe-scan-sexps keystart 1))
	       (bindings-end (save-excursion
			       (forward-sexp 1)
			       (point)))
	       (scope-end (save-excursion
			    (backward-up-list)
			    (forward-sexp 1)
			    (1- (point)))))
	  (goto-char keystart)
	  (cond
	   ((looking-at "lambda")
	    (let* ((names-start (safe-scan-lists keyend 1 -1))
		   (names-end (1- (safe-scan-sexps (1- names-start) 1)))
		   (name-start names-start)
		   (name-end (safe-scan-sexps name-start 1)))
	      (while (and name-end (<= name-end names-end))
		(push (list (buffer-substring-no-properties name-start name-end)
			    nil
			    keyend scope-end
			    nil)
		      result)
		(goto-char name-end)
		(skip-to-actual-code)
		(setq name-start (point)
		      name-end (safe-scan-sexps name-start 1)))))
	   ((looking-at "let\\*?\\>")
	    (let ((star (looking-at "let\\*")))
	      (goto-char keyend)
	      (down-list)		; into the bindings
	      (let* ((binding-start (point))
		     (binding-end nil))
		(while (setq binding-end (safe-scan-sexps binding-start 1))
		  (goto-char binding-start)
		  (down-list)
		  (let* ((name-start (point))
			 (name-end (safe-scan-sexps name-start 1))
			 (name-string (buffer-substring-no-properties name-start name-end))
			 (value-end (safe-scan-sexps name-end 1))
			 (value-start (if value-end
					  (safe-scan-sexps value-end -1)
					nil))
			 (value-string (if (and value-start value-end)
					   (buffer-substring-no-properties value-start value-end)
					 nil)))
		    (push (list name-string
				nil
				(if star value-end bindings-end) scope-end
				value-string)
			  result))
		  (setq binding-start binding-end))))))
	  (goto-char bindings-end)))
      (nreverse result))))

;;; end of demo-text.el
