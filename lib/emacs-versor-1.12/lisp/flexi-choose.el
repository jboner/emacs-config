;;;; flexi-choose.el -- choose from a list, using pedals or similar
;;; Time-stamp: <2007-12-31 20:38:16 jcgs>

;; Copyright (C) 2007, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: around 2001
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; if this is loaded before we are, put it into our menu

;;; Commentary:
;; 

(require 'languide)
(require 'cl)

;;; Code:
(defun read-from-minibuffer-with-kill-ring (prompt)
  "Read a string from the minibuffer, using PROMPT, with `kill-ring' as history."
  (read-from-minibuffer prompt
			nil		; initial-contents
			nil		; keymap
			nil		; read
			'kill-ring))

(defvar in-completing-read-with-history-hack nil
  "Non-nil when in `completing-read-with-history-hack'.
This is used by number commands in my voice software,
to decide whether to throw out to this package, or
alternatively to set the prefix arg.
It is also used to see whether to do some clever things
with completion, subverting it to do filtering of choices.")
			
(defun completing-read-with-history-hack (prompt
					  history-var default
					  choices-list &optional choices-alist extras)
  "With PROMPT and using HISTORY-VAR and DEFAULT, choose from CHOICES-LIST.
This function has to construct an alist internally; if you have
one ready, you can give it as optional argument CHOICES-ALIST.  Or
if you only have the alist ready, you can give that, passing in
nil for choices-list, which it will then construct for you.  You
can give some extra possibilities for completion with the
optional argument EXTRAS.  For example, in the hierarchical
chooser, this is used to pass in the original choices along with
the hierarchical level chunks.  It should be an alist."
  (if (and choices-list (null choices-alist))
      (setq choices-alist (mapcar 'list choices-list))
    (when (and choices-alist (null choices-list))
      (setq choices-list (mapcar 'car choices-alist))))
  (let ((reverse-choices (reverse choices-list)))
    ;; use two copies, so we can start either direction
    (set history-var (append reverse-choices reverse-choices)))
  ;; (message "Was given choices %S, set history var to %S" choices-list (symbol-value history-var))
  (when (null default) (setq default (car choices-list)))
  (let ((starting (1+ (position default (symbol-value history-var) :test 'string=)))
	(completion-ignore-case t))
    ;; (message "Starting position for %S is %d" default starting)
    (let ((in-completing-read-with-history-hack t))
      (completing-read prompt
		       (append choices-alist extras)
		       nil		; predicate
		       t		; require-match
		       default		; initial
		       (cons history-var starting)))))

(defvar choices-hack-history nil
  "A history hack variable.")

(defvar choose-singletons-silently t
  "*If non-nil, `choose-using-history' will, when given a single choice, skip asking the user.")

(defun choose-by-number (n)
  "In the context of `choose-using-history', select the Nth item."
  (interactive "p")
  (message "Choosing %d by number from %S" n choices-hack-history)
  (throw 'chosen (nth (- (length choices-hack-history) n 1) choices-hack-history)))

(defun choose-using-history (prompt choices &optional helpstring extras)
  "With PROMPT, get the user to choose one of CHOICES.
This uses the minibuffer with a history hack like that done in the
tmm package.
If optional HELPSTRING is given, pop that up in a temporary buffer.
You can give some extra possibilities for completion with the
optional argument EXTRAS.  For example, in the hierarchical
chooser, this is used to pass in the original choices along with
the hierarchical level chunks.  It should be an alist."
  (if (and choose-singletons-silently
	   (or
	    (null (cdr choices))
	    (string= (cadr choices) flexi-choose-upstring)))
      (car choices)
    (save-window-excursion
      (when (and choices-display-full helpstring)
	(with-output-to-temp-buffer " *Choices*"
	  (princ helpstring)
	  ;; (shrink-window-if-larger-than-buffer (get-buffer-window (get-buffer " *Choices*")))
	  ))
      (let ((result
	     (catch 'chosen
	       (completing-read-with-history-hack prompt 'choices-hack-history
						  nil
						  choices
						  nil
						  extras))))
	;; (message "Chosen %S" result)
	result))))

(defun choose-value-using-history (prompt choices)
  "With PROMPT, get the user to choose one of CHOICES, which is an alist of strings to values.
This uses the minibuffer with a history hack like that done in the
tmm package."
  (let ((string (completing-read-with-history-hack prompt 'choices-hack-history
						   choices
						   nil
						   nil)))
    (cdr (assoc string choices))))

(defun make-chunk-desc (things)
  "Make a chunk for make-chunk-list."
  (cons (if (null (cdr things))
	    (car things)
	  (format "%s .. %s" (first things) (car (last things))))
	things))

(defun make-full-chunk-descr (things)
  "Make a long description of THINGS for the popup display"
  (let* ((full (mapconcat 'identity (cdr things) ", "))
	 (fulllen (length full))
	 (fw (- (frame-width) 8)))
    (cond
     ((< fulllen fw)
      ;; all fits on one line
      full)
     ((> fulllen (* fw 2))
      ;; more than two lines-worth, use continuation lines
      (concat (substring full 0 fw)
	      " ...\n   ... "
	      (substring full (- fw))))
     (t
      (let ((fulllby2 (/ fulllen 2)))
	(concat (substring full 0 fulllby2)
		"\n    "
		(substring full (- fulllby2))))))))

(defun make-chunk-list (single n)
  "Make a SINGLE list into an alist of N chunks, each labelled with a summary of its contents.
This makes most sense if SINGLE is sorted."
  (let* ((total (length single))
	 (per-chunk (+ (/ total n)
		       ;; compensate for the rounding-down by the integer division above
		       (if (zerop (mod total n)) 0 1)))
	 (chunks nil)
	 (i 0))
    ;; (message "%d total, splitting into %d chunks of %d each which totals %d too many" total n per-chunk (- (* n per-chunk) total))
    (catch 'done
      (while (< i n)
	(push (make-chunk-desc (subseq single 0
				       (min per-chunk (length single)))) chunks)
	;; (when (<= (length single) (- n i)) (setq per-chunk 1))
	(setq single (nthcdr per-chunk single))
	(when (null single) (throw 'done t))
	(incf i)))
    (when (< (* n per-chunk) total)
      (message "make-chunk-desc used overflow!")
      (push (make-chunk-desc single) chunks))
    (nreverse chunks)))

(defvar choices-per-step 6
  "*The number of possible choices per choice step for treewise choosers.")

(defun number-items (string-list)
  "Prepend a number to the items of STRING-LIST."
  (let ((i 0)
	(results nil))
    (dolist (string string-list)
      (push (format "%d: %s" i string) results)
      (incf i))
    (nreverse results)))

(defun filter-choices (choices filters)
  "Return just those of CHOICES that match all of FILTERS."
  (let ((result nil))
    (while choices
      (let ((element (car choices))
	    (fs filters)
	    )
	(while (and fs (not (eq fs t)))
	  (setq fs (if (string-match (car fs) element)
		       (cdr fs)
		     t)))
	(if (null fs)
	    (setq result (cons element result)))
	(setq choices (cdr choices))))
    result))

(defun minibuffer-blank ()
  "Erase the minibuffer, if using completion hacks"
  (interactive)
  (when in-completing-read-with-history-hack
    (if (eq last-command 'minibuffer-blank)
	(minibuffer-completion-help)
      (erase-buffer))))

(defun voice-seems-active ()
  "Return whether we seem to have a voice system, and it is on."
  (and (boundp 'vr-mode)
       vr-mode
       (string= vr-mic-state "on")))

(defun words-present-in-strings (strings)
  "Return the words present in STRINGS."
  (require 'cl)
  (let ((result nil))
    (while strings
      (let ((words (split-string (car strings) "[-.*_ <>]")))
	(while words
	  (let ((word (downcase (car words))))
	    (unless (member word result)
	      (setq result (cons word result))))
	  (setq words (cdr words))))
      (setq strings (cdr strings)))
    result))

(defun choose-step-size (n)
  "Return a chunk size for choosing amongst N choices."
  (let* ((max-per-step (if (voice-seems-active)
			   (if (and (boundp 'vr-choices-per-step)
				    (integerp vr-choices-per-step))
			       vr-choices-per-step
			     (min (- (/ (frame-height) 2) 2)
				  vr-choices-per-step-limit))
			 choices-per-step))
	 (per-step
	  (cond
	   ((<= n max-per-step) n)
	   ((<= n (* max-per-step max-per-step)) (ceiling (sqrt n)))
	   ((<= n (* max-per-step max-per-step max-per-step)) (ceiling (expt (float n) (/ 1.0 3.0))))
	   (t max-per-step))))
    per-step))

(defun choose-in-steps (prompt choices &optional display-modifier)
  "With PROMPT, get the user to choose one of CHOICES.
At each stage, up to choices-per-step choices are presented,
and are displayed in the help buffer.
If the optional extra argument DISPLAY-MODIFIER is given, apply it to
each line of the help buffer if this is the last step, that is, if
the choices are leaf choices."
  (let* ((all-choices choices)
	 (interesting-words (if (voice-seems-active)
				;; expensive to compute; used only by voice
				(words-present-in-strings all-choices)
			      nil))
	 (n (length choices))
	 (per-step (choose-step-size n))
	 (previous-levels nil)
	 (extras nil)
	 (result nil)
	 (originals-alist (mapcar 'list choices)))
    (while (null result)
      (let* ((last-step (<= n per-step))
	     (chunks (make-chunk-list choices per-step))
	     ;; if 'filter is thrown a cons, it is a list of words to filter by
	     (stepkey (catch 'filter
			(choose-using-history
			 prompt
			 ;; the choices:
			 (append (if last-step
				     (copy-list choices)
				   (mapcar 'car chunks))
				 extras)
			 ;; the text to display in a pop-up buffer
			 (mapconcat 'identity
				    (append (number-items
					     (if last-step
						 (if display-modifier
						     (let ((max-choice-length
							    (apply 'max (mapcar 'length choices))))
						       (mapcar display-modifier choices))
						   choices)
					       (mapcar 'make-full-chunk-descr chunks)))
					    extras)
				    "\n")
			 originals-alist))))
	(cond
	 ((stringp stepkey)
	  (cond
	   ((member stepkey choices)
	    (setq result stepkey))
	   ((string= stepkey flexi-choose-upstring)
	    (setq choices (car previous-levels)
		  n (length choices)
		  previous-levels (cdr previous-levels)))
	   ((string= stepkey flexi-choose-topstring)
	    (setq choices all-choices
		  n (length choices)
		  previous-levels nil))
	   (t
	    (if last-step
		(setq result stepkey)
	      (let* ((stepval (cdr (assoc stepkey chunks))))
		(setq previous-levels (cons choices previous-levels)
		      choices stepval
		      n (length choices)))))))
	 ((consp stepkey)
	  (let ((filtered (filter-choices choices stepkey)))
	    (setq previous-levels (cons choices previous-levels)
		  choices (if filtered filtered choices)
		  n (length choices)))
	  ))
	(if previous-levels
	    (setq extras (list flexi-choose-upstring flexi-choose-topstring)))))
    result))

(defun make-chunk-tree (single n)
  "Make a tree of chunk lists from SINGLE."
  (message "make-chunk-tree to %d per level, on list of %d in total" n (length single))
  ;; nesting wrong below, no conditional should be outside the call to make-chunk-list
  (let ((total (length single)))
    (if (> total n)
	(let ((tree (make-chunk-list single (/ total n))))
	  (if (> (length tree) n)
	      (dolist (subtree tree)
		(rplacd subtree (make-chunk-tree (cdr subtree) n))
		))
	  tree)
      (mapcar (function
	       (lambda (item)
		 (cons item item)))
	      single))))

(defun chooseable-buffers (limit)
  "return a list of buffers eligible for being chosen."
  (let ((buffers (buffer-list)))
    (if (and (integerp limit)
	     (> (length buffers)
		limit))
	(setq buffers (subseq buffers 0 limit)))
    (delete-if (function
		(lambda (str)
		  (string-match "^ " str)))
	       (mapcar 'buffer-name buffers))))

(defun choose-buffer (prompt &optional limit sort)
  "Choose a buffer, with PROMPT.
With optional LIMIT argument, only looking recently used buffers."
  (let ((buffers (chooseable-buffers limit)))
    (choose-in-steps prompt
		     (if sort
			 (sort buffers 'string<)
		       buffers))))

(defun choose-file-maybe-directory (prompt starting-dir)
  "With PROMPT, choose a file starting from STARTING-DIR."
  (unless (file-directory-p starting-dir) (setq starting-dir (file-name-directory starting-dir)))
  (expand-file-name
   (choose-in-steps prompt (directory-files starting-dir))
   starting-dir))

(defun choose-file (prompt starting-dir)
  "With PROMPT, choose a file starting from STARTING-DIR."
  (let ((file (choose-file-maybe-directory prompt starting-dir)))
    (while (file-directory-p file)
      (setq file (choose-file-maybe-directory prompt file)))
    file))

(defun choose-library (prompt)
  "Flexi selection of files on emacs load-path."
  (let ((elibs (el-lib-list)))
    (choose-in-steps prompt
		     (sort (mapcar 'car elibs)
			   'string-lessp)
		     (function
		      (lambda (choice)
			(format (format "%%%ds: %%s" max-choice-length)
				choice
				(cdr (assoc choice elibs))))))))

(defun flexi-choose-library (library)
  "Find an elisp file on your path."
  (interactive (list (choose-library "Find library file: ")))
  (find-file (elisp-file-more-readable-version (cdr (assoc library (el-lib-list))))))

(defun choose-paper (prompt)
  "Flexi selection of paper."
  (choose-in-steps prompt (sort (mapcar 'car (papers-list))
				'string-lessp)))

(defun flexi-choose-paper (paper)
  "Find PAPER."
  (interactive (list (choose-paper "Paper: ")))
  (find-paper paper))

(defun flexi-switch-to-buffer (buffer)
  "Switch buffers"
  (interactive (list (choose-buffer "Switch to buffer: " nil t)))
  (switch-to-buffer buffer))

(defun flexi-switch-to-recent-buffer (buffer)
  "Switch buffers"
  (interactive (list (choose-buffer "Switch to buffer: "
				    (if (and (boundp 'vr-choices-per-step-limit)
					     (integerp vr-choices-per-step-limit))
					vr-choices-per-step-limit
				      choices-per-step)
				    t)))
  (switch-to-buffer buffer))

(defun flexi-find-file (file)
  "Find file"
  (interactive (list (choose-file "Find file: " default-directory)))
  (find-file file))

(defun flexi-choose-email-alias (prompt)
  "With PROMPT, chooose an email alias."
  (when (eq mail-aliases t)
    (setq mail-aliases nil)
    (when (file-exists-p mail-personal-alias-file)
      (build-mail-aliases)))
  (choose-in-steps prompt (sort (remove-if (function
					    (lambda (name)
					      (string-match "-part[0-9]+" name)))
					   (mapcar 'car
						   mail-aliases))
				'string<)))

(defun flexi-insert-email-alias ()
  "Choose and insert an email alias."
  (interactive)
  (let* ((alias (flexi-choose-email-alias "Mail alias: "))
	 (expansion (cdr (assoc alias mail-aliases))))
    (message "%s expands to %s" alias expansion)
    (sit-for 1)
    (insert alias)))

(defun flexi-read-email-addresses (to-prompt &optional just-one)
  "Read email aliases using the flexi system. Return cons of to-list and cc-list"
  (let* ((recipients (list (flexi-choose-email-alias to-prompt)))
	 (expansion (cdr (assoc (car recipients) mail-aliases)))
	 (cc nil)
	 (ccq "CC? "))
    (message "%s expands to %s" (car recipients) expansion)
    (sit-for 1)
    (unless just-one
      (while (yes-or-no-p "Further \"to\" recipients? ")
	(let ((this (flexi-choose-email-alias "Also to: ")))
	  (push this recipients)
	  (message "%s expands to %s" this (cdr (assoc this mail-aliases)))))
      (while (yes-or-no-p ccq)
	(let ((this (flexi-choose-email-alias "CC to: ")))
	  (push this cc)
	  (message "%s expands to %s" this (cdr (assoc this mail-aliases)))
	  (setq ccq "Further CC? "))))
    (cons (nreverse recipients)
	  (nreverse cc))))

(defun flexi-mail-to (&optional several)
  "Choose a mail alias, and start a message to them."
  (interactive "P")
  (let* ((both-lists (flexi-read-email-addresses  "Compose message to: " (not several)))
	 (recipients (car both-lists))
	 (cc (cdr both-lists))
	 (recipient-string (mapconcat 'identity recipients ", "))
	 (cc-string (if cc (mapconcat 'identity cc ", ") ; cc
		      nil))
	 (mail-buffer-name (generate-new-buffer-name
			    (format "mail to %s" recipient-string))))
    (mail nil				; noerase
	  recipient-string		; to
	  nil				; subject
	  nil				; in-reply-to
	  cc-string)
    (rename-buffer mail-buffer-name))
  (mail-subject))

(defun flexi-vm-forward (&optional several)
  "Forward a message, using the flexi system to read addresses."
  (interactive "P")
  (let* ((both-lists (flexi-read-email-addresses "Forward message to: " (not several)))
	 (cc (cdr both-lists)))
    (vm-forward-message)
    (mail-to) (insert (mapconcat 'identity (car both-lists) ", "))
    (when cc
      (mail-cc) (insert (mapconcat 'identity cc ", ")))))

(defun flexi-choose-web-directory (prompt)
  "Get the user to select a web directory"
  (choose-in-steps prompt (sort (mapcar 'car (web-directories)) 'string<)))

(defun flexi-raise-web-directory ()
  "Choose a web directory, and bring it to the top of the buffer list."
  (interactive)
  (let* ((web-dir (flexi-choose-web-directory "Raise web directory: ")))
    (raise-web-directory web-dir)))

(defun flexi-choose-context (prompt)
  "Get the user to select a context"
  (choose-in-steps prompt (sort (mapcar 'car context-contexts) 'string<)))

(defun flexi-load-context ()
  "Choose a context, and bring it to the top of the buffer list."
  (interactive)
  (require 'contexts)
  (let* ((context (flexi-choose-context "Switch to context: ")))
    (context-load context)))

(defun flexi-choose-screen-setup ()
  "Get the user to select a screen setup."
  (choose-in-steps "Screen setup: " (sort (mapcar 'car screen-setups) 'string<)))

(defun flexi-select-screen-setup ()
  (interactive)
  (require 'screen-setups)
  (use-screen-setup (flexi-choose-screen-setup)))

(pushnew 'flexi-choose-menu menu-bar-final-items)
(defvar flexi-choose-menu (make-sparse-keymap "Flexi choose"))
(fset 'flexi-choose-menu flexi-choose-menu)
(define-key global-map [menu-bar flexi-choose] '(menu-item "Flexi choose" flexi-choose-menu))

(define-key flexi-choose-menu [find-file]
  '("Find File" . flexi-find-file))

(define-key flexi-choose-menu [send-message]
  '("Send message" . flexi-mail-to))

(define-key flexi-choose-menu [raise-web-directory]
  '("Raise web directory" . flexi-raise-web-directory))

(define-key flexi-choose-menu [select-screen-setup]
  '("Select screen setup" . flexi-select-screen-setup))

(if (featurep 'sensible-languages)
    ;; todo: find whether this is really part of languide
    (define-key flexi-choose-menu [statement-type]
      '("Statement type" . statement-type)))

(define-key flexi-choose-menu [flexi-choose-paper]
  '("Find paper" . flexi-choose-paper))

(define-key flexi-choose-menu [flexi-load-context]
  '("Load context" . flexi-load-context))

(define-key flexi-choose-menu [ flexi-choose-library ]
  '("Find library file" . flexi-choose-library))

(define-key flexi-choose-menu [switch-to-buffer]
  '("Switch to buffer" . flexi-switch-to-buffer))

;; I don't find this one useful, although it seemed like a good idea at first
;;(define-key flexi-choose-menu [switch-to-recent-buffer]
;;  '("Switch to recent buffer" . flexi-switch-to-recent-buffer))

(provide 'flexi-choose)

;;;; end of flexi-choose.el

(provide 'flexi-choose)

;;; flexi-choose.el ends here
