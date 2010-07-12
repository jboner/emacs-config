(require 'auto-complete)

(defun ensime-ac-move-point-back-to-call-target (prefix)
  "Assuming the point is in a member prefix, move the point back so it's
   at the last char of the call target.
  "
  (backward-char (length prefix))
  (re-search-backward "[^\\. ]" (point-at-bol) t))

(defun ensime-ac-member-candidates (prefix)
  "Return candidate list."
  (ensime-save-buffer-no-hooks)
  (save-excursion
    (ensime-ac-move-point-back-to-call-target prefix)
    (let ((members (ensime-rpc-members-for-type-at-point prefix)))
      (mapcar (lambda (m)
		(let* ((type-name (plist-get m :type-name))
		       (type-id (plist-get m :type-id))
		       (is-callable (plist-get m :is-callable))
		       (name (plist-get m :name))
		       (candidate (concat name ":" type-name)))
		  ;; Save the type for later display
		  (propertize candidate
			      'symbol-name name
			      'scala-type-name type-name 
			      'scala-type-id type-id
			      'is-callable is-callable
			      ))
		) members))))


(defun ensime-ac-completing-constructor-p (prefix)
  "Are we trying to complete a call of the form 'new [prefix]' ?"
  (save-excursion
    (goto-char (- (point) (length prefix)))
    (looking-back "new\\s-+" (ensime-pt-at-end-of-prev-line))
    ))

(defun ensime-ac-name-candidates (prefix)
  "Return candidate list."
  (ensime-save-buffer-no-hooks)
  (let* ((is-constructor (ensime-ac-completing-constructor-p prefix))
	 (names (ensime-rpc-name-completions-at-point 
		 prefix is-constructor)))
    (mapcar (lambda (m)
	      (let* ((type-name (plist-get m :type-name))
		     (type-id (plist-get m :type-id))
		     (is-callable (plist-get m :is-callable))
		     (name (plist-get m :name))
		     (candidate (concat name ":" type-name)))
		;; Save the type for later display
		(propertize candidate
			    'symbol-name name
			    'scala-type-name type-name 
			    'scala-type-id type-id
			    'is-callable is-callable
			    ))
	      ) names)))


(defun ensime-ac-get-doc (item)
  "Return doc for given item."
  (get-text-property 0 'scala-type-name item))

(defun ensime-ac-member-prefix ()
  "Starting at current point. Find the point of completion for a member access. 
   Return nil if we are not currently looking at a member access."
  (let ((point (re-search-backward "[\\. ]+\\([^\\. ]*\\)?" (point-at-bol) t)))
    (if point (1+ point))))

(defun ensime-pt-at-end-of-prev-line ()
  (save-excursion (forward-line -1)(point-at-eol)))

(defun ensime-ac-name-prefix ()
  "Starting at current point - find the point of completion for a symbol.
   Return nil if we are not currently looking at a symbol."
  (if (looking-back "[=(\\[\\,\\;\\}\\{\n]\\s-*\\(?:new\\)?\\s-*\\(\\w+\\)" (ensime-pt-at-end-of-prev-line))
      (let ((point (- (point) (length (match-string 1)))))
	(goto-char point)
	point
	)))

(defun ensime-ac-complete-action ()
  "Defines action to perform when user selects a completion candidate.
   In this case, if the candidate is a callable symbol, add the meta-info
   about the params and param types as text-properties of the completed name."

  (let* ((candidate candidate) ;;Grab from dynamic environment..
	 (name (get-text-property 0 'symbol-name candidate))
	 (type-id (get-text-property 0 'scala-type-id candidate)))
    (kill-backward-chars (length candidate))
    (let ((name-start-point (point)))
      (insert name)

      ;; If this member is callable, use the type-id to lookup call completion 
      ;; information to show parameter hints.
      (when (get-text-property 0 'is-callable candidate)

	(let* ((call-info (ensime-rpc-get-call-completion type-id))
	       (param-types (ensime-type-param-types call-info))
	       (param-names (ensime-type-param-names call-info)))
	  (when (and call-info param-names param-types)

	    ;; Save param info as a text properties of the member name..
	    (add-text-properties name-start-point 
				 (+ name-start-point (length name))
				 (list 'param-types param-types
				       'param-names param-names
				       ))

	    ;; Insert space or parens depending on the nature of the
	    ;; call
	    (save-excursion
	      (if (and (= 1 (length param-types))
		       (null (string-match "[A-z]" name)))
		  ;; Probably an operator..
		  (insert " ")
		;; Probably a normal method call
		(insert "()" )))

	    ;; Setup hook function to show param help later..
	    (add-hook 'post-command-hook 'ensime-ac-update-param-help nil t)

	    ;; This command should trigger help hook..
	    (forward-char)
	    ))))))


(defun ensime-ac-get-active-param-info ()
  "Search backward from point for the param info of the call that 
   we are currently completing."
  (save-excursion
    (catch 'return 
      (let ((lbound (point-at-bol)) ;; TODO <-- what about multiline param lists
	    (balance 0))
	(backward-char 1)
	(while (> (point) lbound)
	  (cond
	   ((ensime-in-string-or-comment (point)) nil)
	   ((looking-at "\\s)") (decf balance))
	   ((looking-at "\\s(") (incf balance))
	   (t
	    (let ((param-types (get-text-property (point) 'param-types))
		  (param-names (get-text-property (point) 'param-names)))
	      (if (and (or (> balance 0)) param-types)
		  (throw 'return (list 
				  :name-end-point (point)
				  :param-types param-types
				  :param-names param-names))))))
	  (backward-char 1))))))


(defun ensime-ac-update-param-help ()
  "When entering the arguments to a call, display a tooltip
   with the param names and types of the call."
  (let ((info (ensime-ac-get-active-param-info)))
    (if info
	(let* (;; To be used for tooltip positioning..
	       (name-end (plist-get info :name-end-point))

	       (param-types (plist-get info :param-types))
	       (param-names (plist-get info :param-names))
	       (i -1)
	       (param-str (mapconcat
			   (lambda (pt)
			     (incf i)
			     (format 
			      "%s:%s" 
			      (propertize (nth i param-names) 'face font-lock-variable-name-face)
			      (propertize (ensime-type-name pt) 'face font-lock-type-face)
			      ))
			   param-types ", ")))
	  (message (concat "( " param-str " )")))
      (remove-hook 'post-command-hook 'ensime-ac-update-param-help t))))


(ac-define-source ensime-members
  '((document . ensime-ac-get-doc)
    (candidates . (ensime-ac-member-candidates ac-prefix))
    (prefix . ensime-ac-member-prefix)
    (action . ensime-ac-complete-action)
    (requires . 0)
    (symbol . "f")
    (cache . t)
    ))

(ac-define-source ensime-scope-names
  '((document . ensime-ac-get-doc)
    (candidates . (ensime-ac-name-candidates ac-prefix))
    (prefix . ensime-ac-name-prefix)
    (action . ensime-ac-complete-action)
    (requires . 0)
    (symbol . "s")
    (cache . t)
    ))

(defun ensime-ac-enable ()
  (make-local-variable 'ac-sources)

  ;; Note, we try to complete names before members.
  ;; This simplifies the regexes.
  (setq ac-sources '(ac-source-ensime-scope-names
		     ac-source-ensime-members ))

  (make-local-variable 'ac-quick-help-delay)
  (setq ac-quick-help-delay 1.0)

  (make-local-variable 'ac-auto-start)
  (setq ac-auto-start nil)

  (make-local-variable 'ac-expand-on-auto-complete)
  (setq ac-expand-on-auto-complete nil)

  (make-local-variable 'ac-use-fuzzy)
  (setq ac-use-fuzzy nil)

  (make-local-variable 'ac-trigger-key)
  (ac-set-trigger-key "TAB")

  (auto-complete-mode 1)
  )

(defun ensime-ac-disable ()
  (auto-complete-mode 0)
  )

(provide 'ensime-auto-complete)