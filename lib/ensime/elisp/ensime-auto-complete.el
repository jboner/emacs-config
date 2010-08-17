;;; ensime-auto-complete.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

(require 'auto-complete)

(defun ensime-ac-delete-text-back-to-call-target ()
  "Assuming the point is in a member prefix, delete all text back to the
target of the call. Point should be be over last character of call target."
  (let ((p (point)))
    (re-search-backward "[^\\. ][\\. ]" (point-at-bol) t)
    (let ((text (buffer-substring (1+ (point)) p))
	  (deactivate-mark nil))
      (delete-region (1+ (point)) p)
      text)))


(defun ensime-ac-member-candidates (prefix)
  "Return candidate list."
  (let ((members 
	 (ensime-ac-with-buffer-copy 
	  (save-excursion (insert " "))
	  (ensime-ac-delete-text-back-to-call-target)
	  (ensime-save-buffer-no-hooks)
	  (ensime-rpc-members-for-type-at-point prefix))))
    (mapcar (lambda (m)
	      (let* ((type-sig (plist-get m :type-sig))
		     (type-id (plist-get m :type-id))
		     (is-callable (plist-get m :is-callable))
		     (name (plist-get m :name))
		     (candidate (concat name " : " type-sig)))
		;; Save the type for later display
		(propertize candidate
			    'symbol-name name
			    'scala-type-sig type-sig 
			    'scala-type-id type-id
			    'is-callable is-callable
			    ))) 
	    members)
    ))


(defmacro* ensime-ac-with-buffer-copy (&rest body)
  "Create a duplicate of the current buffer, copying all contents. 
Bind ensime-buffer-connection and buffer-file-name to the given values.
Execute forms in body in the context of this new buffer. The idea is that
We can abuse this buffer, even saving it's contents to disk, and all the 
changes will be forgotten."
  `(let ((buf (current-buffer))
	 (file-name buffer-file-name)
	 (p (point))
	 (conn (ensime-current-connection)))
     (let ((val (unwind-protect
		    (with-temp-buffer
		      (let ((ensime-buffer-connection conn)
			    (buffer-file-name file-name))
			(insert-buffer-substring buf)
			(goto-char p)
			,@body
			)))))
       ;; Make sure we overwrite any changes
       ;; saved from temp buffer.
       (clear-visited-file-modtime)
       (ensime-save-buffer-no-hooks)
       val
       )))


(defun ensime-ac-completing-constructor-p (prefix)
  "Are we trying to complete a call of the form 'new [prefix]' ?"
  (save-excursion
    (goto-char (- (point) (length prefix)))
    (looking-back "new\\s-+" (ensime-pt-at-end-of-prev-line))
    ))

(defun ensime-ac-name-candidates (prefix)
  "Return candidate list."
  (let ((is-constructor (ensime-ac-completing-constructor-p prefix)))
    (let ((names 
	   (ensime-ac-with-buffer-copy 
	    (backward-delete-char (length prefix))
	    (save-excursion
	      ;; Insert a dummy value after (point), so that
	      ;; if we are at the end of a method body, the
	      ;; method context will be extended to include
	      ;; the completion point.
	      (insert "()"))
	    (ensime-save-buffer-no-hooks)
	    (ensime-rpc-name-completions-at-point
	     prefix is-constructor))))

      (mapcar (lambda (m)
		(let* ((type-sig (plist-get m :type-sig))
		       (type-id (plist-get m :type-id))
		       (is-callable (plist-get m :is-callable))
		       (name (plist-get m :name))
		       (candidate (concat name " : " type-sig)))
		  ;; Save the type for later display
		  (propertize candidate
			      'symbol-name name
			      'scala-type-sig type-sig 
			      'scala-type-id type-id
			      'is-callable is-callable
			      ))
		) names))))


(defun ensime-ac-get-doc (item)
  "Return doc for given item."
  (get-text-property 0 'scala-type-sig item))

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

Delete the candidate from the buffer as inserted by auto-complete.el (because the
candidates include type information that we don't want inserted), and re-insert just 
the name of the candidate.

If the candidate is a callable symbol, add the meta-info about the 
params and param types as text-properties of the completed name. This info will
be used later to give contextual help when entering arguments."

  (let* ((candidate candidate) ;;Grab from dynamic environment..
	 (name (get-text-property 0 'symbol-name candidate))
	 (type-id (get-text-property 0 'scala-type-id candidate)))
    (delete-backward-char (length candidate))
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
			      (propertize (nth i param-names) 
					  'face font-lock-variable-name-face)
			      (propertize (ensime-type-name-with-args pt)
					  'face font-lock-type-face)
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

  (make-local-variable 'ac-use-comphist)
  (setq ac-use-comphist nil)

  (make-local-variable 'ac-auto-start)
  (setq ac-auto-start nil)

  (make-local-variable 'ac-expand-on-auto-complete)
  (setq ac-expand-on-auto-complete nil)

  (make-local-variable 'ac-use-fuzzy)
  (setq ac-use-fuzzy nil)

  (make-local-variable 'ac-use-quick-help)
  (setq ac-use-quick-help nil)

  (make-local-variable 'ac-trigger-key)
  (ac-set-trigger-key "~")

  (auto-complete-mode 1)
  )

(defun ensime-ac-disable ()
  (auto-complete-mode 0)
  )

(provide 'ensime-auto-complete)