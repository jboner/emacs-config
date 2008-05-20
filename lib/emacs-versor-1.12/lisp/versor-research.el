;;;; versor-research.el -- Count use and non-use of versor
;;; Time-stamp: <2007-06-23 15:06:34 jcgs>

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



(defvar versor-research-buffer-before-command nil
  "The buffer we were in before the current command.")

(defvar versor-research-change-happened nil
  "Whether anything changed during this command.")

(defvar versor-research-starting-position nil
  "Where point was when this command started.")

(defvar versor-research-previous-command-type nil
  "The type of command before the current one.")

(defvar versor-research-previous-previous-command-type nil
  "The type of command before the one before the current one.")

(defvar versor-research-same-type-command-chain nil
  "Details of the latest series of commands of the same type.")

(defvar versor-research-previous-same-type-command-chain nil
  "Details of the previous series of commands of the same type.")

(defun versor-research-pre-command-function ()
  "Set things up to see what happens during the coming command."
  ;; (message "Resetting research vars")
  (setq versor-research-buffer-before-command (current-buffer)
	versor-research-change-happened nil
	versor-research-starting-position (point)))

(defvar versor-research-versor-edit-after-versor-moves nil
  "Each entry is the list of versor movement commands leading directly
to a versor edit.")

(defvar versor-research-non-versor-edit-after-versor-moves nil
  "Each entry is the list of versor movement commands leading directly to a non-versor edit.")

(defvar versor-research-non-versor-edit-after-versor-moves nil
  "Each entry is the list of non-versor movement commands leading
directly to a non-versor edit.")

(defvar versor-research-versor-edit-after-versor-moves-with-manual-adjustment nil
  "Each entry is a cons of the list of versor movement commands
leading indirectly to a versor edit, and the list of non-versor
movements that came between them.")

(defvar versor-research-versor-edit-after-non-versor-moves nil
  "Each entry is the list of non-versor movement commands leading
directly to a versor edit.")

(defvar versor-research-non-versor-edit-after-versor-moves-with-manual-adjustment nil
  "Each entry is a cons of the list of versor movement commands
leading indirectly to a non-versor edit, and the list of non-versor
movements that came between them.")

(defvar versor-research-non-versor-edit-after-non-versor-moves nil
    "Each entry is the list of non-versor movement commands leading
directly to a non-versor edit.")

(defun versor-get-equivalent-command (command)
  "Find the versor equivalent for COMMAND, if any."
  (cdr (assoc command versor-equivalent-commands)))

(defvar versor-research-report-errors t
  "*Whether to report errors in the research code.")

(defun versor-research-post-command-function ()
  "Record what happened during this command."
  (when (and (eq (current-buffer) versor-research-buffer-before-command)
	     (not (string-match "^\\*Minibuf" (buffer-name))))
    ;; we are not interested in buffer-switching commands, as they
    ;; are not versor commands
    (condition-case error-var
	(let* ((commentary (and versor-research-live-commentary
				(not (string-match "inibu" (buffer-name)))))
	       (command-was-versor (versor-command-p this-command))
	       (command-was-languide (languide-command-p this-command))
	       (command-was-versor-languide (versor-languide-command-p this-command))
	       (command-type-string (concat (cond
					     (command-was-versor-languide "versor-languide")
					     (command-was-languide "languide")
					     (command-was-versor "versor")
					     (t "non-versor"))
					    "-"
					    (cond
					     (versor-research-change-happened
					      (if (zerop (third versor-research-change-happened))
						  "insertion"
						"deletion"))
					     ((not (eq (point) versor-research-starting-position)) "move")
					     (t "other"))))
	       ;; todo: try to get it all done in the "symbol-building" style above
	       (command-type (intern command-type-string)))
	  (when commentary
	    (message "Command was %S(%S) (previous was %S)"
		     command-type
		     command-type-string
		     versor-research-previous-command-type))
	  (when (and versor-indicate-missed-opportunities
		     (not (string-match "inibuf" (buffer-name)))
		     (not command-was-versor))
	    (let ((equivalent (or (versor-get-equivalent-command this-command)
				  (if (memq this-command versor-used-commands)
				      this-command
				    nil))))
	      (when equivalent
		(let* ((action-this (versor-find-in-current-dimension equivalent))
		       (action-over (versor-find-in-current-dimension equivalent 1))
		       (access-command-this (versor-command-for-action action-this))
		       (access-command-over (versor-command-for-action action-over 1))
		       (keys (if access-command-this
				 (where-is-internal access-command-this)
			       (if access-command-over
				   (where-is-internal access-command-over)
				 nil))))
		  (when (or access-command-this
			    access-command-over)
		    (if keys
			(progn
			  (message "You could have used %s (%s)"
				   (or access-command-this
				       access-command-over)
				   (mapconcat 'key-description keys " or "))
			  (when (numberp versor-indicate-missed-opportunities)
			    (sit-for versor-indicate-missed-opportunities)))
		      (when (eq versor-indicate-missed-opportunities 'all)
			(message "You could have used %s"
				 (or access-command-this
				     access-command-over)))))))))
	  (if (eq command-type versor-research-previous-command-type)
	      ;; If several consecutive commands are of the same
	      ;; general type, build up a chain of them.
	      (push (cons this-command versor-research-change-happened)
		    versor-research-same-type-command-chain)
	    ;; The type of command has changed, between navigation and
	    ;; editing, or between versor and non versor. This is the
	    ;; interesting bit -- particularly the transitions between
	    ;; versor and non-versor commands.
	    (cond
	     ((memq command-type '(versor-insertion
				   versor-deletion
				   versor-languide-insertion
				   versor-languide-deletion
				   languide-insertion
				   languide-deletion))
	      (cond
	       ((memq versor-research-previous-command-type '(versor-move
							      versor-languide-move
							      languide-move))
		;; this is the one we like -- a versor command after versor moves
		(when commentary (message "versor edit after versor moves"))
		(push versor-research-same-type-command-chain
		      versor-research-versor-edit-after-versor-moves)
		)
	       ((eq versor-research-previous-command-type 'non-versor-move)
		;; versor command after non-versor move
		(if (memq versor-research-previous-previous-command-type '(versor-move
									   versor-languide-move
									   languide-move))
		    (progn
		      ;; non-versor move may have been an adjustment after a versor move
		      (when commentary (message "versor edit after versor moves with manual adjustment"))
		      (push (cons versor-research-same-type-command-chain
				  versor-research-previous-same-type-command-chain)
			    versor-research-versor-edit-after-versor-moves-with-manual-adjustment)

		      )
		  (when commentary (message "versor edit after non versor moves"))
		  (push versor-research-same-type-command-chain
			versor-research-versor-edit-after-non-versor-moves)

		  ))))
	     ((memq command-type '(non-versor-deletion non-versor-insertion))
	      (cond
	       ((eq versor-research-previous-command-type 'non-versor-move)
		;; old-fashioned non-versor emacs editing, but did they use versor first then adjust?
		(if (memq versor-research-previous-previous-command-type '(versor-move
									   versor-languide-move
									   languide-move)) 
		    (progn
		      (when commentary (message "non versor edit after versor moves with manual adjustment"))
		      (push (cons versor-research-same-type-command-chain
				  versor-research-previous-same-type-command-chain)
			    versor-research-non-versor-edit-after-versor-moves-with-manual-adjustment))
		  (when commentary (message "non versor edit after non versor moves"))
		  (push versor-research-same-type-command-chain
			versor-research-non-versor-edit-after-non-versor-moves)))
	       ((memq versor-research-previous-command-type '(versor-move
							      languide-move
							      versor-languide-move))
		;; using versor to navigate but not to edit
		(when commentary (message "non versor edit after versor moves"))
		(push versor-research-same-type-command-chain
		      versor-research-non-versor-edit-after-versor-moves))
	       )))
	    (setq versor-research-previous-previous-command-type versor-research-previous-command-type
		  versor-research-previous-command-type command-type
		  versor-research-same-type-command-chain nil)))
      (error (when versor-research-report-errors
	       (message "error %s in  versor-research-post-command-function" error-var))))))

;; I don't think we really need this one
;; (defun versor-research-before-change-function (from to)  )

(defun versor-research-after-change-function (from to old-length)
  "Part of looking at what happened during a command"
  (when (eq (current-buffer) versor-research-buffer-before-command)
    (setq versor-research-change-happened (list from to old-length)))
  ;; (message "Change %S in %S" versor-research-change-happened (current-buffer))
  )

(defun versor-research-start ()
  "Start recording lengths of sequences of consecutive commands of different kinds."
  (interactive)
  (add-hook 'pre-command-hook 'versor-research-pre-command-function)
  (add-hook 'post-command-hook 'versor-research-post-command-function)
  ;; (add-hook 'before-change-functions 'versor-research-before-change-function)
  (add-hook 'after-change-functions 'versor-research-after-change-function))

(defun versor-research-stop ()
  (interactive)
  (remove-hook 'pre-command-hook 'versor-research-pre-command-function)
  (remove-hook 'post-command-hook 'versor-research-post-command-function)
  ;; (remove-hook 'before-change-functions 'versor-research-before-change-function)
  (remove-hook 'after-change-functions 'versor-research-after-change-function))

(defun versor-research-reset (&optional force)
  "Reset the versor research data. You probably don't want to do this."
  (interactive nil)
  (when (or force (yes-or-no-p "Reset research data? "))
    (setq versor-research-versor-edit-after-versor-moves nil
	  versor-research-versor-edit-after-versor-moves-with-manual-adjustment nil
	  versor-research-versor-edit-after-non-versor-moves nil
	  versor-research-non-versor-edit-after-versor-moves nil
	  versor-research-non-versor-edit-after-non-versor-moves nil
	  versor-research-non-versor-edit-after-versor-moves-with-manual-adjustment nil)))

(defun hook-memq (fn var)
  "Check whether FN is part of VAR, allowing for the global hook mechanism."
  (or (memq fn (symbol-value var))
      (and (memq t (symbol-value var))
	   (memq fn (default-value var)))))

(defun versor-generate-report-text ()
  "Helper function for versor-research-report."
  (princ (format "  Versor edits after versor moves: %d\n"
		 (length versor-research-versor-edit-after-versor-moves)))
  (princ (format "  Versor edits after versor moves with adjustments: %d\n"
		 (length versor-research-versor-edit-after-versor-moves-with-manual-adjustment)))
  (princ (format "  Versor edits after non-versor moves: %d\n"
		 (length versor-research-versor-edit-after-non-versor-moves)))
  (princ (format "  Non-versor edits after versor moves: %d\n"
		 (length versor-research-non-versor-edit-after-versor-moves)))
  (princ (format "  Non-versor edits after non-versor moves: %d\n"
		 (length versor-research-non-versor-edit-after-non-versor-moves)))
  (princ (format "  Non-versor edits after versor moves with adjustments: %d\n"
		 (length versor-research-non-versor-edit-after-versor-moves-with-manual-adjustment)))
  (let ((pre-command-ok (hook-memq 'versor-research-pre-command-function 'pre-command-hook))
	(post-command-ok (hook-memq 'versor-research-post-command-function 'post-command-hook))
	(after-change-ok (hook-memq 'versor-research-after-change-function 'after-change-functions)))
    (if (and pre-command-ok post-command-ok after-change-ok)
	(princ "  All hooks still in place\n")
      (unless pre-command-ok (princ "  Pre-command hook missing\n"))
      (unless post-command-ok (princ "  Post-command hook missing\n"))
      (unless after-change-ok (princ "  After-change function missing\n")))))

(defun versor-research-report ()
  (interactive)
  (with-output-to-temp-buffer "*Versor research report*"
    (versor-generate-report-text)))

(defun versor-save-research-data ()
  "Save the versor research data to file."
  (interactive)
  (save-window-excursion
    (find-file versor-research-log-file)
    (goto-char (point-max))
    (insert "Log for "
	    user-mail-address
	    " at "
	    (current-time-string)
	    "\n")
    (let ((standard-output (current-buffer)))
      (versor-generate-report-text)
      (princ "\n"))
    (basic-save-buffer)
    (versor-research-reset t)))

(provide 'versor-research)

;;; end of versor-research.el
