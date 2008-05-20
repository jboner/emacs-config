;;; jde-usages-redefines.el --- jde functions that are redefined to add extra functionality. Hopefully these will all be integrated into jde one day.

;;; Commentary:
;; 


;;; History:
;; 

;;; Code:
(defvar beanshell-scratch-buffer "*jde-beanshell-scratch*"
  "Stores the name of a buffer which stores the output from the beanshell process during calls to `bsh-eval'."
  )

;; modified methods from beanshell.el
(defmethod bsh-snag-lisp-output ((this bsh) process output &optional jeval-output-buffer)
  "Assemble Lisp OUTPUT from the BeanShell."
  (let* ((end-of-output (string-match ".*bsh % " output))
	 (trimmed-output (if end-of-output (substring output 0 end-of-output) output))
	 (jeval-output-buffer (get-buffer-create beanshell-scratch-buffer))
	 (last-output nil))
    (if jeval-output-buffer
	(with-current-buffer jeval-output-buffer
	  (save-excursion
	    (goto-char (point-max))
	    (insert trimmed-output))
	  (let ((old-point (point))
		(pm (point-max)))
	    (condition-case eval-error
 		(while (> pm old-point)
		  (setq last-output (read jeval-output-buffer))
		  (eval last-output)
		  (setq old-point (point))
		  )
	      (end-of-file
	       (goto-char old-point))
	      (error
	       (message "error : %s" (prin1-to-string eval-error))
	       (goto-char old-point)))
	    )
	  )
      )
    (oset this lisp-output (concat (oref this lisp-output) trimmed-output) ) ;; (prin1-to-string last-output)
    (if (not end-of-output)
	(accept-process-output process bsh-eval-timeout))))

(defmethod bsh-eval-lisp-output ((this bsh))
  (if (not (string= (oref this lisp-output) ""))
      (flet ((format-error-msg (error-symbols)
	       (mapconcat (lambda (s) (symbol-name s)) error-symbols " ")))
	(condition-case eval-error
	    (eval (car (last (read (concat "( " (oref this lisp-output) " )")))))
	  (error
	    (message "Error evaluating Lisp result of Java expression evaluation.")
	    (message "  Java expression: %s." (oref this java-expr))
	    (message "  Java evaluation result: %s." (oref this lisp-output))
	    ;; The following causes an unreadable object error on XEmacs when
	    ;; trying to load the byte-compiled file:
	    ;;
	    ;; (message "  Error from evaluating result as Lisp: %s"
	    ;;	  (mapconcat (lambda (s) (symbol-name s)) eval-error " ")
	    (message "  Error from evaluating result as Lisp: %s"
		     (format-error-msg eval-error))
	    (error "Error evaluating Java expresson.  See *Messages* buffer"))))
    (progn
      (message "bsh-eval-r error: Beanshell result is null. Cannot evaluate.")
      (message "  Expression: %s" (oref this java-expr)))))

(defadvice bsh-eval (before jde-usages-move-point-to-end-of-eval-buffer first (this expr &rest r) activate)
  "Move point to the end of 'beanshell-scratch-buffer.  The output of bsh command will end up at the end of this buffer and to eval it correctly point needs to go before it.  This is usually the case but unexpected errors sometimes prevent it, also the user could move point manually when looking at the contents of the buffer."
  
  ;; move to the end of the scratch eval buffer
  (with-current-buffer (get-buffer-create beanshell-scratch-buffer)
    (goto-char (point-max))
    (insert "bsh %:")
    (insert expr)
    (insert "\n")))

;; from jde-find
(defun jde-usages-find-class-source (class &optional other-window)
  "*Find the source file for a specified CLASS.
Calls `jde-find-class-source-file' to do the search.
If it finds the source file, it opens the file in a buffer.
It also tries to go the begining of the class definition.
Optional argument OTHER-WINDOW If non-nil opens CLASS in the other window."
  (let ((source (jde-find-class-source-file class)))

    (when source
      (if (not (string-equal (buffer-file-name)  source))
	  (if other-window
	      (find-file-other-window source)
	    (find-file source)))
      
      (if (fboundp 'senator-re-search-forward)
	  (let ((class-name-pos (string-match "[^.$]*$" class)))
            (if class-name-pos
                (let* ((class-name (substring class class-name-pos))
                       (class-num  (string-to-number class-name)) ;; for anonymous inner classes
                       )
                  (goto-char (point-min))
                  (senator-parse)
                  (if (> class-num 0)
                      (loop for var from 1 to class-num by 1
                            do
                            (re-search-forward "\\Wnew\\s-+\\(\\w\\|\\.\\)+\\s-*(\\s-*)\\s-*{")
                            finally
                            (backward-char)
                            )
                    (senator-re-search-forward (concat "\\b" class-name "\\b") nil t))))))
      t ;; return non-nil, we could find the source file
      )))

(provide 'jde-usages-redefines)
;;; jde-usages-redefines.el ends here
