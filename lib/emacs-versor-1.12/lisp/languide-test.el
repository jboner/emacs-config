;;;; test routines for languide
;;; Time-stamp: <2007-03-19 20:26:59 jcgs>

(require 'languide)

(defvar next-colour nil)

(defun next-colour ()
  "Provide a different colour on each subsequent call."
  (when (null next-colour)
      (setq next-colour
	    '( "green" "cyan" "yellow"
	      )
	    ))
  (prog1
      (car next-colour)
    (setq next-colour (cdr next-colour))))

(defun make-languide-test-areas (a b)
  "Make list of conses of start and end of movement.
As a side-effect, colours in the buffer."
  (font-lock-mode -1)
  (let ((probe a)
	(remembered-whereto 0)
	this-whereto
	(areas nil)
	(this-area (cons a a)))
    (while (<= probe b)
      (goto-char probe)
      (condition-case error-var
	  (progn
	    (beginning-of-statement-internal nil)
	    (setq this-whereto (point)))
	(error
	 (setq this-whereto (cons 'error error-var))))
      (if (eq this-whereto remembered-whereto)
	  (progn
	    (rplacd this-area probe)
	    )
	(let ((area-descr (cons remembered-whereto this-area)))
	  (if (and (numberp (car this-area))
		   (numberp (cdr this-area)))
	      (let ((buffer-read-only nil))
		(put-text-property (car this-area) (cdr this-area)
				   'face (cons 'background-color (next-colour)))))
	  (setq areas (cons area-descr
			    areas)
		remembered-whereto this-whereto
		this-area (cons probe probe))
	  ))
      (incf probe))
    (setq areas (cons (cons remembered-whereto this-area) areas))
    (nreverse areas)))


(defun languide-test-region (a b)
  "Test statement-based navigation in text from A to B.
This classifies the text into chunks according to where
beginning-of-statement-internal moves to."
  (interactive "r")
  (let ((areas (make-languide-test-areas a b)))
    (message "areas list: %S" areas)
    (with-output-to-temp-buffer "*Areas*"
      (while areas
	(setq this-area (car areas))
	(princ (format "\"%s\": \"%s\"\n"
		       (if (and
			    (consp this-area)
			    (numberp (car this-area)))
			   (subst-char-in-string
			    ?\n ?_
			    (buffer-substring (max (car this-area) (point-min))
					      (min (+ 10 (car this-area)) (point-max))))
			 "error")
		       (if (and (consp this-area)
				(consp (cdr this-area))
				(numberp (cadr this-area))
				(numberp (cddr this-area)))
			   (subst-char-in-string
			    ?\n ?_
			    (buffer-substring (cadr this-area) (cddr this-area)))
			 (format "strange area: %S" this-area))))
	(setq areas (cdr areas))))))

(defun languide-test-buffer ()
  "Test statement-based navigation in current buffer."
  (interactive)
  (languide-test-region (point-min) (point-max)))

(defun languide-test (&optional test)
  "Test statement-based navigation on a standard text."
  (interactive "sTest: ")
  (or (and (stringp test)
	   (not (zerop (length test))))
      (setq test "test1.c"))
  (find-file (expand-file-name test (substitute-in-file-name "$COMMON/open-projects/emacs-versor/testfiles")))
  (toggle-read-only 1)
  (languide-test-buffer))
