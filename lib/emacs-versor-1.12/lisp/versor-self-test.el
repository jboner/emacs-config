;;;; versor-self-test.el -- check what is defined and what is not
;;; Time-stamp: <2006-08-02 12:18:07 john>

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



(defun map-statement-parts (mode-fn statement-fn statement-part-fn)
  (mapatoms
   (function
    (lambda (atom)
      (when (and (string-match "-mode$" (symbol-name atom))
		 (get atom 'statements))
	(when mode-fn
	  (funcall mode-fn atom))
	(let ((statements (get atom 'statements)))
	  (mapcar
	   (function
	    (lambda (statement)
	      (when statement-fn 
		(funcall statement-fn atom  statement))
	      (mapcar
	       (function
		(lambda (part)
		  (when statement-part-fn
		    (funcall statement-part-fn atom statement part))))
	       (cdr statement))))
	   statements)))))))

(defvar versor-self-test-part-memory nil
  "Alist of statements to all the known parts for that kind of statement.")

(defun versor-self-test-remember-part (mode statement part)
  "Remember that in MODE, STATEMENT has a PART."
  (let* ((statement-name (car statement))
	 (statement-pair (assoc statement-name versor-self-test-part-memory)))
    (unless statement-pair
      (setq versor-self-test-part-memory
	    (cons (setq statement-pair (list statement-name))
		  versor-self-test-part-memory)))
    (let* ((part-name (car part))
	   (part-pair (assoc part-name (cdr statement-pair))))
      (if part-pair
	  (unless (memq mode (cdr part-pair))
	    (rplacd part-pair (cons mode (cdr part-pair))))
	(rplacd statement-pair
		(cons (list part-name mode)
		      (cdr statement-pair)))))))

(defun versor-self-test (&optional only-show-omissions complete-string)
  (interactive)
  "A pre-release integrity check for versor."
  (with-output-to-temp-buffer "*Versor self-test*"
    (setq versor-self-test-part-memory nil)
    (map-statement-parts nil nil 'versor-self-test-remember-part)
    (princ "Languide statements listing:\n")
    (map-statement-parts
     (lambda (mode)
       (princ (format "\n  %s\n" mode)))
     (lambda (mode statement)
       (let ((possibles (cdr (assoc (car statement) versor-self-test-part-memory)))
	     (missing nil))
	 ;; (princ (format "possibles %S\n" possibles))
	 (mapcar (lambda (possible) (unless (assoc (car possible) (cdr statement))
				      (setq missing (cons possible missing))))
		 possibles)
	 ;; (princ (format "Missing %S\n" missing))
	 (when (or missing (not only-show-omissions))
	   (princ (format "    %s %s\n" (car statement)
			  (if missing
			      (format "(missing %s)"
				      (mapconcat
				       (lambda (missed)
					 (format "\"%s\", defined in %s"
						 (symbol-name (car missed))
						 (mapconcat (lambda (known-in)
							      (symbol-name known-in))
							    (cdr missed)
							    ", ")))
				       missing "; "))
			    (or complete-string "(complete)")))))))
     (lambda (mode statement part)
       (unless only-show-omissions 
	 (princ (format "      %s\n" (car part))))))))

(defun versor-show-omissions ()
  (interactive)
  (versor-self-test t ""))

(provide 'versor-self-test)

;;; end of versor-self-test.el
