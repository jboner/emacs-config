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
 
;;; DEPENDENCIES
  
;; Auc-tex dependencies exist as well (they may be ignored)

(require 'thingatpt)
(require 'find-func)

(eval-when-compile (require 'cl))


;;; ERRORS
(put 'listen-not-found 'error-conditions '(error listen-not-found))
(put 'listen-not-found 'error-message "EmacsListen")

;;; thing-at-point seems to be not entirely worked through.  The
;;; function forward-thing is probably not defined correctly; it would
;;; barf on a THING whose forward-op is a lambda expression; this has
;;; been corrected in GNU Emacs 21 (according to the GNU people who
;;; have been informed about the problem).  We overcome this trivial
;;; problem as follows (where we could also have used the advice
;;; package).

(when (string-match "Emacs 20." (emacs-version))
  (if (not (fboundp 'forward-thing-original))
      (fset 'forward-thing-original 
	    (symbol-function 'forward-thing)))
  
  (defun forward-thing (thing &optional number)
    "This function invokes the original forward-thing and if that fails
it calls forward the forward-op of THING."
    (condition-case nil
	(forward-thing-original thing number)
      (error (let ((f (get thing 'forward-op)))
	       (if (and f (indirect-function f))
		   (funcall f (or number 1))))))))
  
;;; The first half of bounds-of-thing-at-point the algorithm below
;;; would erroneously (?) terminate the whole function call when an
;;; error was signaled by the forward-x function at the end of the
;;; buffer; we address this problem in a way similar to what we just
;;; did above.  This little problem has been reported to the GNU
;;; folks. It has not been corrected as of Emacs 21.3.

(if (not (fboundp 'bounds-of-thing-at-point-original))
    (fset 'bounds-of-thing-at-point-original 
	  (symbol-function 'bounds-of-thing-at-point)))

(defun bounds-of-thing-at-point (thing)
  "This function invokes the original bounds-of-thing-at-point and if
that fails it tries to use end-op to move forward, then backward to
find the thing at point."
  (condition-case nil
      (bounds-of-thing-at-point-original thing)
    (error 
     (let ((current-pos (point)))
       (condition-case nil
	   (save-excursion	    
	     (let ((e (condition-case nil
			  (funcall (get thing 'end-op) 
				   (point)) 
			(error nil)))
		   (b (condition-case nil
			  (funcall (get thing 'forward-op) 
				   (point) -1)
			(error nil))))
	       (if (and b e (< b current-pos))
		   (let ((e1
			  (condition-case nil
			      (progn 
				(funcall 
				 (funcall (get thing 'end-op)))
				(point)
				(error nil)))))
		     (if e1
			 (cons b e1)))))))))))

;;; A generic way of constructing a forward function for a
;;; bounds-of-thing-at-point function

(defun listen-forward (thing &optional n skip-comments)
  "Define a forward function assumming that an explicit or implicit
bounds-of-thing-at-point is provided."
  (if (null n) (setq n 1))
  (cond 
   ((> n 0)
    (do ((i 0 (1+ i))) ((eq i n))
      (forward-comment (buffer-size))
      (let (bounds)
	(save-excursion 
	  (while 
	      (and (or 
		    (null
		     (setq bounds
			   (bounds-of-thing-at-or-beginning-at-point 
			    thing)))
		    (= (cdr bounds) (point)))
		   (not (eobp)))
	    (forward-char 1)
	    (forward-comment (buffer-size))))
	(if bounds (goto-char (cdr bounds))))))
   ((< n 0)
    (do ((i n (1+ i))) ((eq i 0))
      (forward-comment (- (buffer-size)))
      (let (bounds)
	(save-excursion
	  (while 
	      (and (or 
		    (null
		     (setq bounds
			   (bounds-of-thing-at-or-ending-at-point
			    thing)))
		    (= (car bounds) (point)))
		   (not (bobp)))
	    (forward-char -1)    
	    (forward-comment (- (buffer-size)))))
	(if bounds (goto-char (car bounds))))))))

(defun bounds-of-a-number-of-things-at-point (thing n)
  "Determine the start and end buffer locations for N THINGs at point,
where THING is an entity for which there is a either a corresponding
forward-THING operation, or corresponding beginning-of-THING and
end-of-THING operations, eg. 'word, 'sentence, 'defun.  Return a cons
cell '(start . end) giving the start and end positions.  THING may
also be 'this or 'that get.  Signal 'listen-not-found if there are no
such things."
  (assert (integerp n))
  (condition-case nil 
      (cond ((member thing '(this that))
	     (let ((result 
		    (bounds-of-thing-at-point 'thing)))
	       (or result
		   (error))))
	    (t 
	     (let* ((start-end 
		     (if (not (member thing '(char line-inclusive)))
			 (bounds-of-thing-at-or-ending-at-point thing)
		       (bounds-of-thing-at-point thing)))
		    (result
		     (cond ((= (abs n) 1)
			    start-end)
			   ((and (or (< n -1) (> n 1)) start-end)
			    (save-excursion
			      (condition-case nil
				  (let ((end 
					 (if (> n 0) (car start-end) 
					   (if (member thing '(line))
					       ;; hack, since (forward-line -1) at end
					       ;; of line goes to the beginning of the
					       ;; previous line 
					       (incf n))
					   (cdr start-end))))
				    (goto-char end) 
				    (forward-thing thing n)
				    (cons (min (point) end) (max (point)
								 end)))))))))
	       (or result (error))))) 
    (error
     (signal 'listen-not-found 
	     (list (format "%s not found" thing))))))


(defun bounds-of-a-number-of-things-at-or-near-point (thing n)
  (let ((r (bounds-of-a-number-of-things-at-point thing n)))
    (if r 
	r
      (save-excursion
	(re-search-forward "[ \n\t]*" nil t)
	(bounds-of-a-number-of-things-at-point thing n)))))

(defun locate-bounds-of-things-near-point (thing n)
  (condition-case nil
      (or (bounds-of-a-number-of-things-at-or-near-point thing n) 
	  (error))
    (error (signal 'listen-not-found
		   (list (format (if (= n 1)
				     "No %s found" "No %ss found")
				 (symbol-name thing)))))))

(defun bounds-of-thing-at-or-beginning-at-point (thing &optional
						       alternative-thing)
  (if (and (eq thing 'term) (null alternative-thing))
      (setq alternative-thing 'block))
  (let ((suggested-bounds (bounds-of-thing-at-point thing)))
    (if (and suggested-bounds 
	     (= (cdr suggested-bounds) (point)))
	;; then, there could still possibly be a thing that really
	;; starts at point, since things may abut
	(if (not (eobp))
	    (let ((new-bounds
		   (save-excursion 
		     (forward-char 1)
		     (bounds-of-thing-at-point thing)))
		  (new-alter-bounds
		   (and alternative-thing
			(save-excursion 
			  (forward-char 1)
			  (bounds-of-thing-at-point 
			   alternative-thing)))))
	      (if (and new-bounds (= (car new-bounds) (point)))
		  new-bounds
		(if (and new-alter-bounds 
			 (= (car new-alter-bounds) (point)))
		    new-alter-bounds
		  suggested-bounds)))
	  suggested-bounds)
      suggested-bounds)))

(defun bounds-of-thing-at-or-ending-at-point (thing &optional
						    alternative-thing)
  (if (and (eq thing 'term) (null alternative-thing))
      (setq alternative-thing 'block))
  (let ((suggested-bounds (bounds-of-thing-at-point thing)))
    (if (and suggested-bounds 
	     (= (car suggested-bounds) (point)))
	;; then, there could still possibly be a thing that really
	;; ends right before point, since things may abut
	(if (not (bobp))
	    (let ((new-bounds
		   (save-excursion 
		     (forward-char -1)
		     (bounds-of-thing-at-point thing)))
		  (new-alter-bounds
		   (and alternative-thing
			(save-excursion 
			  (forward-char -1)
			  (bounds-of-thing-at-point 
			   alternative-thing)))))
	      (if (and new-bounds (= (cdr new-bounds) (point)))
		  ;;	      (if (and new-bounds (<= (cdr new-bounds) (point)))
		  new-bounds
		(if (and new-alter-bounds
			 (= (cdr new-alter-bounds) (point)))
		    new-alter-bounds
		  suggested-bounds)))
	  suggested-bounds)
      suggested-bounds)))

(defun listen-end-op (thing)
  (let ((res (bounds-of-thing-at-or-beginning-at-point thing)))
    (if res (goto-char (cdr res))
      (error "Not found"))))

(defun listen-beginning-op (thing)
  (let ((res (bounds-of-thing-at-or-ending-at-point thing)))
    (if res (goto-char (car res))
      (error "Not found"))))

(defun next-thing (THING &optional N)
  "Move forward to the beginning of the next THING."
  (let ((forward-op (or (get THING 'forward-op)
			(intern-soft (format "forward -%s" THING)))))
    (assert (indirect-function forward-op))
    (let ((bounds
	   (bounds-of-thing-at-or-beginning-at-point THING)))
      (condition-case 
	  nil
	  (cond 
	   ((and N (> N 0))
	    (if (not (member THING '(line)))
		(progn 
		  (if (consp bounds)
		      (goto-char (cdr bounds)))
		  (funcall forward-op (or N 1))
		  (funcall forward-op -1))
	      ;; else, we have a thing concept, where
	      ;; forward doesn't bring us to the end of the
	      ;; thing at point, but directly to the next
	      ;; thing
	      (funcall forward-op N)))
	   ((and N (< N 0))
	    (if (consp bounds)
		(goto-char (car bounds)))
	    (funcall forward-op N)))
	(error "not found")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;;; CHAR

(put 'char 'forward-op 'forward-char)
(put 'char 'bounds-of-thing-at-point 
     (function (lambda () (cons (point) (1+ (point))))))

;;; WORD 

(put 'word 'forward-op 'forward-word)


;;; CHUNK

;;; the (defconst chunk-syntax "^-()\"$@") definition, meant to be
;;; used with 'skip-syntax', of chunk-syntax has the unfortunate
;;; effect that newline characters are included in lisp mode (and
;;; probably other modes, too ())
(defconst chunk-syntax-delimiter-characters
  "][{}()<> \n\t/")
(defconst chunk-syntax
  (concat "[^" chunk-syntax-delimiter-characters "]"))
(defconst non-chunk-syntax
  (concat "[" chunk-syntax-delimiter-characters "]"))

(put 'chunk 'end-op
     (function (lambda () 
		 (re-search-forward (concat chunk-syntax "*") nil t)
		 (if (match-beginning 0)
		     (goto-char (match-end 0))))))

(put 'chunk 'beginning-op
     (function (lambda () 
		 (save-excursion
		   (re-search-backward 
		    (concat "\\(^\\|" non-chunk-syntax "\\)"
			    chunk-syntax "*\\=") nil t))
		 (if (match-beginning 0)
		     (goto-char (match-end 1))))))

(put 'chunk 'forward-op
     (function (lambda (&optional n) 
		 (if (null n) (setq n 1))
		 (if (> n 0)
		     (while (> n 0)
		       (re-search-forward
			(concat non-chunk-syntax "*" chunk-syntax "*") nil
			t)
		       (decf n))
		   (while (< n 0)
		     (re-search-backward
		      chunk-syntax nil t)
		     (if (match-beginning 0)
			 (funcall (get 'chunk 'beginning-op)))
		     (incf n))))))


;;; STRETCH
;;; intra line white space.

(put 'stretch 'forward-op 
     (function 
      (lambda (&optional n)
	(if (> n 0)
	    (while (> n 0)
	      (re-search-forward "\\=[^ \t]*[ \t]*" nil t) 
	      (decf n))
	  (while (< n 0)
	    (re-search-backward "[ \t]" nil t)
	    (re-search-backward "[^ \t]" nil t) 
	    (if (match-beginning 0)
		(goto-char (match-end 0)))
	    (incf n))))))

(put 'stretch 'end-op
     (function (lambda () 
		 (re-search-forward "[ \t]*" nil t))))

(put 'stretch 'beginning-op
     (function (lambda () 
		 (re-search-backward "[^ \t]" nil t)
		 (if (match-beginning 0)
		     (goto-char (match-end 0))))))

;;; THING

(put 'thing 'forward-op 
     (function 
      (lambda (&optional n)
	(if (null n) (setq n 1))
	(if (> n 0)
	    (while (> n 0)
	      (re-search-forward "\\=[ \n\t]*[^ \n\t]*" nil t)
	      (decf n))
	  (while (< n 0)
	    (re-search-backward "[^ \t\n]" nil t)
	    (re-search-backward "[ \t\n]" nil t) 
	    (if (match-beginning 0)
		(goto-char (match-end 0)))
	    (incf n))))))

(put 'thing 'end-op
     (function (lambda () 
		 (re-search-forward "[^ \n\t]*" nil t))))

(put 'thing 'beginning-op
     (function (lambda () 
		 (re-search-backward "^\\|[ \n\t]" nil t)
		 (if (match-beginning 0)
		     (goto-char (match-end 0))))))


;;; LINE WITHOUT TERMINATOR

(put 'line 'forward-op 'forward-line)
(put 'line 'end-op 'end-of-line)
(put 'line 'beginning-op 'beginning-of-line)

;;; LINE WITH TERMINATOR

(put 'line-inclusive 'forward-op 'forward-line)

;;; BUFFER

(put 'buffer 'thing-at-point (function (lambda () (cons (point-min)
							(point-max)))))
(put 'buffer 'forward-op 
     (function (lambda (&optional n) 
		 (cond ((eq n 1)
			(end-of-buffer))
		       ((eq n -1)
			(beginning-of-buffer))))))

;; DEF

(put 'def 'beginning-op
     (function (lambda (&optional n)
		 (funcall
		  (cond ((member major-mode 
				 '(emacs-lisp-mode lisp-mode))
			 'beginning-of-defun)
			((member major-mode '(c++mode c-mode java-mode))
			 'c-beginning-of-defun)
			(t 'beginning-of-defun))
		  n))))

(put 'def 'end-op
     (function (lambda (&optional n)
		 (funcall
		  (cond ((member major-mode 
				 '(emacs-lisp-mode lisp-mode))
			 'end-of-defun)
			((member major-mode '(c++mode c-mode java-mode))
			 'c-end-of-defun)
			(t 'end-of-defun))
		  n))))

(put 'def 'forward-op 
     (function (lambda (&optional n) (listen-forward 'def n))))

;;; rely on thing-at-point's method of calculating bounds from
;;; beginning and end operator
(put 'def 'bounds-of-thing-at-point nil)

	   
;; SENTENCE

(put 'sentence 'forward-op 'forward-sentence)
  

;;; PARAGRAPH


(put 'para 'forward-op 'forward-paragraph)
 
;;; BLOCK

(defun listen-detailed-bounds-of-block-at-point ()
  "Problem: should work with listen-paren-registry."
  (let* ((delimiter-length 1)
	 (old-point (point)) 
	 (string-begin
	  ;;  heurstic look for "xyz... (quote is not metaquote) where
	  ;;  x is not a space
	  ;; \s" doesn't work in various text modes, so use ["] or \s"
	  (save-excursion
	    ;; our search will look for what is at current point, as
	    ;; well as is what is before, we do need to check that a
	    ;; non-space, non-cr follows the left "	    
	    (forward-char 1) 
	    (and 
	     ;; \\s\\\\s\" means an escaped quote
	     (re-search-backward
	      "\\(^\\|\\S\\\\)\\(\\s\"\\|[\"]\\)[^ \t\n][^\"]*[\"]?\\=" nil t)
	     ;; the quote is before old-point
	     (< (match-beginning 1) old-point)
	     ;; check that character before quote is reasonable,
	     ;; currently: not a symbol constituent

	     ;; does not work in SGML mode (= is a symbol constituent)
	     ;;    ((not (string-match "\\sw\\|\\s_" 
	     ;;				(buffer-substring 
	     ;;			 (1- (match-beginning 2))
	     ;;			 (match-beginning 2))))
	     (save-match-data
	       (not (string-match "[a-zA-Z0-9_-]" 
				  (buffer-substring 
				   (1- (match-beginning 2))
				   (match-beginning 2)))))
	     ;; here is the beginning of the quoted string
	     (match-beginning 2))))
	 (string-end
	  (if string-begin
	      (save-excursion
		(goto-char string-begin)
		(if (re-search-forward 
		     ;; skip past something that is not '' or " until
		     ;; '' or " (and deal with quoted ")
		     "\\(\\s\\\\s\"\\|[^\"']\\|'[^']\\)*\\S\\\\(\\s\"\\|[\"]\\|''\\)\\(\\W\\|$\\)" 
		     (+ string-begin 1000)
		     t)
		    (if (> (match-end 2) old-point)
			(match-end 2))))))
	 (two-char-quote-string-begin
	  (save-excursion
	    (re-search-backward
	     "``\\([^']\\|['][^']\\)*\\=" nil t)))
	 (two-char-quote-string-end
	  (when two-char-quote-string-begin
	    (save-excursion
	      (goto-char two-char-quote-string-begin)
	      (re-search-forward "''" (+ two-char-quote-string-begin
					 1000) t))))
	 (tag-begin
	  (if (member major-mode '(sgml-mode))
	      (condition-case nil
		  (save-excursion
		    (sgml-skip-tag-backward 1)
		    (point))
		(error nil))))
	 (tag-end 
	  (when tag-begin
	    (condition-case nil
		(save-excursion
		  (goto-char tag-begin)
		  (sgml-skip-tag-forward 1)
		  (point))
	      (error nil))))
	 (env-end
	  (if (member major-mode '(latex-mode))
	      (condition-case nil
		  (save-excursion
		    (LaTeX-find-matching-end)
		    (point))
		(error nil))))
	 (env-begin
	  (when env-end
	    (condition-case nil
		(save-excursion
		  (LaTeX-find-matching-begin)
		  (point))
	      (error nil))))
	 (lbounds
	  (save-excursion
	    (condition-case nil 
		(let ((end-position (progn (up-list 1) (point)))
		      (begin-position (progn (backward-sexp)
					     (point))))
		  (if (and (< begin-position old-point)
			   (> end-position old-point))
		      (cons begin-position end-position)))
	      (error nil))))
	 ;; paired delimiter bounds (dollar signs in TeX/LaTeX)
	 (pd-begin  (save-excursion 
		      (if (re-search-backward
			   "\\s$" 
			   (max 1 (- (point) 1000))
			   t)
			  (point))))
	 
	 (pd-end (if pd-begin 
		     (condition-case nil
			 (save-excursion
			   (goto-char pd-begin)
			   (forward-sexp 1) 
			   (if (> (point) old-point)
			       (point)))
		       (error nil)))))
    
    ;; nullify string presence assumption if the string end couldn't
    ;; be found
    (if (not string-end) (setq string-begin nil))
    ;; and, just to be sure, same thing for tag presence assumption
    (if (not two-char-quote-string-end)
	(setq two-char-quote-string-begin) nil)
    (if (not tag-end) (setq tag-begin nil))
    ;; 
    (if (not pd-end) (setq pd-begin nil))
    (if (not env-begin) (setq env-end nil))
    (if (or lbounds string-begin two-char-quote-string-begin
	    tag-begin pd-begin env-begin)
	(let ((b (apply 'max 
			(delete* nil 
				 (list (car-safe lbounds) 
				       string-begin  
				       two-char-quote-string-begin tag-begin
				       pd-begin env-begin))))
	      (e (apply 'min 
			(delete* nil 
				 (list (cdr-safe lbounds) 
				       string-end
				       two-char-quote-string-end tag-end
				       pd-end env-end)))))
	  (if b
	      (if (and (eq b tag-begin) (eq e tag-end))
		  (cons
		   (cons b e)
		   (cons
		    (save-excursion
		      (goto-char b)
		      (re-search-forward ">\\s-*" nil t)
		      (match-end 0))
		    (save-excursion
		      (goto-char e)
		      (re-search-backward "\\S-\\s-*<" nil 'move)
		      (1+ (match-beginning 0)))))
		;; check that if lbounds is defined and one end is
		;; chosen then the other is not (this is necessary if,
		;; say a stray quoted interval, intersects with a
		;; properly balanced parentheses as in
		;;
		;;  xhz"  (defun x @ "fdsf")
		;;
		;;     |-------------|
		;;
		(if (and lbounds (or (eq b (car lbounds))
				     (eq e (cdr lbounds))))
		    (setq b (car lbounds)
			  e (cdr lbounds)))
		(if (eq b two-char-quote-string-begin)
		    (setq delimiter-length 2))
		(cons
		 (cons b e)
		 (cons (+ b delimiter-length) 
		       (- e delimiter-length)))))))))

(defun bounds-of-block-at-point ()
  (car-safe (listen-detailed-bounds-of-block-at-point)))
 
(put 'block 'end-op
     (function (lambda () (let ((res (bounds-of-block-at-point)))
			    (if res (goto-char (cdr res))
			      (error "Not found"))))))
(put 'block 'beginning-op   
     (function (lambda () (let ((res (bounds-of-block-at-point)))
			    (if res (goto-char (car res))
			      (error "Not found"))))))

(put 'block 'bounds-of-thing-at-point 'bounds-of-block-at-point)

(put 'block 'forward-op 
     (function (lambda (&optional n) (listen-forward 'term n))))


(put 'block 'beginning-op   
     (function (lambda () (listen-beginning-op 'block))))

(put 'block 'end-op   
     (function (lambda () (listen-end-op 'block))))


;;; INNER

(put 'inner 'bounds-of-thing-at-point 
     (function (lambda ()
		 (cdr-safe (listen-detailed-bounds-of-block-at-point)))))

(put 'inner 'forward-op 
     (function (lambda (&optional n) (listen-forward 'inner n))))


;;; SIMPLE TERM

(put 'simple-term 'bounds-of-thing-at-point 'bounds-of-simple-term-at-point)

(defun bounds-of-simple-term-at-point ()
  (or
   ;; something like "\abc" is a term (TeX)
   (and (thing-at-point-looking-at
	 "[\\][a-zA-Z]+")
	(cons (match-beginning 0) (match-end 0)))
   ;; and any identifier is also a term
   (bounds-of-thing-at-point 'identifier)))

;;; TERM

(defun listen-after-end-tag ()
  (save-match-data
    (and  (member major-mode '(sgml-mode xml-mode))
	  (let ((current-point (point)))
	    (save-excursion
	      (re-search-backward "<\\w+/>\\|</\\w+>" nil t)
	      (when (match-end 0) 
		(goto-char (match-end 0))
		(re-search-forward "\\s-*" current-point t)
		(eq (match-end 0) current-point)))))))

(defun listen-after-begin-tag ()
  (save-match-data
    (and  (member major-mode '(sgml-mode))
	  (let ((current-point (point)))
	    (save-excursion
	      (re-search-backward "<\\w+/>\\|<\\w+>" nil t)
	      (when (match-end 0) 
		(goto-char (match-end 0))
		(re-search-forward "\\s-*" current-point t)
		(eq (match-end 0) current-point)))))))

(defun listen-before-begin-tag ()
  (save-match-data
    (and  (member major-mode '(sgml-mode))
	  (let ((current-point (point)))
	    (save-excursion
	      (if (re-search-forward "<\\w+/>\\|<\\w+>" nil t)
		  (let ((b (match-beginning 0)))
		    (when b
		      (goto-char current-point)
		      (re-search-forward "\\s-*" b)
		      (eq (match-end 0) b)))))))))

(defun bounds-of-term-at-point ()
  (cond    
   ;; now, see whether we are just at the beginning of a parenthesized
   ;; expression, by making one step into the presumed expression 
   ((if (not (eobp))
	(save-excursion
	  (let ((old-point (point)))
	    (forward-char 1)
	    (let ((bounds (bounds-of-block-at-point)))
	      (if (and bounds (= (car bounds) old-point))
		  bounds))))))
   ;; maybe at symbol? 
   ((or (and (not (member major-mode '(sgml-mode)))
	     (bounds-of-thing-at-point 'simple-term))
	;; symbol doesn't work in xml ("<", ">" part of symbols)
	(and (member major-mode '(sgml-mode))
	     ;; don't pick <tag> if e.g. <tag>@ 
	     (save-excursion (not (re-search-backward ">\\=" nil t)))
	     (or
	      (and (thing-at-point-looking-at
		    "\\(</\\|<\\)\\w*\\(/>\\|>\\)")
		   (cons (match-beginning 0) (match-end 0)))
	      (and (thing-at-point-looking-at 
		    "\\(^\\|[<>]\\|\\S_\\)\\([^]<>
/+!@#^*:,.?{}()'\" \t\n[]+\\)")
		   (cons (match-beginning 2) (match-end 0)))))))
   ;; so far, no luck!  Then, maybe we're at the last character of a
   ;; parenthesized expression 
   ((if (not (bobp))
	(save-excursion
	  (let ((old-point (point)))
	    (forward-char -1)
	    (if (looking-at "\\s)\\|[\"']")
		(let ((bounds (bounds-of-block-at-point)))
		  (if  (and bounds (= (cdr bounds) old-point))
		      bounds)))))))
   ;; or, we are one position to the right of such an expression, that
   ;; is, at the cdr of a what bounds-of-thing-at-point should return
   ((if (not (bobp))
	(save-excursion
	  (let ((old-point (point)))
	    (forward-char -1)
	    (if (not (bobp))
		(save-excursion
		  (forward-char -1)
		  (let ((bounds (bounds-of-block-at-point)))
		    (if (and bounds (= (cdr bounds) old-point))
			bounds))))))))))


(put 'term 'bounds-of-thing-at-point 'bounds-of-term-at-point)

(put 'term 'beginning-op   
     (function (lambda () (listen-beginning-op 'term))))

(put 'term 'end-op   
     (function (lambda () (listen-end-op 'term))))

(put 'term 'forward-op 
     (function (lambda (&optional n) (listen-forward
				      'term n))))


;;; IDENTIFIER

(put 'identifier 'bounds-of-thing-at-point 
     (function (lambda () 
		 (save-excursion
		   (let* 
		       ((s (save-excursion
			     (while (re-search-backward
				     "\\(\\w\\|[-_]\\)\\=" 
				     nil t))
			     (point)))
			(e (save-excursion
			     (goto-char s)
			     (re-search-forward
			      "\\=\\(\\w\\|[-_]\\)+" nil t))))
		     (if e (cons s e)))))))

(put 'identifier
     'forward-op 
     (function (lambda (&optional n) (listen-forward
				      'identifier n))))


;;; THIS
(put 'this 'bounds-of-thing-at-point
     (function 
      (lambda ()
	(let* ((data (at-mouse-point
		      (cons (selected-window) 
			    (point))))
	       (window (car data))
	       (buf (window-buffer window))
	       (mouse (cdr data)))
	  (if (eq buf (current-buffer))
	      (cons (min mouse (point))
		    (max mouse (point))))))))

;;; SELECTION

;; also known as 'that 
(put 'that 'bounds-of-thing-at-point
     (function 
      (lambda ()
	(if mark-active
	    (cons (region-beginning) (region-end))))))

;; WHAT'S THE WINDOW THE MOUSE IS IN

(defun mouse-window ()
  (let* ((mouse-position (mouse-position))
	 (frame (car mouse-position))
	 (coordinates (cdr mouse-position))
	 (x-pos (car coordinates))
	 (y-pos (cdr coordinates)))
    (window-at x-pos y-pos frame)))

(defun goto-mouse ()
  (select-window (mouse-window))
  (goto-mouse-in-window))

(defun move-to-window-column (n)
  (move-to-column n))

(defun goto-mouse-in-window ()
  "Go to the mouse's location, but only if it's in this window."
  (let* ((coordinates (cdr (mouse-position)))
	 (mouse-window (mouse-window))
	 (coordinates (coordinates-in-window-p coordinates mouse-window)))
    (cond ((not (eq mouse-window (selected-window)))
	   (error "Mouse not in current window"))
	  ((eq coordinates 'mode-line)
	   (error "Can't go to mode-line"))
	  ((eq coordinates 'vertical-line)
	   (error "Can't go to vertical line"))
	  ((listp coordinates)
	   (let ((result 
		  (compute-motion (window-start)
				  '(0 . 0)
				  (point-max)
				  coordinates
				  (window-width)
				  (cons (window-hscroll) 0)
				  (selected-window))))
	     (goto-char (nth 0 result))
	     (unless (eq (nth 2 result) (cdr coordinates))
	       ;; then scan left us at the beginning of the next line,
	       ;; we must back up to end of last line
	       (backward-char 1)))))))

;;; BUG, Nils: doesn't work when mouse is beyond end of line  
;;; save-window-excursion doesn't work in Emacs 20
(defmacro at-mouse-point (&rest body)
  "Save current location, move to mouse's location, execute BODY, restore."
  `(save-excursion
     (let ((old-window (selected-window)))
       (select-window (mouse-window))
       (unwind-protect
	   (save-excursion
	     (goto-mouse-in-window)
	     ,@body)
	 (select-window old-window)))))

(defun mouse-point ()
  "Return the buffer position of the character mouse is over."
  (at-mouse-point (point)))

(defun thing-at-mouse (thing)
  (at-mouse-point (thing-at-point thing)))

(defun listen-buffer-region-near-common-position-this-or-that
  (words &optional numb)
  "Return (BUFFER (BEGINNING . END) if a NUMBER of SCOPEs in BUFFER
are found at COMMON-POSITION or if this or that range is provided.  If
region is not defined, listen-not-found is signaled with an
explanatory string."
  (let ((scope (listen-scope-argument words))
	(number (or numb (listen-number-argument words))))
    (cond ((member scope '(this that))
	   (condition-case m
	       (let ((result (bounds-of-thing-at-point scope)))
		 (if result (cons (current-buffer)
				  result)
		   (error)))
	     (error
	      (signal 'listen-not-found (list (format "%s not found" scope))))))
	  ((not (listen-position-argument words))
	   (error "internal--no position"))
	  (t
	   (condition-case m
	       (listen-simple-do-at-position
		`(cons (current-buffer)
		       (locate-bounds-of-things-near-point (quote ,scope)
							   ,number))
		words)
	     (listen-not-found
	      (signal 'listen-not-found
		      (list (concat (cadr m) 
				    (format " at position %s" 
					    (symbol-name (car 
							  (rassoc
							   (listen-position-argument
							    words)
							   listen-common-positions)))))))))))))


(defun listen-region-near-common-position-this-or-that
  (words &optional numb)
  ""
  (cdr-safe 
   (listen-buffer-region-near-common-position-this-or-that
    words numb)))

(defun listen-buffer-near-common-position-this-or-that
  (words &optional numb)
  ""
  (car-safe 
   (listen-buffer-region-near-common-position-this-or-that
    words numb)))

;;; DO-AT-POSITION
(defun listen-simple-do-at-position (command &optional words)
  "Execute directly at position indicated by current listen command,
no encapsulating in event macros as listen-do-at-position does."
  (if (null words)
      (setq words (listen-event-command-words last-command-event)))
  (let ((position (listen-argument listen-common-positions words t)))

    (assert (member position '(nil here go there mark last)))
    (cond
     ((member position '(nil here)) (listen-simple-execute command))
     ((eq position 'there)
      (at-mouse-point (listen-simple-execute command)))
     ((eq position 'go)
      (let ((res
	     (at-mouse-point 
	      (cons (listen-simple-execute command)
		    (cons (selected-window) (point))))))
	(select-window (cadr res))
	(goto-char (cddr res))
	(car res)))
     ((eq  position 'mark) 
      (save-excursion (goto-char (or (mark t)
				     (point)))
		      (listen-simple-execute command)))
     ((eq position 'last) 
      (save-excursion
	(let ((l (listen-last-buffer-position)))
	  (if l
	      (with-current-buffer (car l)
		(goto-char (cdr l))
		(listen-simple-execute command)))))))))

(defun listen-delete-region (start end)
  (delete-region start end)
  (listen-fix start t))

(defun listen-insert-string (string &optional no-fix-flag)
  (let ((old-point (point-marker))) 
    (listen-insert-simple string)
    (if (not no-fix-flag)
	(let ((new-point (point-marker)))
	  (listen-fix (point) t)
	  (listen-fix old-point t new-point)
	  (goto-char new-point)
	  (setq new-point nil)))
    (listen-set-last)))

(defun listen-push-mark ()
  (if (or (not (mark t))
	  (/= (mark t) (point)))
      ;; t means "no message":
      (push-mark nil t)))

(defun listen-push-mark-if-prev-not-movement ()  
  (if (and (not mark-active)
	   (not (listen-get-assoc-command-status 'movement)))
      (listen-push-mark)))

(defun listen-set-mark-command ()
  (interactive)
  (when (or (not (mark t))
	    (/= (mark t) (point)))
    (cond (cua-mode
	   (if mark-active
	       ;; otherwise, CUA would clear the mark
	       (setq mark-active nil))
	   (cua-set-mark))
	  (t (set-mark-command nil))))
  (setq mark-active t))

;;; TEXT MOVEMENT COMMANDS

(defun cut-command (words)
  (interactive (list (listen-event-command-words last-command-event)))
  ;; set default position to 'here (cursor (point) position)  
  (unless (listen-argument listen-common-positions words t)
    (setq words (concat words " " (symbol-name listen-here-word))))
  (let* ((scope (listen-scope-argument words))
	 (source
	  (listen-buffer-region-near-common-position-this-or-that
	   words)))    
    (with-current-buffer (car source)
      (listen-push-mark-if-prev-not-movement)
      (kill-region (cadr source) (cddr source))
      (if (not (eq scope 'char))
	  (listen-fix (car (cdr source)) t)))))

(defun delete-command (words)
  (interactive (list (listen-event-command-words last-command-event)))
  ;; Set default position to 'here (cursor (point) position)  
  (unless (listen-argument listen-common-positions words t)
    (setq words (concat words " " (symbol-name listen-here-word))))
  (let* ((scope (listen-scope-argument words))
	 (source
	  (listen-buffer-region-near-common-position-this-or-that
	   words)))
    (with-current-buffer (car source)
      (save-excursion
	(delete-region (cadr source)
		       (cddr source))
	(if (not (eq  scope 'char))
	    (listen-fix (cadr source) t))))))

(defun copy-command (words)
  (interactive (list (listen-event-command-words last-command-event)))
  ;; set default position to 'here (cursor (point) position)  
  (unless (listen-argument listen-common-positions words t)
    (setq words (concat words " " (symbol-name listen-here-word))))
  
  (let* ((source
	  (listen-buffer-region-near-common-position-this-or-that
	   words))) 
    (with-current-buffer (car source)
      (copy-region-as-kill (cadr source) (cddr source))
      (message "Copied: \"%s\"" (car kill-ring-yank-pointer)))))

(defun choose-command (words)
  (interactive (list (listen-event-command-words last-command-event)))
  ;; set default position to 'here (cursor (point) position)  
  (unless (listen-argument listen-common-positions words t)
    (setq words (concat words " " (symbol-name listen-here-word)))) 
  (let* ((scope (listen-scope-argument words))
	 (source-region
	  (unless (eq scope 'that)
	    (listen-region-near-common-position-this-or-that
	     words))))
    (cond ((not (eq scope 'that))
	   (with-current-buffer 
	       (listen-buffer-near-common-position-this-or-that 
		words)
	     (listen-make-region (car source-region) 
				 (cdr source-region))))
	  (t (when (mark t))
	     (let ((mark-even-if-inactive t))
	       (listen-make-region (mark) (point)))))))

(defun listen-simple-yank-command (number)
  (let* ((text (current-kill (1- number) t)))    
    (when text
      (listen-insert-string text)))) 

(defun listen-yank-command (words &optional repl)
  (interactive (list (listen-event-command-words last-command-event)))
  (let ((n (listen-number-argument words))
	(replace (or repl (eq (listen-argument
			       listen-replace-policy words t) 'on))))
    (if replace
	;; then n is part of the structure designation
	(let ((region
	       (listen-region-near-common-position-this-or-that
		words n))
	      (buffer
	       (listen-buffer-near-common-position-this-or-that
		words)))
	  (with-current-buffer buffer
	    (delete-region (car region) (cdr region))
	    (goto-char (car region))
	    (listen-simple-yank-command 1)))
      ;; else n designates the entry
      (listen-do-at-position (list 'listen-simple-yank-command n)
			     nil nil t))))

(defun listen-pop-command (words)
  (interactive (list (listen-event-command-words 
		      last-command-event)))
  (listen-simple-do-at-position 'yank)
  (when kill-ring 
    (setq kill-ring (cdr kill-ring))
    (setq kill-ring-yank-pointer kill-ring)))

(defun listen-text-from-common-position-to-point (words &optional repl move)
  (interactive (list (listen-event-command-words last-command-event)))
  ;; set default position to 'there (mouse position)
  (if (member (listen-argument listen-common-positions words t)
	      '(nil this that))
      (setq words (concat words " " (symbol-name listen-there-word))))
  (let* ((scope (listen-scope-argument words))
	 (source (listen-buffer-region-near-common-position-this-or-that
		  words))
	 (replace (or repl
		      (eq (listen-argument
			   listen-replace-policy words t) 'on)))
	 (region (if replace
		     (locate-bounds-of-things-near-point scope 1)))
	 (replace-marker (if replace
			     (set-marker (make-marker)
					 (car region))))
	 (text (with-current-buffer (car source)
		 (prog1 (buffer-substring 
			 (cadr source) 
			 (cddr source))
		   (if move
		       (listen-delete-region (cadr source) 
					     (cddr source)))))))
    (if replace
	(listen-delete-region 
	 replace-marker
	 ;; this arithmetic is necessary for replace-marker may have moved
	 (+ replace-marker (abs (- (car region)
				   (cdr region))))))
    (if (eq scope 'char)
	(listen-insert-simple text)
      (listen-insert-string text))
    ;; gc replace-marker
    (setq replace-marker nil)))

(defun listen-text-from-point-to-common-position (words &optional repl move)
  (interactive (list (listen-event-command-words last-command-event)))
  ;; set default position to 'there
  (if (member (listen-argument listen-common-positions words t)
	      '(nil this that))
      (setq words (concat words " " (symbol-name listen-there-word))))
  (let* ((scope (listen-scope-argument words))
	 (number (listen-number-argument words))
	 (position (listen-position-argument words))
	 (region  (locate-bounds-of-things-near-point scope number))
	 (replace (or repl
		      (eq (listen-argument
			   listen-replace-policy words t) 'on)))
	 (target-buffer
	  (listen-buffer-near-common-position-this-or-that
	   words))
	 (target-region
	  (if replace
	      (listen-region-near-common-position-this-or-that 
	       words 1)))
	 (target-position 
	  (if (and (eq scope 'that) (eq position 'mark))
	      ;; to just copy the current selection is not terribly
	      ;; useful--instead, copy the selection to the value of
	      ;; mark before mark was used to delineate current
	      ;; selection
	      (let ((second-mark (car-safe mark-ring)))
		(or second-mark
		    (error "A previous mark position does not exist")))
	    ;; the normal case
	    (listen-simple-do-at-position '(point) words)))
	 (target-position-marker (set-marker 
				  (make-marker)
				  target-position
				  target-buffer)))
    (let ((text (buffer-substring (car region) (cdr region)))
	  (pos (point-marker)) 
	  (replace-marker (set-marker (make-marker)
				      (car target-region)
				      target-buffer)))
      (if move
	  (listen-delete-region (car region) (cdr region)))
      (with-current-buffer target-buffer
	(save-excursion
	  (cond (replace ;; delete the region starting at the
		 ;; replace-marker and place cursor there
		 (listen-delete-region 
		  replace-marker
		  (+ replace-marker (abs (- (car target-region)
					    (cdr target-region)))))
		 (goto-char replace-marker))
		(t ;; otherwise, place cursor at the target-position
		 (goto-char target-position-marker))) 
	  (if (eq scope 'char)
	      (listen-insert-simple text)
	    (listen-insert-string text))))
      ;; ease gc'ing of markers
      (set-marker replace-marker nil)
      (set-marker target-position-marker nil))))
 
(defun transpose-command (words)
  (interactive (list (listen-event-command-words last-command-event)))
  (listen-simple-do-at-position `(swap-command ,words t)))

(defun swap-command (words &optional transpose)
  "Normally, this command expects a designation of a non-here
position.  The structure at point (as designated by scope and number)
is exchanged with the structure at position (designated by scope).
If TRANSPOSE non-nil, the exchange is done instead with the structure
following the first identified structure at point."
  (interactive (list (listen-event-command-words last-command-event)))
  (listen-set-last)
  (let* ((scope (listen-scope-argument words))
	 (number (listen-number-argument words))
	 (position (let ((pos (listen-position-argument words)))
		     (cond ((eq pos 'here)
			    (setq words 
				  (concat words
					  " "
					  (symbol-name
					   (listen-argument-inverse
					    listen-common-positions
					    'there))))
			    'there)
			   (pos))))
	 (listen-swap-command-result)
	 (no-fix-flag (eq scope 'char))
	 ;; the function below calculates the first region
	 (at-position-form
	  `(setq listen-swap-command-result
		 (cons
		  (bounds-of-a-number-of-things-at-or-near-point
		   (quote ,scope) ,number)
		  (current-buffer))))
	 (region-2))
    (if (not transpose)
	(listen-simple-do-at-position at-position-form words)
      (setq listen-swap-command-result
	    (cons (bounds-of-a-number-of-things-at-or-near-point 
		   scope number)
		  (current-buffer))))
    (if (null (car listen-swap-command-result))
	(message "%s(s) not near position '%s'"
		 scope position)
      (if (or (eq position 'here) transpose)
	  ;; then the first region denoted by scope and number is around
	  ;; point, so, the second region, region-2, is what is after this
	  ;; region as indicated by scope (and no number)
	  (setq region-2
		(save-excursion
		  (goto-char (cdar listen-swap-command-result))
		  (if (not no-fix-flag)
		      (re-search-forward "[-,.: \n\t]*"))
		  (let ((bnds (bounds-of-thing-at-point scope)))
		    (cond ((or (null bnds) (< (car bnds)
					      (cdar
					       listen-swap-command-result)))
			   (forward-char 1)
			   (bounds-of-thing-at-point scope))
			  (bnds)))))
	;; then region-2 is what is at point
	(setq region-2
	      (bounds-of-thing-at-point scope)))
      (if (null region-2)
	  (message "%s not at destination" scope)
	(let ((x1 (caar listen-swap-command-result))
	      (y1 (cdar listen-swap-command-result))
	      (x2 (car region-2))
	      (y2 (cdr region-2)))
	  (if (and (eq (current-buffer)
		       (cdr listen-swap-command-result))
		   (or (and (> y1 x2) (< y1 y2))
		       (and (> y2 x1) (< y2 y1))))
	      (message "regions overlap")
	    ;; now, we are ready to do the swap
	    (let* ((text2 (buffer-substring x2 y2))
		   (marker2 (progn (goto-char x2) (point-marker)))
		   (marker2-do-not-move 
		    (set-marker(make-marker)
			       (marker-position marker2)))
		   (marker1)
		   (text1 (with-current-buffer
			      (cdr listen-swap-command-result)
			    (prog1
				(buffer-substring x1 y1)
			      (delete-region x1 y1)
			      (goto-char x1)
			      (setq marker1 (point-marker))
			      (set-marker-insertion-type marker1 t)
			      (set-marker-insertion-type marker2 t)
			      (listen-insert-string text2
						    no-fix-flag)))))
	      (goto-char marker2)
	      (delete-region marker2 (+ marker2 (- y2 x2)))   
	      (set-marker-insertion-type marker2 nil)
	      (listen-insert-string text1 no-fix-flag)
	      (set-buffer (cdr listen-swap-command-result))
	      (goto-char marker2))))))))

(defun kill-command (words)
  (interactive (list (listen-event-command-words last-command-event)))
  (listen-set-last)
  (let* ((scope (listen-scope-argument words))
	 (number (listen-number-argument words))
	 (old-pos (point)))
    ;; takes care of negative number as well:
    (if (member scope '(line))
	(goto-char (or (cdr-safe
			(bounds-of-thing-at-point scope))
		       (point)))
      (forward-thing scope number))
    (delete-region old-pos (point))
    (if (not (eq scope 'char))
	(listen-fix (point) t))))

(defun erase-command (words)
  (interactive (list listen-event-command-words))
  (let* ((scope (listen-scope-argument words))
	 (number (listen-number-argument words))
	 (old-pos (point)))
    (if (member scope '(line line-inclusive)) 
	(progn
	  (beginning-of-line (or (- 2 number) 1)))
      (forward-thing
       scope
       (- (or number 1))))
    (delete-region old-pos (point))
    (if (not (eq scope 'char))
	(listen-fix (point) t))))

(defun indent-command (words) 
  (interactive (list listen-event-command-words))
  ;; Set default position to 'here (cursor (point) position)  
  (unless (listen-argument listen-common-positions words t)
    (setq words (concat words " " (symbol-name listen-here-word))))
  (unless (listen-argument listen-scopes words t)
    (setq words (concat words " " (symbol-name 'line-inclusive))))
  (let* ((source
	  (listen-buffer-region-near-common-position-this-or-that
	   words)))
    (with-current-buffer (car source)
      (save-excursion
	;; hack because indent-region in Lisp mode sometimes indents one
	;; line too many
	(let ((b (cadr source)) (e (cddr source)))
	  (if (string-match "\\n" (buffer-substring b e))
	      (listen-indent-region b e)
	    ;; otherwise, we were asked to act on one line only, do not
	    ;; to any chances with indent-region
	    (goto-char b)
	    (indent-according-to-mode)))))))
			    
(defun listen-indent-region (b e)
  (indent-region b e nil))

(defun indent-rigidly-command (indentation-level &optional words)
  (interactive (list listen-event-command-words))
  (let* ((source
	  (listen-buffer-region-near-common-position-this-or-that
	   words)))
    (with-current-buffer (car source)
      (save-excursion
	(indent-rigidly (cadr source)
			(cddr source)
			indentation-level)))))

(defun comment-command (&optional argument words)
  (interactive (list listen-event-command-words))
  (let* ((source
	  (listen-buffer-region-near-common-position-this-or-that
	   words)))
    (with-current-buffer (car source)
      (save-excursion
	(comment-region (cadr source)
			(cddr source)
			argument)))))

(defun generic-paren-pair (words)
  (interactive (list listen-event-command-words))
  (let* ((paren-type (listen-argument listen-paren-registry words)))
    (let* ((start-chars (car paren-type))
	   (end-chars (cdr paren-type)))
      (listen-set-last)
      (if mark-active
	  (let ((start) (end (region-end)))
	    (goto-char (region-beginning))
	    (setq start (point-marker))
	    ;; fix below may insert spaces at beginning of line, mark
	    ;; clings to the end of inserted text
	    (set-marker-insertion-type start t)
	    (goto-char end)
	    (insert end-chars) 
	    (listen-fix (point))
	    (setq end (point-marker))
	    (goto-char start)
	    (insert start-chars) 
	    (listen-fix (point)))
	(let (start middle)
	  (setq start (point))
	  (insert "")
	  (insert start-chars)
	  (insert "x")
	  (setq middle (point-marker))
	  (insert end-chars)
	  (listen-fix (point))
	  (listen-fix start t)
	  (goto-char middle)
	  (delete-char -1)))
      (listen-set-last))))

(defun listen-unpair-command (words)
  (interactive (list listen-event-command-words))
  (let* ((paren-type (listen-argument listen-paren-registry words))
	 bounds)
    (if (catch 'notfound
	  (save-excursion
	    (if (thing-at-point-looking-at (regexp-quote (car paren-type)))
		(goto-char (match-end 0)))
	    (if (not (search-backward (car paren-type) nil 'move))
		(throw 'notfound t)
	      (save-excursion
		(let ((start (point)))
		  ;; step just past the left paren
		  (forward-char 1)
		  (let ((bounds (bounds-of-thing-at-point 'block))
			(next (search-forward (cdr paren-type) nil
					      'move)))
		    (cond
		     ;; try to use match found by
		     ;; bounds-of-thing-at-point
		     ((and bounds (eq start (car bounds)))
		      (goto-char (cdr bounds))
		      (delete-backward-char 
		       (length (cdr paren-type))))
		     ;; otherwise, use the delimiter found by search
		     (next
		      (delete-backward-char (length (cdr paren-type))))
		     (t (throw 'notfound t)))
		    (goto-char start)
		    (delete-char (length (car paren-type)))))))))
	(error "No enclosing %s...%s found" 
	       (car paren-type) (cdr paren-type)))))

(defun listen-beginning-end (words)
  (interactive (list listen-event-command-words))
  (let* ((scope (let ((scope-temp (listen-scope-argument words)))
		  (if (eq scope-temp 'line-inclusive) 'line scope-temp)))
	 (extremity (listen-argument listen-extremities 
				     words))
	 (number (listen-number-argument words)))
    (assert (wholenump number))
    (assert (member extremity '(beginning end)))
    (listen-add-assoc-command-status '(movement))
    (listen-push-mark-if-prev-not-movement)
    (while (> number 0)
      (decf number)
      (cond  ((eq extremity 'beginning)
	      (let ((bounds
		     (bounds-of-thing-at-or-ending-at-point 
		      scope)))
		(if bounds (goto-char (car bounds)))))
	     ((eq extremity 'end)
	      (if (get scope 'end-op) 
		  (funcall (get scope 'end-op))
		(let ((bounds
		       (bounds-of-thing-at-or-beginning-at-point 
			scope)))
		  (if bounds (goto-char (cdr bounds))))))))))

(defun begin-comment ()
  (interactive)
  (if (not comment-start) 
      (error "No comment syntax is defined")
    (listen-insert-simple comment-start))) 

(defun end-comment ()
  (interactive)
  (if (not comment-end) 
      (error "No comment end syntax is defined")
    (if (string= comment-end "")
	(newline)
      (listen-insert-simple comment-end))))

(defun listen-scroll (&optional words direction)
  (interactive (list listen-event-command-words))
  (unless words (setq words ""))
  (let* ((number (listen-argument listen-numbers words t))
	 (vnumber (listen-argument listen-variation-numbers words t))
	 (h    (1- (window-height)))
	 lines)
					;     (setq buffer-undo-list
					;	   (cons (point)
					;		 buffer-undo-list))
    (if (not (or vnumber number))
	(if (eq direction 'up)
	    (setq number 5)
	  (setq  vnumber 5)))
    (listen-add-assoc-command-status '(movement))
    (listen-push-mark-if-prev-not-movement)
    (condition-case nil
	(if number  
	    ;; page down
	    (scroll-up (1- (/ (* number h) 5)))
	  ;; page up
	  (scroll-down (1- (/ (* vnumber h) 5))))
      (error nil))))

(defun listen-hyphen ()
  (interactive)
  (insert "-")
  (delete-horizontal-space)
  (delete-horizontal-space))

(defun listen-hyphenate-region (beginning end)
  (interactive "r")
  (save-excursion
    (let ((b (min beginning end))
	  (e (set-marker (make-marker) (max beginning end))))
      (goto-char b)
      (while (re-search-forward "\\> \\<" e t)
	(replace-match "-" nil nil))
      (setq e nil))))

(defun listen-just-one-space-in-region (beginning end)
  (interactive "r")
  (save-excursion
    (let ((b (min beginning end))
	  (e (set-marker (make-marker) (max beginning end))))
      (goto-char b)
      (while (re-search-forward "[ \t]+" e t)
	(just-one-space))
      (setq e nil))))

(defun listen-no-space-in-region (beginning end)
  (interactive "r")
  (save-excursion
    (let ((b (min beginning end))
	  (e (set-marker (make-marker) (max beginning end))))
      (goto-char b)
      (while (re-search-forward "[ \t]+" e t)
	(delete-horizontal-space))
      (setq e nil))))

(defun listen-insert-empty-element (name)  
  (interactive "sEmpty element name: ")
  (listen-insert (concat "<" name "/>")))

(defun listen-insert-element (&optional insert-newlines name)  
  (interactive)
  (let ((pos (point-marker))) 
    (if mark-active
	(if (and (not insert-newlines)
		 (or (equal 1 (count-lines (region-beginning)
					   (region-end)))
		     ;; something before (region-beginning) on the
		     ;; line of (region-beginning)
		     (or (save-excursion (goto-char (region-beginning))  
					 (re-search-backward
					  "^\\S-*\\s-.*\\=" nil t)))
		     ;; something before (region-end) on the line of (region-end)
		     (or (save-excursion (goto-char (region-end))  
					 (re-search-backward
					  "^\\S-*\\s-.*\\=" nil t)))))
	    ;; don't put in newlines
	    (cond (name 
		   (goto-char (point))
		   (insert "<" name ">")
		   (goto-char (mark))
		   (insert "</" name ">"))
		  (t (call-interactively 'sgml-tag)))
	  ;; insert newlines
	  (let* ((element-name
		  (if name 
		      name
		    (save-excursion (goto-char (region-beginning))
				    (completing-read
				     "Tag region with element: "
				     nil))))
		 (start (region-beginning))
		 (new-end (prog1
			      (progn (goto-char (region-end))
				     (point-marker))
			    (if (not 
				 (save-excursion (re-search-backward 
						  "^\\s-*\\=" nil t)))
				;; something before point on this line
				(insert-before-markers "\n")
			      ;; else, it's all blank before, if
			      ;; if the whole line, in face, and the
			      ;; the next one is blank, then don't
			      ;; do anything more, otherwise, insert
			      ;; a newline
			      (if (not (looking-at "\\s-*[\n]\\s-*$"))
				  (newline-and-indent)))))
		 (new-start (progn (goto-char start) (newline 1)
				   (point-marker))))
	    (set-marker-insertion-type new-start t)
	    (goto-char (point))
	    (insert "<" element-name ">")
	    (goto-char new-end)
	    (insert "</" element-name ">")
	    (indent-region (marker-position new-start)
			   (progn (goto-char (marker-position new-end))
				  (forward-line 2) (point))
			   nil)
	    (goto-char new-start)
	    (delete-horizontal-space)
	    (save-excursion
	      (previous-line 1)
	      (delete-blank-lines))
	    (set-marker new-start nil)
	    (set-marker new-end nil)))
      ;; so, mark not active---   
      ;; is buffer empty (only spaces and newlines)?
      (if (save-excursion
	    (beginning-of-buffer)
	    ;; is everything, from point to the end of the buffer, a whitespace?
	    (re-search-forward "\\=\\s-*\\'" nil t))
	  (progn
	    (unless name
	      (setq name
		    (read-string "Document element: " "")))
	    (insert "<" name ">" "\n")
	    (save-excursion 
	      (insert "\n</" name ">")))
	(indent-according-to-mode)
	(when insert-newlines
	  (if (not (save-excursion (re-search-backward "^\\s-*\\=" nil t)))
	      (newline-and-indent))
	  (when (looking-at ".*\\S-.*$")
	    (save-excursion
	      (newline-and-indent))))
	(if name (sgml-tag name)
	  (call-interactively 'sgml-tag))
	(when insert-newlines
	  (save-excursion
	    (newline-and-indent))))
      (save-excursion (goto-char pos) (listen-fix pos)))))

(defun listen-insert-XML-element ()
  (interactive)
  (listen-call-interactively-identifier 'listen-insert-element))

(defun listen-insert-XML-element-new-lines ()
  (interactive)
  (listen-call-interactively-identifier 
   '(listen-insert-element t)))

(defun listen-start-tag ()
  (interactive)
  (indent-according-to-mode)
  (listen-insert-simple "<>")
  (goto-char (- (point) 1)))

(defun listen-end-tag ()
  (interactive)
  (sgml-close-tag)
  (indent-according-to-mode))

(defun listen-empty-element ()
  (interactive)
  (listen-insert-simple "</>")
  (goto-char (- (point) 2))
  (indent-according-to-mode))

(defun listen-kill-element ()
  (interactive)
  (message "not tested")
  (sgml-delete-tag 1)
  (indent-according-to-mode))

(defun listen-erase-element ()
  (interactive)
  (message "not tested")
  (sgml-skip-tag-backward 1)
  (sgml-delete-tag 1)
  (indent-according-to-mode))

(defun listen-change-element ()
  (interactive)
  (message "not implemented currently"))

(defun listen-no-tag ()
  (interactive)
  (save-excursion
    (let* ((e (search-forward-regexp "[<>]" nil t))
	   (start (if (equal (char-before) ?>)
		      (search-backward-regexp "<")
		    (1- e)))
	   end)
      (when e
	(if (looking-at "[^>]*[/][>]") ;; empty element
	    (delete-region start (match-end 0))
	  ;; non-empty element
	  (goto-char start)
	  (if (looking-at "[<][/]") ;; end tag
	      (setq start (save-excursion 
			    (sgml-skip-tag-backward 1)
			    (point))))
	  (goto-char (1+ start))
	  (sgml-skip-tag-forward 1)
	  (search-backward-regexp "</.\\(\\w\\|\\s-\\)*>\\=")
	  (delete-region (match-beginning 0) (match-end 0))
	  (beginning-of-line)
	  (if (looking-at "[ \t]*$")
	      (delete-region (match-beginning 0) (1+ (match-end 0))))
	  (setq end (point))
	  (goto-char start)
	  (search-forward-regexp "[^>]*[>]")
	  (delete-region (match-beginning 0) (match-end 0))
	  (beginning-of-line)
	  (if (looking-at "[ \t]*$")
	      (delete-region (match-beginning 0) (1+ (match-end 0))))
	  (indent-region (point) end nil))))))

(defun listen-do-at-mark-position (command-or-sexp)
  "Execute command-or-sexp at the position of mark."
  (let ((m (mark t)))
    (if m
	(listen-do-at-pos-buffer 
	 command-or-sexp
	 m
	 (current-buffer)))))

(defun listen-do-at-last-word-position (command-or-sexp)
  "Execute command-or-sexp at the position different from point
  where something last was affected by a voice command."
  (let ((last (listen-last-buffer-position)))
    (if last
	(listen-do-at-pos-buffer 
	 command-or-sexp
	 (cdr last)
	 (car last)))))

(defun listen-go-last ()
  (interactive)
  (let* ((last (listen-last-buffer-position))
	 (w (if last (get-buffer-window (car last)))))
    (when w
      (select-window w)
      (goto-char (cdr last))))) 

(defun listen-do-at-pos-buffer (command-or-sexp pos buffer)
  "Execute command-or-sexp at position POS in buffer BUFFER."
  (listen-execute-events-as-macro 
   (list
    (list 'listen-execute-now command-or-sexp)
    (list 'listen-return-from-excursion 
	  (current-buffer) (point-marker))))
  (set-buffer buffer)
  (goto-char pos))

(defun listen-do-movement-command (command)
  "Execute COMMAND, which is a symbol bound to an interactive command,
override the value of this-command with COMMAND and make the
command-status to that of 'movement."
  (assert (commandp command)) 
  (let ((counter 0)
	(value-of-binding (if (symbolp command)
			      command
			    (key-binding command))))    
    (listen-do-at-position 
     (list
      'progn
      '(listen-add-assoc-command-status '(movement)) 
      ;; listen-do-at-position may issue the same interactive command
      ;; many times, but consider pushing mark only the first time! note
      ;; nice/ugly use of dynamic binding
      '(if (= counter 0) (listen-push-mark-if-prev-not-movement))
      '(incf counter)
      (list 'call-interactively (list 'quote value-of-binding))
      (list 'setq 'last-command (list 'quote value-of-binding))
      (list 'setq 'this-command (list 'quote value-of-binding))))))

(defvar listen-do-at-marker
  "The position of point while listen-do-at-position excurses to where
the mouse is.")

(defun listen-do-at-position (command-or-sexp &optional 
					      movement
					      explicit-position
					      ignore-number
					      words)
  "Execute the command or sexp COMMAND-OR-SEXP at point
or at mouse or at the last position where some change happened
before point.  If movement is t, then treat the command as a movement
command."
  (if (null words)
      (setq words (listen-event-command-words last-command-event)))
  (when movement
    (listen-add-assoc-command-status '(movement))
    (listen-push-mark-if-prev-not-movement))
  (let ((position (listen-argument listen-common-positions words t))
	(number (if (not ignore-number)
		    (listen-number-argument words)
		  1)))
    ;; make all what is stuffed into unread-command-events appear
    ;; as secondary (the 'listen-primary-events will appear at then
    ;; end of all the events that are stuffed in the following)
    (setq unread-command-events
	  (cons 
	   'listen-primary-events
	   unread-command-events))
    (setq listen-secondary-events t)
    (do ((i 0 (1+ i))) ((eq i number)) 
      (cond 
       (explicit-position
	(listen-execute-events-as-macro 
	 (list
	  '(listen-execute-now 
	    (setq listen-do-at-marker (point-marker)))
	  (list 'listen-execute-now 
		(list 'goto-char explicit-position))
	  (list 'listen-execute-now command-or-sexp)
	  '(listen-execute-now
	    (progn
	      (goto-char listen-do-at-marker)
	      (set-marker listen-do-at-marker nil))))))
       ((eq position 'there)
	;; put clean up event in queue
	(listen-execute-events-as-macro 
	 (list
	  'listen-click
	  (list 'listen-execute-now command-or-sexp)
	  (list 'listen-return-from-click-excursion 
		(selected-window) (point-marker)))))
       ((eq position 'go)
	(listen-add-assoc-command-status '(movement))
	(listen-push-mark-if-prev-not-movement)
	(listen-execute-events-as-macro 
	 (list
	  'listen-click
	  (list 'listen-execute-now command-or-sexp)))) 
       ((eq position 'last)
	(listen-do-at-last-word-position command-or-sexp))
       ((eq position 'mark)
	(listen-do-at-mark-position command-or-sexp))
       (t (listen-execute command-or-sexp))))))

(defun listen-do-position-or-region (position-function
				     &optional region-function 
				     default-is-here default-scope
				     words)
  "If the scope indicated by the current listen command is a position,
then apply POSITION-FUNCTION; otherwise, assume that the scope is a
region and apply REGION-FUNCTION. If DEFAULT-IS-HERE is t and no
position or scope is indicated, then the default position 'here is
assumed (otherwise, in this situation, when DEFAULT-IS-HERE is nil,
the listen-default-scope is used)."
  (if (null words)
      (setq words (listen-event-command-words last-command-event)))
  (let ((case-fold-search nil)		; is that right?  
	(position (listen-argument listen-common-positions words t)))
    (cond 
     ((eq position 'that)
      (if mark-active
	  (apply region-function
		 (dotted-pair-to-list (bounds-of-thing-at-point
				       'that)))))
     ((eq position 'this)
      (apply region-function
	     (dotted-pair-to-list (bounds-of-thing-at-point 'this))))
     ((and position
	   (not (listen-argument listen-numbers words t))
	   (not (listen-argument listen-variation-numbers words t)))
      (listen-do-at-position position-function))
     ((or (and
	   (not default-scope)
	   (not (listen-argument listen-scopes words t))
	   default-is-here)
	  (and
	   ;; there is a default scope and also "here" is the
	   ;; default position, now check that no number was
	   ;; indicated (since that would pertain to the default
	   ;; scope) and that no scope was given
	   default-scope
	   (not (listen-argument listen-numbers words t))
	   (not (listen-argument listen-variation-numbers words t))
	   (not (listen-argument listen-scopes words t))
	   default-is-here))
      (listen-do-at-position position-function))
     (t (listen-simple-do-at-position
	 `(let ((thing (bounds-of-a-number-of-things-at-point
			(listen-scope-argument ,words)
			(listen-number-argument ,words))))
	    (funcall region-function (car thing) 
		     (cdr thing))))))))

(defun listen-predicate-argument (prompt)
  (cond ((and (string= listen-event-argument-words "")
	      (not listen-repeating))
	 (listen-set-undo-action 'keyboard-escape-quit)
	 (setq listen-event-argument-words
	       (read-string prompt "" 'listen-search-history)))
	((and (string= listen-event-argument-words "")
	      listen-repeating)	 
	 (setq listen-event-argument-words 
	       listen-last-argument-words)))
  (setq listen-last-argument-words listen-event-argument-words)
  listen-event-argument-words)

;;; CAPITALIZATION

(defun listen-capitalize ()
  (interactive)
  (listen-do-position-or-region 
   'capitalize-word 
   'capitalize-region
   t
   t))

(defun listen-downcase ()
  (interactive)
  (listen-do-position-or-region 'downcase-word 'downcase-region t t))

(defun listen-upcase ()
  (interactive)
  (listen-do-position-or-region 'upcase-word 'upcase-region t t))

(defun listen-last-mark (number)
  (assert (integerp number))
  (if (> number 0)
      (let ((i number)) 
	(while (> i 0) 
	  (setq listen-mark-undone-list
		(cons (point-marker)
		      ;; if we just had another last-mark operation, then append
		      ;; to that list
		      (if listen-mark-last-undo-ready-last
			  listen-mark-undone-list
			;; otherwise, a new list
			nil))
		listen-mark-last-undo-ready t)
	  ;; pop the mark and change point:
	  (set-mark-command t)
	  (decf i)))
    ;; number < 0
    (let ((i number)) 
      (while (and (< i 0) 
		  (consp listen-mark-undone-list))
	;; push the mark
	(push-mark (point) t)
	(goto-char (car listen-mark-undone-list))
	(set-marker (car listen-mark-undone-list) nil)
	(setq listen-mark-undone-list (cdr listen-mark-undone-list))
	(setq listen-mark-last-undo-ready t)
	(incf i)))))



;;; EXTRACTING ARGUMENTS FROM LISTEN COMMANDS

(defun listen-number-argument (&optional words)
  (if (null words) 
      (setq words (listen-event-command-words 
		   last-command-event))) 
  (assert (stringp words))
  (or 
   (let ((n (listen-argument listen-numbers 
			     words t)))
     n)
   (let ((n (listen-argument listen-variation-numbers 
			     words t)))
     (if n (- n)))
   1))

(defun listen-extract-number (str)
  (let ((found nil)
	(list-of-numbers listen-numbers))
    (while (and (consp list-of-numbers) (not found))
      (if (string-match (symbol-name (caar list-of-numbers)) (downcase str))
	  (setq found t)
	(setq list-of-numbers (cdr list-of-numbers))))
    (if found  (cdar list-of-numbers) 1)))

(defun listen-position-argument (&optional words)
  (if (null words) 
      (setq words (listen-event-command-words 
		   last-command-event)))
  (assert (stringp words))
  (or
   (listen-argument listen-common-positions words t)
   listen-default-position))

(defun listen-scope-argument (&optional words)
  (if (null words) 
      (setq words (listen-event-command-words 
		   last-command-event))) 
  (assert (stringp words))
  (or (listen-argument listen-scopes words t)
      listen-default-scope))

(defun listen-search-argument (&optional words)
  (if (null words) 
      (setq words (listen-event-command-words 
		   last-command-event)))
  (assert (stringp words))
  (listen-argument listen-search-keywords words))

(defun listen-penultimate-argument (&optional words)
  (if (null words) 
      (setq words (listen-event-command-words 
		   last-command-event)))
  (assert (stringp words))
  (listen-argument listen-penultimate-keywords words t))


(defun listen-massage-dot (str)
  "Some constituents of commands are of the form 'X. word'.  This
representation is converted into a symbol with name 'x\ word'."
  (let ((i 0))
    (if (eq (length str) 0)
	""
      (while (< i (1- (length str)))
	(when (string= (substring str i (+ i 2)) ". ")
	  (when (> i 0)
	    (setq str (concat (substring str 0 (- i 1))
			      (capitalize (substring str (- i 1) i))
			      "\\.\\ "
			      (substring str (+ i 2))))
	    (incf i)))
	(incf i))
      str)))

(defun listen-argument (registry &optional words do-not-complain)
  (if (not (stringp words))
      (setq words (listen-event-command-words 
		   last-command-event)))
  (let* ((word-list (car-safe
		     (condition-case nil
			 (read-from-string 
			  (listen-massage-dot (concat "(" words
						      ")")))
		       (error nil))))
	 found)
    (while (and (consp registry)
		(or (and (consp (car registry))
			 (or (symbolp (car (car registry)))
			     (numberp (car (car registry)))))
		    (error "Registry is not a list of dotted pairs, each having a car that is a symbols"))
		(not (setq found 
			   (string-match
			    (concat "\\<" (format "%s" (car (car
							     registry))) 
				    "\\>")
			    words))))
      (setq registry (cdr registry)))
    (or (and found (cdr (car registry)))
	(and (not do-not-complain)
	     (error "listen-argument: not found %s" registry)))))

(defun listen-argument-inverse (registry sym)
  (while (and registry (not (eq sym (cdr (car registry)))))
    (setq registry (cdr registry)))
  (if registry (car (car registry))))


(defun listen-move-point (words)
  (interactive (list listen-event-command-words))
  (let* ((number (listen-number-argument words))
	 (extremity (listen-argument listen-extremities words t))
	 (scope (let ((scope-temp (listen-scope-argument words)))
		  (if (eq scope-temp 'line-inclusive) 'line scope-temp)))
	 ;; (listen-scope-argument words))
	 (direction (listen-argument listen-directions words t)))
    (listen-add-assoc-command-status '(movement)) 
    (listen-push-mark-if-prev-not-movement)
    (if (not extremity) 
	(setq extremity 'beginning))
    (assert (member extremity '(beginning end)))
    (if (and (> number 0) (eq direction 'backward))
	(setq number (- number)))
    (next-thing scope number)
    (if (eq extremity 'end)
	(or (when (get scope 'end-op)
	      (funcall (get scope 'end-op))
	      t)
	    (goto-char (cdr (bounds-of-thing-at-point scope)))))))

(defun listen-move-according-to-phonetic (words)
  (interactive (list (listen-event-command-words 
		      last-command-event))) 
  (let* ((number (listen-number-argument words))
	 (direction (listen-argument listen-directions words t))
	 (extremity (listen-argument listen-extremities words t)))
    (listen-add-assoc-command-status '(movement)) 
    (listen-push-mark-if-prev-not-movement)
    (assert (> number 0))
    (let ((reg-exp (regexp-quote 
		    (listen-argument listen-phonetic-registry)))
	  (reg-exp-with-repeats ""))
      (while (> number 0)
	(setq reg-exp-with-repeats (concat reg-exp-with-repeats reg-exp))
	(decf number))
      (if (eq direction 'forward)
	  (if (save-excursion (if (eq extremity 'beginning) (forward-char))
			      (re-search-forward reg-exp-with-repeats
						 nil t 1))
	      (if (eq extremity 'beginning)
		  (goto-char (match-beginning 0))
		(goto-char (match-end 0))))
	(if (save-excursion (if (eq extremity 'end) (forward-char -1))
			    (re-search-backward reg-exp-with-repeats
						nil t 1))
	    (if (eq extremity 'beginning)
		(goto-char (match-beginning 0))
	      (goto-char (match-end 0))))))))
   
;;; TOKEN SEARCH

(defvar listen-search-history nil
  "A list of the last searches in a human-readable form.")  
(defvar listen-search-history-length 0)
(defvar listen-search-history-tokenized nil
  "The list of token lists corresponding to listen-search-history.")

(defun listen-search-token (token direction skip-status &optional limit)
  ;; Returns (BEGIN END), where BEGIN is the beginning match for
  ;; TOKEN; END is the end of the match.
  (assert (member direction '(forward backward)))
  (assert (member skip-status '(skip skip-whitespace skip-space no-whitespace)))
  (assert (or (not (eq direction 'backward)) (eq skip-status 'skip)))
  (save-excursion 
    (let* (search-string ;; defined just to avoid compiler warnings
	   (old-point (point))
	   (search-for-string
	    (function
	     (lambda ()	;; (limit search-string)
	       (let ((modified-search-string
		      (cond ((eq skip-status 'skip-whitespace)
			     ;; don't depend on whitespace (\s-)---it
			     ;; doesn't include \n in some modes (like tex-mode)
			     (concat "[ \n\t]*" search-string))
			    ((eq skip-status 'skip-space)
			     (concat "[ \t]*" search-string))
			    (search-string)))
		     (case-fold-search t))
		 (if (cond 
		      ((and (eq direction 'forward)
			    (eq skip-status 'skip))
		       (re-search-forward modified-search-string limit t))
		      ((eq direction 'forward)
		       ;; and skip-status is 'no-whitespace,
		       ;; 'skip-space, or 'skip-whitespace
		       (looking-at modified-search-string))
		      ;; otherwise, we are going backwards and
		      ;; skip-status is 'skip 
		      ((re-search-backward modified-search-string limit t)))
		     (list (match-beginning 0) (match-end 0))))))))
      (cond ;; string search
       ((stringp token)
	(let ((search-string token))
	  (funcall search-for-string)))
       ;; search bounded by extent of thing, token is a pair
       ;; (thing search-string)
       ((and (consp token) (eq (car token) 'bounded))
	(let* ((thing (nth 1 token))
	       (thing-range (bounds-of-thing-at-point thing))
	       (limit (if (eq skip-status 'skip)
			  ;; ignore a 'bounded specification if it is
			  ;; the first token in the list
			  nil
			(if (eq direction 'forward) 
			    (cdr thing-range) 
			  (car thing-range))))
	       (search-token (nth 2 token)))
	  ;; override current skip status and use limit
	  (listen-search-token search-token direction 'skip
			       limit)))
       ;; skip letters
       ((eq token 'skip-letters)
	(if (eq direction 'forward)
	    (re-search-forward "[a-z0-9-_]*" nil t)
	  (re-search-backward "[a-z0-9-_]*" nil t))
	(list (match-beginning 0) (match-end 0)))
       ;; skip non-white space
       ((eq token 'skip-non-whitespace)
	(if (eq direction 'forward)
	    (re-search-forward "[^ \n\t]*" nil t)
	  (re-search-backward "[^ \n\t]*" nil t))
	(list (match-beginning 0) (match-end 0)))
       ;; space search
       ((and (consp token) (eq (car token) 'space))
	(let ((search-string ""))
	  ;; didn't work in Emacs 20.6: (make-string (nth 1 token) ?32)))
	  (let ((i (nth 1 token)))
	    (while (> i 0)
	      (decf i)
	      (setq search-string (concat search-string " "))))
	  (funcall search-for-string)))
       ;; newline search
       ((and (consp token) (eq (car token) 'newline))
	(let ((search-string ""))
	  (let ((i (nth 1 token)))
	    (while (> i 0)
	      (decf i)
	      ;; note that we regard the beginning and the end of the
	      ;; buffer as new lines 
	      (setq search-string
		    (concat search-string 
			    (if (eq direction 'forward)
				"\\(\n\\|\\'\\)"
			      "\\(\n\\|\\`\\)"))))
	    (funcall search-for-string))))
       ;; structural search
       ((and (consp token) (eq (car token) 'structural))
	(let (done found)
	  (while (not done)
	    (setq found
		  (bounds-of-a-number-of-things-at-point
		   (nth 1 token)
		   (nth 2 token)))
	    (if found
		(progn 
		  ;; possibility: not done if going backward and
		  ;; forward-thing stretched beyond original (point),
		  ;; same thing for forward direction
		  (setq done
			(and
			 (not (and (eq direction 'backward)
				   (< old-point (cdr found))))
			 (not (and (eq direction 'forward)
				   (< (car found) old-point)))))))
	    (if (not done)
		(if (eq direction 'backward)
		    (if (bobp)
			;; don't try any more
			(setq done t found nil)
		      ;; inefficient but who cares, step back a
		      ;; little and try again
		      (backward-char 1))
		  ;; forward
		  (if (eobp)
		      ;; don't try any more
		      (setq done t found nil)
		    (forward-char 1)))))
	  (if found
	      (list (car found) (cdr found)))))))))

(defun listen-search-for-token-list (position limit 
					      original-direction
					      return-range token-list
					      skip-status)
  "Only the search for the first token has status 'skip, the remaining
tokens are searched for with status 'skip-whitespace, unless the
preceding token was a space token, in which case the status is
'no-whitespace.  A token is either a string REG, denoting a regular
expression, or a list (structural THING NUMBER), or a list (space N),
or a list (newline N), or a list (bounded THING REG)."
  (assert (member skip-status '(skip skip-whitespace skip-space
				     no-whitespace)))
  (assert (consp token-list))
  (assert (member original-direction '(forward backward neighbor))) 
  (assert (member return-range '(first last all)))
  (assert (or (not (member original-direction '(backward neighbor)))
	      (wholenump limit)))
  (save-excursion
    (goto-char position)
    (let ((token (car token-list)))
      (catch 'done
	(while t
	  (let* ((direction-here 
		  (if (or (not (eq skip-status 'skip))
			  (eq original-direction 'forward))
		      'forward
		    'backward))
		 (found (listen-search-token 
			 token direction-here 
			 skip-status))
		 (b (nth 0 found))
		 (e (nth 1 found)))
	    (if (or (not (consp found))
		    ;; if found and original-direction is backward, then
		    ;; check that match does not extend further than limit
		    (and (eq original-direction 'backward)
			 (> e limit)))
		(throw 'done nil))
	    ;; (assert (consp found))
	    ;; more?  
	    (if (cdr token-list)
		;; this token was not last
		(let* ((new-skip-status
			(cond ((and (consp token) (eq (car token) 'space))
			       ;; current token specifies explicit space, so
			       ;; don't skip whitespace next
			       'no-whitespace)
			      ((and (consp (cadr token-list)) 
				    (eq (car (cadr token-list))
					'newline))
			       ;; the next token is a new line
			       ;; specification so don't skip
			       ;; new lines, only spaces
			       'skip-space)
			      ;; this is the norm
			      ('skip-whitespace)))
		       (recurse-result
			;; now, do the recursive call
			(listen-search-for-token-list 
			 e
			 limit
			 original-direction
			 return-range
			 (cdr token-list)
			 new-skip-status)))
		  (if (consp recurse-result)
		      (throw 'done  
			     (cond ((eq return-range 'first)
				    (cons b e))
				   ((eq return-range 'all)
				    (cons b (cdr recurse-result)))
				   ((eq return-range 'last)
				    recurse-result))))
		  (if (eq recurse-result 'die)
		      (throw 'done 'die))
		  ;; (assert (eq recurse-result nil)) adjust point in
		  ;;  order to try again---for reasons of efficiency
		  ;;  we'll attempt to use forward-thing
		  ;; (note: we don't go to the extremity of the match;
		  ;; that wouldn't work if we're looking for multiple
		  ;; occurrences of a thing---a correct match may
		  ;; overlap with current match)
		  (if (eq direction-here 'forward)
		      (progn
			(if (and (consp (car token-list))
				 (eq (nth 0 (car token-list))
				     'structural))
			    (let ((thing (nth 1 (car token-list))))
			      (cond ((eq thing 'block)
				     ;; in block, a match may have
				     ;; failed, even if there is a
				     ;; subblock where a match succeeds
				     (goto-char (1+ b)))
				    (t
				     (forward-thing 1))))
			  (if (> e b)
			      (goto-char e)
			    ;; e = b possible for (space 0) searches
			    ;; (and skip non-whitespace searches, etc)
			    (throw 'done nil)))
			(if (eobp) (throw 'done nil)))
		    ;; backwards
		    (if (and (consp (car token-list))
			     (eq (nth 0 (car token-list))
				 'structural))
			(let ((thing (nth 1 (car token-list))))
			  (cond ((eq thing 'block)
				 (goto-char (1- e)))
				(t
				 (forward-thing thing -1))))
		      (goto-char b))
		    (if (bobp) (throw 'done nil))))
	      ;; otherwise, this was the last token match to look
	      ;; for.  Check that a neighbor match indeed reaches at
	      ;; least limit + 1
	      (if (and (eq original-direction 'neighbor)
		       (<= e limit))
		  ;; this will prevent a lot of wasteful backtracking
		  ;; (won't work if structural "block" searches are
		  ;; possible, since going back may be necessary to
		  ;; define a match stretching all the way to the
		  ;; limit
		  (throw 'done 'die))
	      (throw 'done (cons b e)))))))))
  
(defun listen-search-for-tokens (text direction return-range 
				      &optional history-reference)
  "TEXT is regarded as a list of tokens, which are words or single
non-word characters.  Whitespace is not considered.  The tokens are
searched for in order.  Only word tokens need to occur consecutively,
separated by possible whitespace; other tokens may be matched by
skipping other characters in the text, but only within a region.  A
range is returned of the form (START . END), but nil is returned if
the search was not successful.  The range denotes the submatch with
the first or last token or the match of all tokens, as specified by
RETURN-RANGE, which is 'first, 'last, or 'all.  The search starts at
point; if the direction, for a specified by DIRECTION, is backward,
then the last token is matched to a position before point (i.e.  the
end of the match, in the Emacs sense, is less than point). If the
direction is 'neighbor, then the search is also backwards but a match
must contain point (i.e. the end of the match must be greater than
point).  In addition to the mechanism just described, TEXT may consist
of a command in listen-predicate-embedded-command-extension, which
denotes an earlier search."

  (assert (member direction '(forward backward neighbor)))
  (assert (member return-range '(first last all)))

  ;; First, find out whether text is a history reference that is by
  ;; itself
  (if (member text listen-search-history-reference)
      (let ((number (listen-number-argument text)))
	(if (<= number listen-search-history-length)
	    (listen-search-for-token-list 
	     (point)			; position
	     (point)			; limit 
	     direction			; original-direction
	     return-range 
	     (nth (1- number)
		  listen-search-history-tokenized) ; token-list
	     'skip	 ; don't care about initial whitespaces at all
	     )
	  ;; Else, the number argument doesn't make sense, perhaps an
	  ;; error message should be printed
	  ))
    ;; Now, start working
    (let* ((tokens nil)
	   (new-text "")
	   (intra-line nil)
	   (intra-para nil))
      (if listen-repeating
	  (setq tokens (car listen-search-history-tokenized))
	;; Otherwise, we need to tokenize text 
	;; proceed from the end of text
	(while (not (string-match "^ *$" text))
	  (let* (lst-begin thing newtokens delta-text)
	    (cond 
	     ;; Use what's under cursor or at point "word hare",
	     ;; "term twain tair" or at last "term loost", ...
	     ;; or what is in clipboard "yank twain"
	     ((and
	       (string-match
		listen-embedded-commands
		text)
	       (= (length text) (match-end 0)))
	      (setq lst-begin (match-beginning 0)
		    thing (substring text lst-begin (match-end 0))
		    newtokens
		    (if (save-match-data 
			  (string-match listen-yank-word thing))
			;; This was an embedded yank command
			(let* ((number (listen-extract-number thing))
			       (this-text (current-kill (1- number) t)))
			  (setq delta-text (concat " " this-text " "))
			  (list (or this-text "")))
		      ;; This was an embedded command that references
		      ;; something at a common position
		      (let* ((scope (listen-scope-argument thing))
			     (number (listen-extract-number thing))
			     (source-region 
			      (listen-region-near-common-position-this-or-that 
			       thing))
			     (source-buffer 
			      (listen-buffer-near-common-position-this-or-that thing))		
			     (text-to-look-for 
			      (with-current-buffer source-buffer
				(buffer-substring (car source-region)
						  (cdr
						   source-region)))))
			;; so
			;; listen-...-near-common-position-this-or-that
			;; succeeded in finding something
			(setq delta-text (concat " " text-to-look-for " "))
			(list text-to-look-for)))))
	     
	     ;; remaining cases
	     ( ;; condition below must be true if control reaches here
	      (string-match "\\w+$\\|.$" text)
	      ;;
	      (setq lst-begin (match-beginning 0)
		    thing (substring text lst-begin (match-end 1))
		    newtokens
		    (cond
		     ;; structural search, not really implemented (?)
		     ((string-match 
		       listen-search-structural-search
		       thing) 
		      (list
		       (list 'structural 
			     ;;	     (listen-extract-scope thing)
			     (listen-extract-number thing))))
		     ;; search for a reference to history
		     ((string-match 
		       (concat "^" 
			       listen-numbers-re
			       "$")
		       thing)
		      (let* ((number 
			      (listen-number-argument thing)))
			(setq delta-text 
			      (if (<= number
				      listen-search-history-length)
				  (nth (1- number)
				       listen-search-history)
				;; Ignore if reference does not make sense
				""))
			(nth (1- number)
			     listen-search-history-tokenized)))
		     ;; search for empty string and don't ignore spaces
		     ((string-match 
		       (concat "^"
			       listen-nospace-word
			       "$")
		       thing)
		      '((space 0)))
		     ;; search for one or more spaces
		     ((string-match 
		       (concat "^"
			       listen-space-word
			       "\\( "
			       listen-numbers-re
			       "\\)?$")
		       thing)
		      (make-list (listen-extract-number thing) (list 'space
								     1)))
		     ;; search for one or more newlines (or beginning/end
		     ;; of buffer)
		     ((string-match 
		       (concat "^"
			       listen-newline-word
			       "\\( "
			       listen-numbers-re
			       "\\)?$")
		       thing)
		      (make-list (listen-extract-number thing) (list
								'newline
								1)))
		     ;; skip non-whitespace
		     ((string-match 
		       (concat "^" listen-non-whitespace-skip-word
			       "$")
		       thing)
		      '(skip-non-whitespace))
		     ;; skip letters & digits
		     ((string-match 
		       (concat "^" listen-letter-skip-word
			       "$")
		       thing)
		      '(skip-letters))
		     ;; skip something of current line (like ".*")
		     ((string-match 
		       (concat "^" listen-intra-line-skip-word
			       "$")
		       thing)
		      (setq intra-line t)
		      nil) 
		     ;; skip something in current paragraph
		     ((string-match 
		       (concat "^" listen-intra-para-skip-word
			       "$")
		       thing)
		      (setq intra-para t)
		      nil)
		     ;; the usual case, just make a regular
		     ;; expression matching the thing
		     (t (unless (string= thing " ")
			  (list (regexp-quote thing))))))))
	    (if (null newtokens)
		(cond
		 (intra-line
		  (setq tokens 
			(cons (list 'bounded 'line (car tokens))
			      (cdr tokens)))
		  (setq intra-line nil))
		 (intra-para
		  ;; match must be within a paragraph only
		  (setq tokens 
			(cons (list 'bounded 'para (car tokens))
			      (cdr tokens)))
		  (setq intra-para nil)))
	      ;; else, when we have a token
	      (setq tokens (append newtokens tokens)))
	    (setq new-text (concat (or delta-text
				       (substring text lst-begin))
				   new-text)
		  text (substring text 0 lst-begin))))
	;; end of while loop 

	;; trim initial white space
	(string-match "^ *" new-text)
	(setq new-text (substring new-text (match-end 0)))

	;; Now, we are not dealing with a sole history reference, do
	;; remember the text, cut off after 30 (arbitrarily)
	(unless (or history-reference
		    listen-repeating)
	  (if (= listen-search-history-length 30) 
	      (progn (setcdr (last listen-search-history 2) 
			     nil)
		     (setcdr (last 
			      listen-search-history-tokenized 2)
			     nil))
	    (incf listen-search-history-length))
	  (setq listen-search-history
		(cons new-text listen-search-history)
		listen-search-history-tokenized
		(cons tokens listen-search-history-tokenized))))
      (if (consp tokens)
	  (let ((result
		 (listen-search-for-token-list 
		  (point)		; position
		  (point)		; limit 
		  direction		; original-direction
		  return-range 
		  tokens		; token-list
		  'skip	 ; don't care about initial whitespaces at all
		  )))
	    (when (consp result)
	      result))))))
  
(defun listen-direction-from-search-indication (search-indication)
  (assert (member 
	   search-indication 
	   '(after-forward before-forward after-backward
			   before-backward after-neighbor
			   before-neighbor)))
  (cond ((member search-indication '(after-forward
				     before-forward))
	 'forward)
	((member search-indication '(after-backward
				     before-backward ))
	 'backward)
	((member search-indication '(after-neighbor
				     before-neighbor ))
	 'neighbor)))
 
(defun listen-search-pos (&optional do-move)
  (let* ((search-indication (listen-search-argument))
	 (penultimate (listen-penultimate-argument))
	 (direction (listen-direction-from-search-indication
		     search-indication))
	 (range (cond 
		 ((member 
		   search-indication 
		   '(before-forward before-backward before-neighbor))
		  'first)
		 (t 'last)))
	 (arg (listen-predicate-argument "Search for: "))
	 (tries 0)
	 pos)
    (assert (or (null penultimate) (eq penultimate 'penultimate)))
    (save-excursion
      (while (< tries 2)
	(let* ((result (listen-search-for-tokens 
			arg direction
			range)))
	  (setq pos (if result
			(if penultimate
			    (if (eq range 'last)
				(car result)
			      (cdr result))
			  (if (eq range 'last)
			      (cdr result)
			    (car result)))
		      (error "Not found")))
	  (if (and do-move (= pos (point)))
	      (if (eq direction 'forward)
		  (forward-char 1)
		(forward-char -1))
	    (setq tries 1))
	  (incf tries))))
    pos))

(defun listen-search-interval-relative ()
  (let ((result (listen-search-pos t)))
    (if result
	(if (< result (point))
	    (list result (point))
	  (list (point) result)))))

(defun listen-search-interval ()
  (let ((search-indication (listen-search-argument))
	(arg  (listen-predicate-argument "Search for: ")))
    (assert (member 
	     search-indication 
	     '(forward backward neighbor)))
    (let ((result  (listen-search-for-tokens 
		    arg
		    search-indication
		    'all)))
      (if result
	  (list (car result) (cdr result))
	(error "Not found")))))


(defun listen-do-at-search-pos (command-or-sexp)
  (listen-do-at-position
   command-or-sexp
   nil
   (listen-search-pos)
   nil
   listen-event-argument-words))


;;; LISTEN INSERT

(defun listen-set-last (&optional insertion-event)
  (let* ((previous-index (and (> listen-valid-records 0)
			      (mod (1- listen-last-index)
				   listen-recent-records-size)))
	 (previous-is-word-event 
	  (and previous-index
	       (listen-word-event-p
		(ListenRecord-event
		 (aref listen-recent-records
		       previous-index))))))
    (when (or (not insertion-event)
	      (and (listen-event-p last-command-event)
		   (or ;; Beginning of new utterance
		    (assq 'speech 
			  (listen-event-alist last-command-event))
		    ;; or previous event was not a listen-word event
		    (not previous-is-word-event))))
      (let ((marker-bef
	     (listen-get-record-field listen-last-index marker-before)))
	(when (not (marker-position marker-bef))
	  ;; If called twice for somer reason, only first
	  ;; call counts
	  (set-marker marker-bef (point)))))

    ;; In any case, signal listen-post-command to set marker-after
    (setq listen-set-marker-after t)

    ;; If previous event was a listen-word and the
    ;; current event is not the first speech event
    ;; in an utterance, then delete marker-after in
    ;; previous record
    (when 
	(and previous-is-word-event
	     insertion-event
	     (listen-word-event-p 
	      (ListenRecord-event
	       (aref listen-recent-records
		     listen-last-index))) 
	     (not (assq 'speech 
			(listen-event-alist last-command-event))))
      (set-marker
       (ListenRecord-marker-after
	(aref listen-recent-records previous-index))
       nil))))

(defun listen-remove-double-at (str)
  (save-match-data
    (let ((i 0))
      (while (setq i (string-match "@@" str i))
	(incf i)
	(setq str (replace-match "@" t t str))))
    str))

(defun listen-insert-simple (&rest x)
  "This function just calls built-in insert function, but it updates
the 'last' position."
  (listen-set-last)
  (apply 'insert x))

(defun listen-insert (text)
  "Insert TEXT (except for any occurrences of \"@\"), then move cursor
to a position corresponding to the \"@\" in TEXT.  Fix at both ends.
Quote \"@\" as \"@@\" if you need to insert \"\@\" in your buffer."
  (let* ((at-position 
	  (if (string-match "\\(^\\|[^@]\\)[@]\\([^@]\\|$\\)" text) 
	      (match-end 1)))
	 b e old-b at-marker begin-marker
	 (rest (if at-position (listen-remove-double-at 
				(substring text (1+ at-position)))))
	 inserted-an-x)
    (if mark-active
	(setq b (region-beginning)
	      e (region-end))
      (setq b (point) e (point)))
    (setq begin-marker (set-marker (make-marker) b))
    (goto-char b)
    (listen-set-last)
    (if at-position
	(save-excursion
	  (insert (listen-remove-double-at 
		   (substring text 0 at-position)))
	  (setq at-marker (point-marker))
	  (goto-char (+ e (length (listen-remove-double-at 
				   (substring text 0 at-position)))))
	  (cond ((and (not mark-active) (not (string= rest "")))
		 ;; it is not always the case that an operation adds
		 ;; new element to the buffer-undo-list (instead, for
		 ;; example, a previous interval is augmented) --
		 ;; force that to be the case
		 (undo-boundary)	
		 ;; insert an extra "x" so as not to confuse
		 ;; listen-fix---for example, when inserting $$
		 (insert "x" rest)
		 (setq inserted-an-x t))
		(t (insert rest)))
	  (listen-fix (point) t))
      (save-excursion
	(insert (listen-remove-double-at text))
	(setq at-marker (point-marker))
	(listen-fix (point) t)))
    (listen-fix begin-marker t)
    (goto-char at-marker)
    (if inserted-an-x
	;; remove that "x"
	(delete-char 1))
    (set-marker begin-marker nil)
    (set-marker at-marker nil))
  (listen-set-last))

;;; SEARCH STUFF

(defvar listen-last-command-is-search nil
  "A flag that is set if and only if the last search did not succeed.")

(defvar listen-search-not-found nil
  "A flag that is set if and only if the last search did not succeed.
This logic could be made more sophisticated, by involving the command
loop, so that the flag is reset whenever the command is not a
repeat.")

(defun listen-go-command ()
  (interactive)
  (listen-add-assoc-command-status '(movement))    
  (let ((direction (listen-direction-from-search-indication
		    (listen-search-argument)))
	(p (condition-case nil
	       (listen-search-pos t)
	     (error nil))))
    (cond 
     (p
      (listen-push-mark-if-prev-not-movement)
      (setq listen-search-not-found nil)
      (goto-char p))
     ((and (not p) 
	   (not listen-search-not-found))
      (setq listen-search-not-found t)
      (error "Not found, repeat to wrap around"))
     ((and (not p) 
	   listen-search-not-found
	   listen-repeating)
      (let ((old-pos (point))) 
	(cond 
	 ((eq direction 'forward)
	  (goto-char (point-min))
	  (message "Trying from the beginning"))
	 (t
	  (goto-char (point-max))))
	(let ((new-p (condition-case nil
			 (listen-search-pos t)
		       (error nil))))
	  (if new-p
	      (goto-char new-p)
	    (goto-char old-pos)
	    (error "Not found anywhere in buffer."))
	  (setq listen-search-not-found nil))))
     (t (setq listen-search-not-found nil)))))

(defun listen-split-and-go-command ()
  (interactive)
  (if listen-repeating
      (listen-go-command)
    (split-window-vertically)
    (listen-go-command)))


;; IMENU
(require 'imenu)

(defun listen-imenu (&optional some-string)
  (interactive)   
  (listen-add-assoc-command-status '(movement)) 
  (listen-push-mark-if-prev-not-movement)
  (if some-string
      (let ((look-up-result (imenu--in-alist some-string 
					     (imenu--make-index-alist))))
	(if look-up-result (goto-char (cdr look-up-result))))
    ;; otherwise, read interactively
    (call-interactively 'imenu)))

(defun listen-goto-def ()
  (interactive)
  (cond ((member major-mode '(tex-mode latex-mode))
	 (listen-call-at-position-and-window-manage
	  ;; let view-crossref reuse current window (in first place)
	  '(reftex-view-crossref t)))))

(defun listen-goto-function-def ()
  (interactive) 
  (cond ((member major-mode '(emacs-lisp-mode listen-grammar-mode))
	 (listen-call-at-position-and-window-manage
	  '(let ((sym (function-at-point)))
	     (if sym
		 (find-function sym)))))))

(defun listen-goto-variable-def ()
  (interactive)
  (cond ((member major-mode '(emacs-lisp-mode listen-grammar-mode))
	 (listen-call-at-position-and-window-manage
	  '(let ((sym (variable-at-point)))
	     (if sym
		 (find-variable sym)))))))


;; SIMULATE A BOTTOM-DOWN EVENT BY VOICE

(defvar listen-original-down-1 
  "Holds the original value of down-mouse-1 while this event is being
simulated.")

;;; Simulate a down-mouse-1 event and prevent the subsequent and
;;; expected down-mouse-1 to have its normal meaning
(defun listen-mouse-down (mouse-down-action)
  (let* ((mouse-position (mouse-position))
	 (frame (car mouse-position))
	 (coordinates (cdr mouse-position))
	 (x-pos (car coordinates))
	 (y-pos (cdr coordinates)))
    (setq listen-original-down-1 
	  (lookup-key (current-global-map) '[down-mouse-1]))
    (global-set-key '[down-mouse-1]
		    (function 
		     (lambda ()
		       (interactive)
		       (global-set-key '[down-mouse-1]
				       listen-original-down-1))))
    (funcall mouse-down-action 
	     (list 'down-mouse-1
		   (list (window-at x-pos y-pos frame)
			 (mouse-point)
			 (cdr (mouse-pixel-position))
			 0	 ; this is the time stamp, what to do?
			 )
		   0			; the click-count
		   ))))

(defun listen-click (mouse-event-symbol)
  (interactive)
  (let* ((mouse-position (mouse-position))
	 (frame (car mouse-position))
	 (window (window-at (cadr mouse-position) 
			    (cddr mouse-position)
			    frame))
	 (coordinates (coordinates-in-window-p 
		       (cdr mouse-position)
		       window))
	 (x-pos (car coordinates))
	 (y-pos (cdr coordinates))
	 (window-pixel-position 
	  (cons (* (frame-char-width frame) x-pos)
		(* (frame-char-height frame) y-pos))))
    (select-window window)
    (listen-execute-events-as-macro
     (list
      (list (intern (concat "down-" (symbol-name mouse-event-symbol)))
	    (list window
		  (mouse-point)
		  window-pixel-position
		  0			; time stamp, what do to?
		  )
	    )
      (list mouse-event-symbol
	    (list window
		  (mouse-point)
		  window-pixel-position
		  0			; time stamp, what do to?
		  )
	    )
      ))))

(defun listen-click-1 ()
  (interactive)
  (listen-add-assoc-command-status '(movement))
  (listen-push-mark-if-prev-not-movement)
  (listen-click 'mouse-1))

(defun listen-click-2 ()
  (interactive)
  (listen-click 'mouse-2))

(defun listen-click-3 ()
  (interactive)
  (listen-click 'mouse-3))


;;; SPACING AND RELATED STUFF

(defun listen-one-space ()
  (interactive)
  (listen-set-last)
  (delete-horizontal-space) 
  (listen-execute-event-macro " "))

(defun listen-make-region (begin end)
  (listen-push-mark)
  (if (<= end (point))
      (progn
	(goto-char end)
	(listen-push-mark)
	(setq mark-active t)
	(goto-char begin))
    (push-mark begin t t)
    (goto-char end)))

(defun listen-buffer-menu ()
  (interactive)
  (if (get-buffer "*Buffer List*")
      (bury-buffer "*Buffer List*"))
  (buffer-menu)
  (if (and (listen-command-event-p last-command-event)
	   (> (length listen-event-argument-words) 0))
      (listen-execute-events-as-macro
       (list
	(list 'listen-word listen-event-argument-words nil 'artificial)
	(list 'return))
       t ;; make undoable
       )))

(defun listen-split-window ()
  (interactive)
  (listen-peek-register)
  (split-window-vertically))

;;; general suggestion: (global-set-key "" 'listen-buffer-menu) so
;;; that ^X-b doesn't return to buffer-menu after ^X^B!

(defun listen-switch-to-buffer ()
  (interactive)
  (if (get-buffer "*Buffer List*")
      (bury-buffer "*Buffer List*"))
  (switch-to-buffer (other-buffer)))

(defun listen-calculate-enlarge-shrink ()
  (/ (cdr (assoc 'height (frame-parameters))) 4))

(defun listen-enlarge-window ()
  (interactive)
  (enlarge-window (listen-calculate-enlarge-shrink)))

(defun listen-shrink-window ()
  (interactive)
  (shrink-window (listen-calculate-enlarge-shrink)))

(defun listen-browse-file-name ()
  (interactive)
  (let ((name (or (and (eq major-mode
			   'Buffer-menu-mode)
		       (dired-get-filename))
		  (expand-file-name 
		   (thing-at-point 'filename)))))
    (message "name %s" name)
    (browse-url-of-file name)))

(defun listen-browse-directory ()
  "Invoke external browser on the directory containing 
the default directory (usually directory of file associated
to current buffer)."
  (interactive)    
  (browse-url-of-file default-directory))


(defun listen-quit ()
  (interactive)
  (setq listen-discarded-input unread-command-events)
  (setq unread-command-events '(7)))
					;   (if (and delete-selection-mode transient-mark-mode mark-active)
					;       ;; abort-recursive-edit seems to empty the unread events queue 
					;       (setq deactivate-mark t)
					;     (abort-recursive-edit) 
					; ;     ;;    (keyboard-quit) doesn't work in minibuffer
					; ;     ))
 
(defun listen-confirm ()
  "When this command is executed during replay, the user is asked
to proceed (finish the recorded sequence and restart it), to try again
(abort the recording, but restart it), or to quit.  During recording,
this command has no immediate effect.  Rather, its purpose is to turn
the playing of the recording into an interactive process."
  (interactive)
  (when listen-repeating
 
  (let* ((the-rest unread-command-events)
	 (keypress (progn
		     (setq unread-command-events nil)
		     (read-event
		      "SPC: proceed, BS (TAB, f2): try again, anything else: quit"))))       

    (cond
     ((or (member keypress '(? ))
	  (and (listen-command-event-p keypress)
	       (string= (listen-event-command-words keypress)
			listen-space-word)))
      (setq unread-command-events (append the-rest '(vox-repeat))))
     ((or (member keypress '(tab backspace f2))
	  (and (listen-command-event-p keypress)
	       (or (string= (listen-event-command-words keypress)
			    listen-backspace-word)
		   (string= (listen-event-command-words keypress)
			    listen-tab-word)
		   (string= (listen-event-command-words keypress)
			    listen-go-again-word))))
      (setq unread-command-events '(vox-repeat)))
     ((keyboard-quit))))))

(defun listen-new-example ()
  (interactive)
  (let ((name (or (and (eq major-mode
			   'Buffer-menu-mode)
		       (dired-get-filename))
		  (expand-file-name 
		   (thing-at-point 'filename)))))))
;;


(defun listen-macro-tex ()
  (interactive)
  (require 'tex)
  (let ((m (point-marker)))
    (listen-call-interactively-identifier 'TeX-insert-macro)
    (listen-fix m)
    (set-marker m nil)))

(defun listen-environment-tex ()
  (interactive)
  (require 'tex)
  (listen-call-interactively-identifier
   'LaTeX-environment))

(defun listen-change-environment-tex ()
  (interactive)
  (require 'tex)
  (listen-call-interactively-identifier
   '(LaTeX-environment 1)))

(defun listen-no-environment-tex ()
  (interactive)
;;; based on LaTeX-modify-environment
  (require 'tex) 
  (save-excursion
    (LaTeX-find-matching-end)
    ;; try to include newline by skipping forward
    (if (not (eobp)) (re-search-forward " *\n?"))
    (re-search-backward (concat (regexp-quote TeX-esc)
				"end"
				(regexp-quote TeX-grop)
				" *\\([a-zA-Z*]*\\)"
				(regexp-quote TeX-grcl)
				" *\n?")
			(save-excursion (beginning-of-line 0) (point)))
    (delete-region (match-beginning 0) (match-end 0))
    (beginning-of-line 1)
    (LaTeX-find-matching-begin)
    (re-search-forward (concat (regexp-quote TeX-esc)
			       "begin"
			       (regexp-quote TeX-grop)
			       " *\\([a-zA-Z*]*\\)"
			       (regexp-quote TeX-grcl)
			       "[][{}a-zA-Z*]* *\n?")
		       (save-excursion (end-of-line 2) (point)))
    (delete-region (match-beginning 0) (match-end 0))))

(defun listen-no-macro-tex ()
  (interactive)
  (require 'tex)
  (save-excursion
    (cond ((thing-at-point-looking-at "[\\][a-zA-Z]+")
	   (delete-region (match-beginning 0) (match-end 0))
	   (if (looking-at "{")
	       (listen-unpair-command "brace")))
	  ((re-search-backward "\\([\\][a-zA-Z]+\\){" nil 'move)
	   (delete-region (match-beginning 1) (match-end 1))
	   (listen-unpair-command "brace"))
	  ((error "Not found")))))

(defun listen-tex-write-letter ()
  (interactive)
  (require 'tex)
  (let ((letter-buffer (generate-new-buffer "*Letter*")))
    (switch-to-buffer letter-buffer)
    (latex-mode)
    (listen-insert listen-tex-letter-template)))

;;; simple
(defun choose-completion-string (choice &optional buffer base-size)
  (let ((buffer (or buffer completion-reference-buffer)))
    ;; If BUFFER is a minibuffer, barf unless it's the currently
    ;; active minibuffer.
    (if (and (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
	     (or (not (active-minibuffer-window))
		 (not (equal buffer
			     (window-buffer (active-minibuffer-window))))))
	(error "Minibuffer is not active for completion")
      ;; Insert the completion into the buffer where completion was requested.
      (set-buffer buffer)
      (if base-size
	  (delete-region (+ base-size (point-min)) (point))
	(choose-completion-delete-max-match choice))
      (listen-insert-simple choice)
      (remove-text-properties (- (point) (length choice)) (point)
			      '(mouse-face nil))
      ;; Update point in the window that BUFFER is showing in.
      (let ((window (get-buffer-window buffer t)))
	(set-window-point window (point))
	(if (not (equal buffer (window-buffer (minibuffer-window))))
	    (select-window window)))
      ;; If completing for the minibuffer, exit it with this choice.
      (and (not completion-no-auto-exit)
	   (equal buffer (window-buffer (minibuffer-window)))
	   minibuffer-completion-table
	   ;; If this is reading a file name, and the file name chosen
	   ;; is a directory, don't exit the minibuffer.
	   (if (and (eq minibuffer-completion-table 'read-file-name-internal)
		    (file-directory-p (buffer-string)))
	       (select-window (active-minibuffer-window))
	     (exit-minibuffer))))))


(defun listen-literal ()
  (interactive)
  (listen-execute-events-as-macro
   (list
    (list 'listen-word 
	  (cdr (assq 'raw-argument 
		     (listen-event-alist last-command-event)))
	  nil 'artificial))))

(provide 'listen-support)
