;;; jde-usages-eval-type-of.el --- Alternate implementation of jde-parse-eval-type-of


;;; Commentary:
;; 

;;; History:
;; 

;;; Code:
;; (defalias 'jde-parse-eval-type-of 'jde-usages-parse-eval-type-of)
(defun jde-usages-parse-eval-type-of (expr &optional imps)
  "Get type of EXPR. This function returns a class name or builtin
Java type name, e.g., int."
  (if expr
      (let ((class-at-point (jde-usages-get-class-at-point))
            (super-at-point (jde-parse-get-super-class-at-point))
	    (package (jde-usages-parse-get-package-name))
	    (last-char (aref expr (- (length expr) 1 )))
	    (imports (or imps (jde-parse-import-list)))
	    to-complete qualified-name chop-pos temp answer)
	(cond
	 
	 ;;If it's number returns an int
	 ((integerp expr)
	  "int")
	 
	 ;;If it's 001L or 134l return long
	 ((if (and (integerp (substring expr 0 (- (length expr) 1)))
		   (or (string= "L" (substring expr (- (length expr) 1)
					       (length expr)))
		       (string= "l" (substring expr (- (length expr) 1)
					       (length expr)))))
	      "long"))
	 
	 ;;If it's a floating point return float
	 ((floatp expr)
	  "double")

	 ;;If it's 000F or 1234f return float
	 ((if (and (floatp (substring expr 0 (- (length expr) 1)))
		   (or (string= "F" (substring expr (- (length expr) 1)
					       (length expr)))
		       (string= "f" (substring expr (- (length expr) 1)
					       (length expr)))))
	      "float"))
	 ((string-match "false\\|true" expr)
	  "boolean")

	 ;;Checking if it's a character
	 ((string= "'" (substring expr 0 1))
	  "char")

	 ;; If it's "this", we return the class name of the class we code in
	 ((and class-at-point (string= "this" expr))
          (jde-usages-parse-class-exists
	  (if package
	      (concat  package "." class-at-point)
	    (jde-usages-parse-get-qualified-name class-at-point t imports)))) ;; no package

	 ;; If it's "super", we return the super class
	 ;;name of the class we code in
	 ((and super-at-point (string= "super" expr))
	  (jde-usages-parse-get-qualified-name super-at-point t imports))
	       
	 ;;check if it's an inner class
	 ((and expr
	       class-at-point
	       (string= "this.this" expr)
	       (setq qualified-name (jde-parse-get-inner-class
				     class-at-point)))
	  qualified-name)
                           
	 ;; is it a function call?
	 ((eq last-char ?\))
	  (let* ((result
		  (jde-parse-isolate-before-matching-of-last-car
		   expr))
		 (temp (if (not (string= "this.this" result))
			   (jde-parse-split-by-dots result)))
		 to-complete)
	    (if temp
		(jde-parse-find-completion-for-pair temp)
	      ;;we need exact completion here
	      (jde-parse-find-completion-for-pair
	       (list "this" result) nil jde-complete-private))

	    ;;if the previous did not work try only result
	    (if (not jde-complete-current-list)
		(jde-parse-find-completion-for-pair (list result "")))
                 
	    ;;if the previous did not work try again
	    (setq qualified-name
		  (jde-usages-parse-get-qualified-name result t imports))
	    (if qualified-name
		qualified-name
	      (if (and jde-complete-current-list (string-match " : " (caar jde-complete-current-list)))
		  (progn
		    (setq to-complete
			  (car (car jde-complete-current-list)))
		    (setq chop-pos (+ 3 (string-match " : " to-complete)))
		    (let* ((space-pos (string-match " " to-complete chop-pos))
			   (uqname (if space-pos (substring to-complete chop-pos space-pos)
				     (substring to-complete chop-pos))))
		      uqname))))))


	 ;;if it's an array
	 ((eq last-char ?\])
	  (let ((temp (jde-usages-parse-eval-type-of
		       (jde-parse-isolate-before-matching-of-last-car
			expr))))
	    (jde-parse-get-component-type-of-array-class temp)))

	       
	 ;;if it's a class name
	 ((and
	   (jde-usages-parse-looks-like-a-class-name expr)
	   (setq qualified-name (jde-usages-parse-get-qualified-name expr t imports)))
	  qualified-name)


	 ;;we look for atoms if expr is splittable by dots
	 ((setq temp (if (not (string= "this.this" expr))
			 (jde-parse-split-by-dots expr)))
	  ;;we need exact completion here
	  (jde-parse-find-completion-for-pair temp t)
	  (if (and jde-complete-current-list (string-match " : " (caar jde-complete-current-list)))
	      (progn
		(setq to-complete (car (car
					jde-complete-current-list)))
		(setq chop-pos (+ 3 (string-match " : " to-complete)))
		(let* ((space-pos (string-match " " to-complete chop-pos))
		       (uqname (if space-pos (substring to-complete chop-pos space-pos)
				 (substring to-complete chop-pos))))
		  (jde-usages-parse-get-qualified-name uqname nil imports)))
		  
	    nil))
	 (t
	  ;; See if it's declared somewhere in this buffer.
	  (let (parsed-type result result-qualifier)
	    (setq parsed-type (jde-parse-declared-type-of expr))
	    (setq result (car parsed-type))
	    (setq result-qualifier (cdr parsed-type))
		  
	    (if result
		(let ((count 0) type)
		  (while (string-match ".*\\[\\]" result)
		    (setq result (substring result 0
					    (- (length result) 2)))
		    (setq count (1+ count)))
			
		  (let (work)
		    (setq type
			  (cond
			   ;; handle primitive types, e.g., int
			   ((member result jde-parse-primitive-types)
			    result)
			   ;; quickly make sure fully qualified name
			   ;;doesn't exist
			   ((and result-qualifier
				 (jde-usages-parse-class-exists
				  (setq work (concat result-qualifier
						     "."
						     result))))
			    work)
			   ;; look it up in the import list and this package
			   ((jde-usages-parse-get-qualified-name-quickly result  imports package))
			   ;; then check for inner classes
			   ((jde-parse-get-inner-class-name result result-qualifier))

			   ;; otherwise use unqualified class name
			   (t
			    (jde-usages-parse-get-qualified-name result t imports)))))
			
		  (if type
		      (progn
			(while (> count 0)
			  (setq type (concat type "[]"))
			  (setq count (1- count)))
			(jde-parse-transform-array-classes-names
			 type))
		    (if (y-or-n-p
			 (format (concat "Could not find type of %s"
					 " Attempt to import %s? ")
				 expr result))
			(progn
			  ;; import
			  (jde-import-find-and-import result)
			  ;; recursive call of eval-type-of
			  (jde-usages-parse-eval-type-of expr))
		      (error "Could not find type of %s" result))))
	      (if (and jde-parse-casting
		       (null jde-parse-attempted-to-import)
		       (y-or-n-p
			(format (concat "Could not find type of %s"
					" Attempt to import %s? ")
				expr expr)))
		  (progn
		    (setq jde-parse-attempted-to-import t)
		    (setq jde-parse-casting nil)
		    (jde-import-find-and-import expr)
		    (jde-usages-parse-eval-type-of expr))
		(progn
		  (setq jde-parse-attempted-to-import nil)
		  nil)))))))))

(defun jde-usages-parse-looks-like-a-class-name (expr)
  "Return t if string EXPR looks like a class name."
  (let ((case-fold-search nil))
    (and (not (string-match "[?()@!%^&*=-+ ]" expr)) ;; chars that are illegal for a classname
	 (let* ((offset (string-match "[^.$]*$" expr))
	       (class-name (substring expr offset (+ 1 offset))))
	   (and offset (equal (capitalize class-name) class-name))))))

(defun jde-usages-parse-get-qualified-name-quickly (name imports package)
  "Guess the fully qualified name of the class NAME without making a java call.
This function only looks for NAME in the imports list IMPORTS and
in the package PACKAGE."
    (let ((package (if package (concat package ".") ""))
	  shortname fullname tmp result)
      (while (and imports (null result))
	(setq tmp (car imports))
	(setq shortname (car (cdr tmp)))
	(setq fullname (concat (car tmp) name))
	(if (string= name shortname)
	    (setq result fullname))
	(setq imports (cdr imports)))
      (if (null result) ;; try this package
          (if (file-exists-p (concat (file-name-directory (buffer-file-name))
				     name ".java"))
	      (concat package name)
	      nil)
	result)))

;;; new: Returns "" if no package statement is present
(defun jde-usages-parse-get-package-name ()
  "Return  the package name of the current class.
This function returns the empty string if no package is present."
  (let ((packages (semantic-brute-find-tag-by-class 'package (current-buffer))))
    (if (and (listp packages) (eq (length packages) 1))
	(semantic-tag-name (car packages))
      "")))


;;; new: Add an optional import list parameter so it doesn't need to be recomputed
(defun jde-usages-parse-get-qualified-name (name &optional import il)
  "Guess the fully qualified name of the class NAME, using the import list. It
returns a string if the fqn was found, or null otherwise. If IMPORT is non-nil
try importing the class"
  (or (jde-usages-parse-class-exists name)
      (let ((importlist (or il (jde-usages-parse-import-list)))
          shortname fullname tmp result)
      (while (and importlist (null result))
	(setq tmp (car importlist))
	(setq shortname (car (cdr tmp)))
	(setq fullname (concat (car tmp) name))
	(cond
	 ((setq result (and (string= "*" shortname) (jde-usages-parse-class-exists fullname))))
         ((string= name shortname)
	  (setq result fullname))
	 (t
	  (setq importlist (cdr importlist)))))
      (if (and (null result) import)
          (progn
            (if (jde-import-find-and-import name t)
              (setq result (jde-usages-parse-get-qualified-name name)))))
      result)))

;; (defalias 'jde-import-insert-import 'jde-usages-import-insert-import)
(defun jde-usages-import-insert-import (new-imports &optional exclude)
  "Asks user, if necessary, to choose one of NEW-IMPORTS and
inserts the selected import in the buffer. Returns t if the user
picked a class to import."
  (let* ((existing-imports (jde-import-get-imports))
         (candidate-imports (if exclude
                                (jde-import-exclude-imports new-imports)
                              new-imports))
         (new-import
          (if (> (length candidate-imports) 1)
              (jde-import-choose-import candidate-imports)
            (car candidate-imports))))
    (if new-import
        (if (jde-import-already-imports-class new-import existing-imports)
            (message "This buffer already imports %s" new-import)
          (jde-import-insert-imports-into-buffer (list new-import))
          t))))

;; (defalias 'jde-parse-import-list 'jde-usages-parse-import-list)
(defun jde-usages-parse-import-list ()
  "Return the list of Java packages declared in the current buffer.
It uses the semantic parser table to find the 'package' and
'import' statements. It implicitly adds the java.lang.* package,
the package name for this class and if this class is an inner
class then all the outer class names as package names. See also
`jde-split-import-token'."
  (let* ((tokens   (semantic-bovinate-toplevel t))
         (package (car (semantic-find-nonterminal-by-token 'package tokens)))
         (classes (semantic-find-tags-by-class 'type (semantic-find-tag-by-overlay (point))))
         (imports  (semantic-find-nonterminal-by-token 'include tokens))
         lst)
    (setq lst (append
               (maplist (lambda (list) (list (concat (mapconcat 'semantic-tag-name (reverse list) ".") ".")  "*"))
                        (nreverse (cons package classes)))
               (mapcar 'jde-split-import-token imports)))
    (or (member "java.lang.*" (mapcar 'car imports))
        (setq lst (append lst '(("java.lang." "*")))))
    lst))


;; (defalias 'jde-open-get-class-to-open 'jde-usages-get-class-to-open)
(defun jde-usages-get-class-to-open (pair parsed-symbol)
  "Evaluates PARSE-SYMBOL to check if it is a variable name or a class name.
If this fails point is on a method or an attribute of a class in the current
buffer or in a superclass. In this cases we check first if the parsed-symbol
is a possible member of the current class(\"this\") and if this fails it
checks if it is a member of the base class(\"super\")."
 (if (and (stringp (car pair)) 
	  (> (length (car pair)) 0))
     ;; if we got a pair all should work fine.
     (or (jde-parse-eval-type-of (car pair)) (jde-parse-eval-type-of (concat "this." (car pair))))
   (or (condition-case () 
	   (jde-parse-eval-type-of parsed-symbol)
         (error nil))
       (if (jde-parse-find-completion-for-pair 
	    `("this" ,parsed-symbol) nil jde-complete-private)
           (jde-parse-eval-type-of "this")
         nil)
       (if (jde-parse-find-completion-for-pair 
	    `("super" ,parsed-symbol) nil jde-complete-private)
           (jde-parse-eval-type-of "super")
	 nil))))

;; (defalias 'jde-parse-class-exists 'jde-usages-parse-class-exists)
(defun jde-usages-parse-class-exists (name)
  "Returns t if the fully qualified class name can be found in the
  classpath, nil otherwise."
  (if (stringp name)
      (progn
        ;; Replace double quotes by empty strings.
        ;; Double quotes seems to break the beanshell.
        (while (string-match "\"" name)
          (setq name (replace-match "" nil nil name))) 
  
        ;; Replace back slashes by empty strings.\
        ;; Back slashes causes Beeanshell problems
        (while (string-match "\\\\" name)
          (setq name (replace-match "" nil nil name)))
 
        (jde-jeval-r (concat "jde.util.Usages.classExists(\"" name "\");")))))

(provide 'jde-usages-eval-type-of)
;;; jde-usages-eval-type-of.el ends here
