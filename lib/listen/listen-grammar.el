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

;;; Originally written by Erik Ostrom
;;; Modified by Nils Klarlund
 
;;; GRAMMAR DATA STRUCTURE
;;; 
;;; grammar:  (rule ...)
;;; 
;;; rule:     is a structure, named listen-grammar-rule; it contains
;;;
;;;   :name
;;;   :elements     
;;;   :command
;;;   :alias
;;;
;;; name:     SYMBOL                    => name by which rule can be
;;;                                        referred to in other rules
;;;                                        and in communication from (to?)
;;;                                        recognizer.
;;;
;;; elements: (element ...)
;;;
;;;
;;; element:  LIST                      => component rule
;;;           STRING                    => literal word
;;;           VECTOR                    => special element: [type parameters]
;;; 
;;; command:  SEXP                      => a symbol representing an
;;;                                        interactive command or a
;;;                                        list encoding an SEXP
;;;                                        
;;; 
;;; a component rule is eq its association in the grammar's alist.
;;;
;;; special types include:
;;;           [| LIST]                  => alternatives

(require 'cl) ;; structs

;; GLOBALS

(defvar listen-grammar-parsed-rule-command nil 
"True if parsing a rule defining a command.")

(defvar listen-grammar-parsed-rule-command-type nil 
"Type of command: nullary, predicate or interactive.")

(defvar	listen-grammar-parsed-rule-alias nil)

(defvar listen-grammar-parsed-rule-production nil 
"True if parsing a production.")

(defvar listen-grammar-parsed-rule-estimate nil
"True when holding an sexp that describes an estimate of the
equivalent work the command would take to issue by keyboard+mouse
combination.")
(defvar listen-grammar-parsed-rule-mark-not-include nil
"True if derivations are not to be included in word list.")

;; GRAMMAR TYPE

(defun listen-make-grammar ()
  "Create an empty grammar."
  '())

(defun listen-grammar-add-rule (grammar name elements command
					command-type alias 
					&optional not-include estimate)
  "Create a new grammar by adding a new rule.
Add to the given GRAMMAR a rule with the given NAME and ELEMENTS.
Fourth argument COMMAND should be either a symbol naming a function to
execute when the rule is recognized, a string or vector describing a
key sequence to execute, or nil if it is not to be used as a command.
Fifth argument ALIAS, if present, is a symbol indicating a
transformation to be made between the recognizer's understanding of
what words are said and the words that should be used in emacs lisp
functions.  (E.g., sounds recognized as \"eye dent\" could be treated
in emacs lisp as \"ident\".)  Flag NOT-INCLUDE is true if 
grammar rule is not supposed to generate strings for inclusion in word
list (meant for speech recognizer.)

ELEMENTS should consist of strings and symbols.  A string represents
a literal word to be recognized; a symbol represents another rule
that is a component of this one.  In the current implementation,
the component rule *must* be in the grammar already.  (Yes, this
means there can be no cycles.) That's because listen-grammar-add-rule
expands any rule names occurring in ELEMENTS."
  (cons (make-listen-grammar-rule
	 :name name
	 :elements (listen-grammar-encode-elements elements grammar)
	 :command command
	 :command-type command-type
	 :alias alias
	 :not-include not-include
	 :estimate estimate)
	grammar))

(defstruct (listen-grammar-rule)
  (name)
  (elements)
  (command)
  (command-type)
  (alias)
  (not-include)
  (estimate))

(defun listen-grammar-rules (grammar)
  "Return the given GRAMMAR's rule set, as a list."
  grammar)

(defun listen-grammar-rule (grammar name)
  "Return the given GRAMMAR's rule with the given NAME, or false."
  (let ((ptr grammar))
    (while (and (consp ptr)
		(not (eq (listen-grammar-rule-name (car ptr))
			 name)))
      (setq ptr (cdr ptr)))
    (if (null ptr)
	nil
      (car ptr))))

;;; VOICE GRAMMAR ELEMENT TYPE REGISTRY

(defvar listen-grammar-elt-types '())
(defun listen-grammar-elt-types () listen-grammar-elt-types)
(defun register-listen-grammar-elt-type (name)
  (setq listen-grammar-elt-types (cons name listen-grammar-elt-types)))

;(defun listen-grammar-elt-type (name)
;  (if (memq name listen-grammar-elt-types)
;      name
;    (error "No such voice grammar element type: %s" name)))

(defmacro define-listen-grammar-elt-type (name properties)
  `(progn
     (register-listen-grammar-elt-type ',name)
     (mapcar (lambda (pair)
	       (put ',name
		    (intern (concat "listen-grammar-elt-type-"
				    (symbol-name (car pair))))
		    (let ((val (car (cdr pair))))
		      (if (symbolp val)
			  val
			(eval val)))))  ; eval ?
	     ',properties)))

(defun listen-grammar-elt-type (elt)
  (let ((answer nil)
	(types (listen-grammar-elt-types)))
    (while (not answer)
      (if types
	  (let ((type (car types)))
	    (if (funcall (get type 'listen-grammar-elt-type-is-fn) elt)
		(setq answer type)
	      (setq types (cdr types))))
	(error "Unknown element type in grammar rule %s" element)))
    answer))

(defun listen-grammar-elt-call-method (elt method &rest args)
  (apply (get (listen-grammar-elt-type elt)
	      (intern (concat "listen-grammar-elt-type-" (symbol-name method))))
	 elt args))

;;; Word Element Type
(define-listen-grammar-elt-type word
  ((is-fn stringp)
   (encode-fn (lambda (word g) word))
   (strings-fn (lambda (word) (list (list word))))))

;;; Rule-Name Element Type -- only used when encoding
(define-listen-grammar-elt-type rule-name
  ((is-fn symbolp)
   (encode-fn (lambda (rule-name g)
		(if rule-name
		    (or (listen-grammar-rule g rule-name)
			(error "Rule %s not found in grammar" rule-name))
		  nil)))))

;;; Rule Element Type -- encoded form of rule-name
(define-listen-grammar-elt-type rule
  ((is-fn listen-grammar-rule-p)
   (encode-fn (lambda (rule g)
		(error "Grammar rule elements must be supplied as symbols")))
   (strings-fn listen-grammar-rule-strings)))

;;; Disjunction Element Type -- foo | bar
(define-listen-grammar-elt-type disjunction
  ((is-fn (lambda (e) (and (vectorp e) (eq (aref e 0) '|))))
   (encode-fn (lambda (vector grammar)
		(vector (aref vector 0)
			(mapcar (lambda (l)
				  (listen-grammar-encode-elements l grammar))
				(aref vector 1)))))
   (strings-fn (lambda (disj)
		 (apply 'append
			(mapcar 'listen-grammar-elements-strings
				(aref disj 1)))))))

;;; Group Element Type -- (foo bar)
(define-listen-grammar-elt-type group
  ((is-fn (lambda (e) (and (vectorp e) (eq (aref e 0) 'group))))
   (encode-fn (lambda (vector grammar)
		(vector (aref vector 0)
			(mapcar (lambda (l)
				  (listen-grammar-encode-element l grammar))
				(aref vector 1)))))
   (strings-fn (lambda (group)
		 (listen-grammar-elements-strings (aref group 1))))))

;;; ENCODING RULES INTO A GRAMMAR

(defun listen-grammar-encode-element (element grammar)
  (listen-grammar-elt-call-method element 'encode-fn grammar))

;;; This is the function used by listen-grammar-add-rule to add more
;;; elements to a grammar
(defun listen-grammar-encode-elements (elements grammar)
  (mapcar (lambda (e) (listen-grammar-encode-element e grammar))
	  elements))

;;; GENERATING STRINGS FOR A GRAMMAR

(defun listen-grammar-element-strings (element)
  (listen-grammar-elt-call-method element 'strings-fn))

(defun listen-grammar-elements-strings (elements)
  "Return a list of all lists of words generated by the given ELEMENTS list."
  (if elements
      (let ((pre-words (listen-grammar-element-strings (car elements)))
	    (post-words (listen-grammar-elements-strings (cdr elements))))
	(apply 'append
	       (mapcar #'(lambda (pre)
			   (mapcar #'(lambda (post)
				       (append pre post))
				   post-words))
		       pre-words)))
    '(())))

(defun listen-grammar-rule-strings (rule)
  "Return a list of all lists of words generated by the given RULE."
  (let ((alias (listen-grammar-rule-alias rule))
	(strings (listen-grammar-elements-strings
		  (listen-grammar-rule-elements rule))))
    (if alias
	(list (list (cons (car strings) alias)))
      strings)))

(defun listen-grammar-map-strings (func grammar)
  "Call FUNC on each string generated by GRAMMAR.
FUNC should be a procedure of two arguments:  A grammar rule,
and a string (i.e,, list of words) that can be generated from it.

Each element of the list of words can be either a simple string or a
cons cell whose car is a list of words to be spotted by the recognizer
and whose cdr is a list of words to be sent back to the emacs lisp
handler.  For example, one rule might generate the strings (\"copy\"
\"eye\" \"dent\") for the recognizer and (\"copy\" \"ident\") for the
perfect world.  This functionality is provided to make up for
deficiencies in the recognizer.  Of course, really this should be a
function of the particular recognizer used.  Whoops."
  (apply 'append
	 (mapcar #'(lambda (rule)
		     (if (listen-grammar-rule-command rule)
			 (mapcar #'(lambda (words)
				     (funcall func
					      rule
					      words))
				 (listen-grammar-rule-strings rule))))
		 (listen-grammar-rules grammar))))

;;; GRAMMAR TO DRAGON VOCABULARY

;;; we insert the vocabulary commands wholesale in the current buffer,
;;; so beware.

(defun listen-grammar-print-macro (command)
  (cond ((vectorp command)
	 (insert "[")
	 (let ((length (length command))
	       (counter 0))
	   (while (< counter length)
	     (insert " ")
	     ;; this is a cheat:
	     (listen-grammar-print-macro (aref command counter))
	     (setq counter (1+ counter))))
	 (insert " ]"))
	((stringp command)
	 (listen-grammar-print-macro
	  (apply 'vector (listify-key-sequence command))))
;;;	 (insert "\\\"")
;;;	 (let ((length (length command))
;;;	       (counter 0))
;;;	   (while (< counter length)
;;;	     (let ((desc (text-char-description (aref command counter)))
;;;		   (start (point)))
;;;	       (if (string-match "^[MC]-.$" desc)
;;;		   (setq desc (concat "\\" desc)))
;;;	       (insert desc)
;;;	       (goto-char start)
;;;	       (if (eq (aref desc 0) ?^)
;;;		   (progn 
;;;		     (delete-char 1)
;;;		     (insert "\\\\C-")))
;;;	       (while (search-forward "\\" nil t)
;;;		 (replace-match "\\\\" nil t))
;;;	       (goto-char start)
;;;	       (while (search-forward "\"" nil t)
;;;		 (replace-match "\\\"" nil t))
;;;	       (end-of-line))
;;;	     (setq counter (1+ counter))))
;;;	 (insert "\\\""))
	((integerp command) ;; really a character in a vector
	 (insert (int-to-string command)))
	((symbolp command)
	 (insert (symbol-name command)))))

(defun listen-grammar-print-words (words select-fn print-fn)
  (let ((first t)
	(print-word (lambda (word)
		      (if first 
			  (setq first nil)
			(insert " "))
		      (insert (funcall print-fn word))))
	(print-words (lambda (words)
		       (if words
			   (let ((word (car words)))
			     (if (stringp word)
				 (funcall print-word word)
			       (mapcar print-word (funcall select-fn word)))
			     (funcall print-words (cdr words)))))))
    (funcall print-words words)))

(defun listen-grammar-sexp-to-dragon (sexp)
  (cond ((null sexp)
	 (insert "()"))
	((listp sexp)
	 (insert "(")
	 (listen-grammar-sexp-to-dragon (car sexp))
	 (mapcar (lambda (sub)
		   (insert " ")
		   (listen-grammar-sexp-to-dragon sub))
		 (cdr sexp))
	 (insert ")"))
	((vectorp sexp)
	 (insert "[")
	 (listen-grammar-sexp-to-dragon (car sexp))
	 (mapcar (lambda (sub)
		   (insert " ")
		   (listen-grammar-sexp-to-dragon sub))
		 (cdr sexp))
	 (insert "]"))
	((stringp sexp)
	 (insert "\\\"")
	 (let ((length (length sexp))
	       (counter 0))
	   (while (< counter length)
	     (let* ((desc (text-char-description (aref sexp counter)))
		    (desclen (length desc))
		    (start (point)))
	       (cond ((member desc '("{" "}"))
		      (insert "\\" desc))
		     ((string= desc "\\")
		      (insert "\\\\\\\\"))
		     ((string= desc "\"")
		      (insert "\\\\\\\""))
		     ((and (eq (length desc) 2)
			   (eq (aref desc 0) ?\\))
		      (insert "\\\\\\" (aref desc 1)))
		     ((string-match "^M-.$" desc)
		      (insert "{Ctrl+q}{Alt+" (aref desc 2) "}"))
		     ((string-match "^\\^.$" desc)
		      (insert "{Ctrl+q}{Ctrl+" (aref desc 1) "}"))
		     (t
		      (insert desc))))
	     (setq counter (1+ counter))))
	 (insert "\\\""))
	(t
	 (insert (prin1-to-string sexp)))))
	     
(defun listen-grammar-to-command-table (grammar)
  (insert ";;; Command table generated by EmacsListen\n") 
  (listen-grammar-map-strings
   #'(lambda (rule words)
       (let ((command (listen-grammar-rule-command rule)))
	 (insert "(\"")
 	 (listen-grammar-print-words words 'cdr 'identity)
         (insert "\" ")
	 (prin1 (listen-grammar-rule-command-type rule)
		(current-buffer))
	 (insert " ")
	 (prin1 command (current-buffer))
	 (insert " ")
	 (prin1 (listen-grammar-rule-estimate rule) (current-buffer))
	 (insert ")\n")))
   grammar))

(defun listen-grammar-to-DNS-list (grammar)
  (listen-grammar-map-strings
   #'(lambda (rule words)
       (let ((command (listen-grammar-rule-command rule)))
	 ;; words (written form)
	 (when (not (listen-grammar-rule-not-include rule))
	   (listen-grammar-print-words words 'cdr 'identity)
	   ;;(insert "\\")	 
	   ;; (listen-grammar-print-words words 'car 'identity)
	   ;; words (spoken form)
	   (insert "\n"))))
   grammar))   

;;; PARSE GRAMMAR

;;; this whole section could probably be simplified by the use of
;;; parse-partial-sexp, forward-sexp, and others, now that we have a
;;; decent syntax table for the mode.

(defun listen-grammar-consume-whitespace ()
  (if (looking-at "\\(\\s-*\n?\\)+")
      (goto-char (match-end 0)))
  (while (looking-at "/[/*]")
    (if (eq (char-after (1+ (point))) ?/)
	(forward-line 1)
      (search-forward "*/"))
    (if (looking-at "\\(\\s-*\n?\\)+")
	(goto-char (match-end 0)))))

(defun listen-grammar-parse:word ()
  (if (not (looking-at "\\(\\w\\|\\s_\\)+[.]?"))
      (error "Not a valid word character: %c" (char-after (point))))
  (prog1
      (buffer-substring (point) (match-end 0))
    (goto-char (match-end 0))
    (listen-grammar-consume-whitespace)))

(defun listen-grammar-parse:rule-name ()
  (forward-char 1)
  (let ((rule-name (intern (listen-grammar-parse:word))))
    (backward-char 1)
    (if (not (looking-at "\\S->"))
	(error "Rule name %s not terminated by > delimiter" rule-name))
    (forward-char 2)
    (listen-grammar-consume-whitespace)
    rule-name))

(defun listen-grammar-parse:quoted-string ()
  (looking-at "\"\\([^\\\"]*\\|\\\\\"\\)*\"")
  (prog1
      (eval (buffer-substring (match-beginning 0) (match-end 0)))
    (goto-char (match-end 0))
    (listen-grammar-consume-whitespace)))

(defun listen-grammar-parse:disjunction (first)
  (looking-at "|\\s-*")
  (goto-char (match-end 0))
  (let ((rest (listen-grammar-parse:expr)))
    (vector '|
	    (if (and (vectorp rest)
		     (eq (aref rest 0) '|))
		(cons first (aref rest 1))
	      (list first rest)))))

(defun listen-grammar-parse:command ()
  (forward-char 1)
  ;; communicate by global variable: we parsed a command
  (setq listen-grammar-parsed-rule-command (read (current-buffer)))
  ;; parse an estimate if present
  (when (looking-at "[ \n\t\]*[[]")
    (search-forward "[")
    (setq listen-grammar-parsed-rule-estimate (read (current-buffer)))
    (if (not (looking-at "[]]"))
	(error "] expected"))
    (search-forward "]")))

(defun listen-grammar-parse:alias ()
  (forward-char 1)
  ;; communicate by global variable: we parsed an alias
  (setq listen-grammar-parsed-rule-alias
	(let ((alias (read (current-buffer))))
	  (cond ((symbolp alias)
		 (list (symbol-name alias)))
		(t
		 (error "Alias must be a symbol: %s" alias))))))
  
(defun listen-grammar-parse:group ()
  (forward-char 1)
  (listen-grammar-consume-whitespace)
  (vector 'group (listen-grammar-parse:expr)))

(defun listen-grammar-parse:term ()
  (let ((c (char-after (point))))
    (cond ((eq c ?\<)
	   (listen-grammar-parse:rule-name))
	  ((eq c ?\")
	   (listen-grammar-parse:quoted-string))
	  ((eq c ?\()
	   (listen-grammar-parse:group))
	  ((looking-at "\\w")
	   (listen-grammar-parse:word))
	  (t
	   (error "Can't begin term with %c character" c)))))


(defun listen-grammar-parse:expr ()
  (let ((terms nil))
    (while (looking-at "[^;|):>.]")
      (setq terms (cons (listen-grammar-parse:term) terms)))
    (setq terms (nreverse terms))
    (let ((c (char-after (point))))
      (cond ((eq c ?|)
	     (list (listen-grammar-parse:disjunction terms)))
	    ((eq c ?.)
	     (if (not (looking-at "[.][.][.]\\s-*:"))
		 (error "Expecting ...:"))
	     (goto-char (1- (match-end 0)))
	     (if (or listen-grammar-parsed-rule-production
		     listen-grammar-parsed-rule-command)
		 (error "Command definition not allowed here"))
	     (listen-grammar-parse:command)
	     (setq listen-grammar-parsed-rule-command-type 'listen-predicate)
	     terms)    
	    ((eq c ?:)
	     (if (or listen-grammar-parsed-rule-production
		     listen-grammar-parsed-rule-command)
		 (error "Command definition not allowed here"))
	     ;; check for ":-" indicating that rule is not to be
	     ;; included for DNS word list purposes
	     (save-excursion
	       (forward-char)
	       (setq listen-grammar-parsed-rule-mark-not-include
		     (eq (char-after (point)) ?-)))
	     (if listen-grammar-parsed-rule-mark-not-include
		 (forward-char))
	     (listen-grammar-parse:command)
	     (setq listen-grammar-parsed-rule-command-type 'listen-nullary)
	     terms)
	    ((eq c ?\))
	     (forward-char)
	     (listen-grammar-consume-whitespace)
	     terms)
	    ((eq c ?>)
	     (if (or (not listen-grammar-parsed-rule-production) 
		     listen-grammar-parsed-rule-alias)
		 (error "Alias not allowed here"))
	     (listen-grammar-parse:alias)
	     terms)
	    (t
	     terms)))))

(defun listen-grammar-parse:rule ()
  (setq listen-grammar-parsed-rule-command nil
	listen-grammar-parsed-rule-alias nil
	listen-grammar-parsed-rule-production nil
	listen-grammar-parsed-rule-command-type nil
	listen-grammar-parsed-rule-mark-not-include nil
	listen-grammar-parsed-rule-estimate nil)
  (cond ((looking-at "<\\([^ \t>]+\\)>\\s *=\\s *")
	 (let ((rule-name (buffer-substring (match-beginning 1)
					    (match-end 1))))
	   (goto-char (match-end 0))
	   (listen-grammar-consume-whitespace)
	   (setq listen-grammar-parsed-rule-production t)
	   (cons rule-name (listen-grammar-parse:expr))))
	(t
	 (cons nil (listen-grammar-parse:expr)))))

(defun listen-grammar-parse-buffer (buf)
  "Parse the current buffer as a voice grammar."
  (let ((g (listen-make-grammar)))
    (unwind-protect
	(save-window-excursion
	  (save-excursion
	    (goto-char (point-min))
	    (listen-grammar-consume-whitespace)
	    (while (or (looking-at "\\(#include \".*\"\\)$")
		       (looking-at "\\(\\([^\";]+\\|\"\\([^\"\\\\]+\\|\\\\.\\|\\)*\"\\|\\)*;\\)"))
	      (let ((start (match-beginning 1))
		    (end (match-end 1)))
		(narrow-to-region start end)
		(if (looking-at "#include \"\\(.*\\)\"$")
		    (panic)
		  (let ((rule (listen-grammar-parse:rule)))
		    (setq g (listen-grammar-add-rule 
			     g
			     (if (car rule)
				 (intern (car rule)))
			     (cdr rule)
					    ;;; father forgive me
			     listen-grammar-parsed-rule-command
			     listen-grammar-parsed-rule-command-type
			     listen-grammar-parsed-rule-alias
			     listen-grammar-parsed-rule-mark-not-include
			     listen-grammar-parsed-rule-estimate))))
		(widen)
		(goto-char end))
	      (listen-grammar-consume-whitespace))))
      ;; clean-up form
      (widen))
    g))

;;; VOICE GRAMMAR MODE

(defvar listen-grammar-mode-syntax-table (make-syntax-table))
(modify-syntax-entry ?< "(>" listen-grammar-mode-syntax-table)
(modify-syntax-entry ?> ")<" listen-grammar-mode-syntax-table)
;(modify-syntax-entry ?_ "w" listen-grammar-mode-syntax-table)
(modify-syntax-entry ?/ ". 124b" listen-grammar-mode-syntax-table)
(modify-syntax-entry ?* ". 23" listen-grammar-mode-syntax-table)
(modify-syntax-entry ?\n "> b" listen-grammar-mode-syntax-table)
(modify-syntax-entry ?| "." listen-grammar-mode-syntax-table)
(modify-syntax-entry ?= "." listen-grammar-mode-syntax-table)

(defun listen-grammar-mode ()
  "blah blah blah, FIXME"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'listen-grammar-mode)
  (setq mode-name "Voice grammar")
  (set-syntax-table listen-grammar-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "// ")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function (lambda () "do nothing"))
  (run-hooks 'listen-grammar-mode-hook))

;;; glue functions
(defun listen-grammar-buffer-to-dragon (buf)
  (interactive "bBuffer: ")
  (let ((g (listen-grammar-parse-buffer buf))
	(b (create-file-buffer " *Dragon Dictate Import File*")))
    (pop-to-buffer b)
    (listen-grammar-to-dragon g)))

(defun listen-grammar-file-to-dragon (&optional infile)
  ;; otherwise big grammars don't work
  (let ((max-lisp-eval-depth (max max-lisp-eval-depth 512))
	(max-specpdl-size (max max-specpdl-size 750)))
    (if (not infile)
      (setq infile
	    (concat listen-path "/grammar.elg")))
  (if (not (string-match "\\.elg$" infile))
      (error "File name must end with .elg"))
  (let ((out-file-table
	 (concat (substring infile 0 -3) "lcm"))
	(out-file-list
	 (concat (substring infile 0 -3) "txt")))
    ;; figure out whether to use an existing buffer b1 (and write a message
    ;; if it has not saved) or create a new buffer b2
    (let ((b 
	   (let ((b1 (get-file-buffer infile)))
	     (cond  
	      (b1
	       (if (buffer-modified-p b1)
		   (message
		    "Listen: grammar file has not been saved"))
	       b1)
	      (t (let ((b2 (create-file-buffer infile)))
		   (set-buffer b2)
		   (insert-file-contents infile nil)
		   b2))))))
      (set-buffer b)
      (listen-grammar-mode)
      (set-visited-file-name infile)
      (set-buffer-modified-p nil)
      (let ((g (listen-grammar-parse-buffer b)))
	;; create command table file
	(let ((b-out (create-file-buffer out-file-table)))
	  (unwind-protect
	      (progn
		(set-buffer b-out)
		(listen-grammar-to-command-table g)
		(write-region (point-min) (point-max) out-file-table)
		(kill-buffer b-out))
	    ;; cleanup
	    (kill-buffer b-out)))
	;; create word list file
	(let ((b-out (create-file-buffer out-file-list)))
	  (unwind-protect
	      (progn
		(set-buffer b-out)
		(listen-grammar-to-DNS-list g)
		(write-region (point-min) (point-max) out-file-list)
		(kill-buffer b-out))
	    ;; cleanup
	    (kill-buffer b-out))))))))

;;; USER STUFF

(defvar listen-grammar-file
  (concat listen-path "/grammar.elg"))

(defvar listen-grammar-dragon-file
  (concat (substring listen-grammar-file 0 -3) "ddx"))  

(defun edit-listen-grammar ()
  (interactive)  
  (find-file listen-grammar-file))

(defun compile-listen-grammar ()
  (interactive)
  (listen-grammar-file-to-dragon listen-grammar-file
				listen-grammar-dragon-file))

(provide 'listen-grammar)

;;; perhaps the grammar file should also be able to contain other
;;; information:
;;; 
;;;   * keystroke/mouseclick combinations instead of function names
;;;   * additional dragon import stuff (enh, portability is nice)
;;;   * elisp code (to be put where?)







