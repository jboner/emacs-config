;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VoiceCode, a programming-by-voice environment
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
; (C)2000, National Research Council of Canada
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Useful macros for writing Python code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar py-no-doc nil
  "Set to true if you don't want to generate template documentation for methods and classes."
)

(defvar py-no-construct nil
 "Set to true if you don't want to generate constructors for classes." 
)


;;;
;;; Set the string of blank spaces used to indent by one level
;;;
(defvar py-indent-padding
      (progn
	(let ((counter py-indent-offset) (buff ""))
	  (while (> counter 0)
	    (setq buff (concat buff " "))
	    (setq counter (- counter 1))
	    )
	  buff
	  )
 	)
)
; (defvar py-indent-padding2 (concat py-indent-padding py-indent-padding))
; (defvar py-indent-padding3 (concat py-indent-padding py-indent-padding2))


(defun pym (name arg-string)
  (interactive "sName: \nsArguments: ")
  "Inserts template code for a python method."

  (py-method name arg-string 1)
)
(defun pyf (name arg-string)
  (interactive "sName: \nsArguments: ")
  "Inserts template code for a python function.

When function ends, the template for the shor/long summary of the method is selected so that it can easily be deleted with Ctrl-w"

  (py-method name arg-string nil)
)

(defun py-method (name arg-string is-method)

  (let ((arg-list nil) (an-arg nil) (arg-num nil) 
 	(desc-start nil) (desc-end nil))
    
    (py-insert-code "\n\n" 1)
    ;;;
    ;;; Determine list of arguments (check if 1st argument is self)
    ;;;
    (setq arg-list (split-string arg-string "[ ,]+"))
    (if (string= (elt arg-list 0) "self")
	(progn
	  (setq arg-list (cdr arg-list))
	  ;;; Interpret this as a method, even if it was called with 
	  ;;; is-method set to nil
	  (setq is-method 1)
	  )
	)

    ;;;
    ;;; Print signature of method 
    ;;; NOTE: we don't indent code for this first line because
    ;;;        Python allows nested def statements. So if a def follows
    ;;;        an other, the second will be indented one level higher
    ;;;        than the first one.
    ;;;
    (beginning-of-line)    
    (py-insert-code (concat py-indent-padding "def " name "(") 1)
    (if is-method (insert "self"))
    (setq arg-num 0)
     (while (< arg-num (length arg-list))
       (if (or (> arg-num 0) is-method) (py-insert-code ", " 1))
       (py-insert-code (elt arg-list arg-num) 1)
       (setq arg-num (+ 1 arg-num))
       )
    (py-insert-code "):\n" 1)

    ;;;
    ;;; Print documentation for method and its arguments
    ;;;
    (py-insert-doc "\"\"\"")
    (setq desc-start (point))
    (py-insert-doc "... Terse 1 line description ...\n\n... Detailed description ...")
    (setq desc-end (point))
    (py-insert-doc "\n\n**INPUTS**\n\n")
   
    
    (mapcar #'(lambda (an-arg) (py-insert-doc (concat "ANY *" an-arg "* -- undocumented \n\n"))) arg-list)
    (if (eq 0 (length arg-list)) (py-insert-doc "*none* -- \n\n"))
    
    (py-insert-doc "\n**OUTPUTS**\n\n*none* -- \n\"\"\"")

    ;;;
    ;;; Insert stub
    ;;;
    (py-insert-code (concat "\n\ndebug.not_implemented('" name "')\n"))

    ;;;
    ;;; Move cursor to short description
    ;;;
    (set-mark desc-end)
    (goto-char desc-start)
    )
)

(defun pyoc (class-name super-classes attrs)
  (interactive "sName: \nsSuperclasses: \nsAttributes: ")
  "Writes template code for an subclass of Object (as defined in Object.py) and its constructor (__init__).

When function ends, the template for the shor/long summary of the class is selected so that it can easily be deleted with Ctrl-w"
  (py-class class-name super-classes attrs 1)
)  

(defun pyc (class-name super-classes attrs exclude-bases)
  (interactive "sName: \nsSuperclasses: \nsAttributes: \nsExclude base classes: ")
  "Writes template code for a class that is not a subclass of Object (as defined in Object.py)

When function ends, the template for the shor/long summary of the class is selected so that it can easily be deleted with Ctrl-w"
  (py-class class-name super-classes attrs exclude-bases nil)
)

(defun py-class (class-name super-classes attrs exclude-bases is-object)
  (let ((descr-start nil) (descr-end nil) (attr-list nil) (super-list nil))

    (beginning-of-line)
    (py-insert-code (concat "class " class-name))

    (if (not (string= "" super-classes)) (py-insert-code (concat "(" super-classes ")")))

     ;;;
     ;;; Get the list of the classe's attributes and super classes
     ;;;
     (setq attr-list (py-parse-named-args attrs))
     (setq super-list (split-string super-classes "[ ,]+"))
     (setq exclude-list (split-string exclude-bases "[ ,]+"))

     (py-insert-code ":\n")
     (py-insert-doc "\"\"\"")
     (setq descr-start (point))
     (py-insert-doc "... Terse 1 line description here ...\n\n... Detailed description here ...")
     (setq descr-end (point))
     (py-insert-doc "\n\n**INSTANCE ATTRIBUTES**\n\n")

     ;;;
     ;;; Insert documentation for instance attributes
     ;;;
     (if (> (length attr-list) 0)
	 (mapcar #'(lambda (an-attr) (py-doc-attr an-attr "ANY")) attr-list)
       (py-insert-doc "*none*-- \n")
       )
     (py-insert-doc "\nCLASS ATTRIBUTES**\n\n*none* -- \n\"\"\"")

     ;;;
     ;;; Insert constructor for the class
     ;;;
     (if (not py-no-construct) (py-constructor class-name attr-list exclude-list is-object))
    
     ;;;
     ;;; Mark description placeholders
     ;;;
     (set-mark descr-end)
     (goto-char descr-start)    
     ;;;
     ;;; Mark description placeholders
     ;;;
     (set-mark descr-end)
     (goto-char descr-start)
    )
)

(defun pyda ()
  (interactive)
  "Inserts code for documenting a new class attribute"
  (let ((old-py-no-doc py-no-doc))
    (setq py-no-doc nil)
    (call-interactively 'py-doc-attr)
    (setq py-no-doc old-py-no-doc)
    )
)
(defun py-doc-attr (name-value-pair type) 
  (interactive "sName: \nsType: ")
  "Inserts template documentation for a class attribute"
  (let ((name nil) (value nil))
    (if (not py-no-doc)
	(progn
	  (if (string= "" type) (setq type "ANY"))
	  (setq name (elt name-value-pair 0))
	  (setq value (elt name-value-pair 1))
	  (py-insert-code (concat type " *" name "=" value "* -- undocumented\n"))
	  )
      )
    )
)

(defun py-insert-code (code &optional no-indent)
  "Insert some code in a Python buffer. Indent code unless no-indent is true."
  (let ((line-list nil))

    ;;;
    ;;; Insert each line and indent
    ;;; Make sure you insert leading/trailing "\n"
    ;;;
    (setq line-list (split-string code "\n"))
    (if (string= (substring code 0 1) "\n") (insert "\n"))
    (mapcar #'(lambda (a-line) (insert a-line) (if (not no-indent) (indent-for-tab-command)) (insert "\n")) line-list)
    (if (string= (substring code (- (length code) 1) (length code)) "\n") (insert "\n"))

    ;;;
    ;;; Delete "\n" inserted by last iteration of mapcar
    ;;;
    (backward-delete-char-untabify 1)
    )
)

(defun py-insert-doc (doc)
  "Insert some documentation unless py-no-doc is true."
  (if (not py-no-doc) (py-insert-code doc))
)

(defun py-parse-named-args (args-string)
  "Parse a list of Python named arguments (i.e. Name = Value)

Returns a list of pairs with name and value of the argument."

  (let ((arg-list nil) (name-value-list (list)))
    (setq arg-list (split-string args-string "[, ]+"))
    (setq name-value-list (mapcar #'(lambda (an-arg) (py-parse-a-named-arg an-arg)) arg-list))
    )
)

(defun py-parse-a-named-arg (an-arg)
  (let ((name-value-pair nil) (name nil) (value "None"))
;    (message (concat "an-arg='" an-arg "'"))
    (setq name-value-pair (split-string an-arg " *= *"))
;    (message (concat "Hi elt0=" (elt name-value-pair 0) "elt1=" (elt name-value-pair 1) "elt3=" (elt name-value-pair 3) "elt4=" (elt name-value-pair 4)))
    (setq name (elt name-value-pair 0))
;    (message (concat "(lengt name-value-pair)=" (number-to-string (length name-value-pair))))
    (if (> (length name-value-pair) 1) (setq value (elt name-value-pair 1)))
    (list name value)
    )
)

(defun py-constructor  (class-name attr-list exclude-bases is-object)
  "Insert code for constructor of a class class-name with attributes attr-list (name-value pairs). Do not invoke constructor of classes in exclude-bases. If is-object is true, build a constructor for a subclass of Object (as defined in Object.py"

  (py-insert-code "\n\ndef __init__(self")
  (mapcar #'(lambda (name-val) (py-insert-code (concat ", " (elt name-val 0) "=" (elt name-val 1)))) attr-list)
  (py-insert-code ", **args_super):\n")

  ;;;
  ;;; Call self.deep_construct 
  ;;;
  (py-insert-code (concat "self.deep_construct(" class-name ", \n"))

  ;;;
  ;;; With list of attributes
  ;;;
  (if (> (length attr-list) 0)
      (progn
	(py-insert-code "{")
	(mapcar #'(lambda (an-attr) (py-insert-code (concat "'" (elt an-attr 0) "'" ": " (elt an-attr 0) ", \n"))) attr-list)
                  ;;; Delete last ", "
	(backward-delete-char-untabify 4)
	(py-insert-code "}")
	)
    (py-insert-code "{}")
    )

  (py-insert-code ", \nargs_super, \n")

  (if (> (length exclude-list) 0)
      (progn
	(py-insert-code "{")
	(mapcar #'(lambda (a-base) (py-insert-code (concat "'" a-base "': 1, \n"))) exclude-bases)

	(backward-delete-char-untabify 4)
	(py-insert-code "}")
	)
    (py-insert-code "{}")
    )

  (py-insert-code ")\n")
  
)

(defun py-test ()
  (interactive)
  (switch-to-buffer "*PYTEST*")
  (kill-buffer "*PYTEST*")
  (switch-to-buffer "*PYTEST*")
  (python-mode)
  (erase-buffer)
  (py-class "Blah" "S1, S2" "x=0, y=1" "S2" 1)
  (end-of-buffer)
;  (py-class "Blob" "" "" "" 1)
  (py-method "a_method" "arg1=1, arg2=2, arg3" 1)
  (end-of-buffer)
  (py-method "a_function" "arg1=1, arg2=2, arg3" nil)
  (end-of-buffer)
)


(defun pyla (attr class module)
  (interactive "sAttribute: \nsClass: \nsModule: ")
  "Inserts definition of a hyperlink to a class attribute.

If module is '.', then point inside the documentation for current module

If module is '', then point to documentation for module with same name as class.

When function terminates, the anchor text is selected so it can be deleted with Ctrl-w and replaced by something else
"
  (pydoc-link attr "" class module)
)

(defun pylm (method class module)
  (interactive "sMethod: \nsClass: \nsModule: ")
  "Inserts definition of a hyperlink to a class method.

If module is '.', then point inside the documentation for current module

If module is '', then point to documentation for module with same name as class.

When function terminates, the anchor text is selected so it can be deleted with Ctrl-w and replaced by something else
"
  (pydoc-link method method class module)
)

(defun pylc (method class module)
  (interactive "sClass: \nsModule: ")
  "Inserts definition of a hyperlink to a class.

If module is '.', then point inside the documentation for current module

If module is '', then point to documentation for module with same name as class.

When function terminates, the anchor text is selected so it can be deleted with Ctrl-w and replaced by something else
"
  (pydoc-link method method class module)
)

(defun pydl ()
  (interactive)
  (call-interactively 'pydoc-link)
)
(defun pydoc-link (anchor method class module)
  (interactive "sAnchor: \nsMethod: \nsClass: \nsModule: ")
  "Inserts definition of a hyperlink for a pythondoc docstring

If module is '.', then point inside the documentation for current module

If module is '', then point to documentation for module with same name as class.

If class is '', then point to the doc for module.

Elese if method is '', then point to the doc for the class.

Else point to the method documentation."

  (let ((anch-start nil) (anch-end nil))

    (if (string= module ".") (setq module (py-curr-module-name))
      (if (string= module "") (setq module class))
      )
    (if (not (string= class "")) (setq class (concat "." class)))
    (if (not (string= method "")) 
	(if (not (string= class "")) (setq method (concat "#" module class "." method)))
      )
    (py-insert-doc (concat ".. [" ))
    (setq anch-start (point))
    (py-insert-doc (concat anchor))
    (setq anch-end (point))
    (py-insert-doc (concat "] file:///"  module class ".html" method))
    (set-mark anch-end)    
    (goto-char anch-start)
    )
)


(defun py-curr-module-name ()
  (let ((bname (buffer-name)))
    (substring bname 0 (- (length bname) 3))
    )
)


(defun comment-traces ()
  "Comments out trace statements"
  (interactive)
  (save-excursion
	(beginning-of-buffer)
	(query-replace-regexp "\n\\( *\\)print\\( *\\)'--" "\n#\\1print\\2'--")
  )
)

(defun delete-trace-outputs ()
   "Deletes outputs of traces from an output file"
   (interactive)
   (while 1
      (progn
         (search-forward-regexp "^-- ")
         (kill-line)
         (backward-delete-char 4)         
      )
   )
)