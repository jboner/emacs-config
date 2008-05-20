;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wddx.el: macros for parsing and generating WDDX messages.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;;
;;; (C)2002, National Research Council of Canada
;;;
;;; Written and maintained by Alain Desilets (alain.desilets@nrc.ca)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for translating between WDDX string and Emacs-Lisp data structure
;;;
;;; WDDX is an XML-based protocol for exchanging data structures between 
;;; different programming languages.
;;;
;;; See http://www.openwddx.org/ for details
;;;
;;; wddx.el makes it possible for Emacs to communicate with applications 
;;; written in other languages by exchanging WDDX messages (for example
;;; through a socket connection).
;;;
;;; wddx.el requires the file xml.el (by Emmanuel Briot)
;;;
;;; There are 2 main public functions:
;;;
;;; - wddx-deserialize: 
;;;      parses a WDDX string and converts it to an Emacs-Lisp data structure
;;;
;;; - wddx-serialize:
;;;      converts an Emacs-Lisp data structure to a WDDX string
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (substitute-in-file-name "$VCODE_HOME/Environments/Emacs/xml.el"))

;;;
;;; Increase this when debugging the script, so can see more of the traces
;;;
(setq message-log-max 1000)

(defun wddx-deserialize (dom)
   "Converts a DOM tree parsed from a WDDX message, into a Lisp data structure"

;   (message (format "-- wddx-deserialize: dom=%S" dom))
   
   (let ((root nil) (value nil) (found-data-node nil))

       ;;;
       ;;; Skip name attributes of root node
       ;;;
       (setq root (elt dom 0))
       (setq children-nodes (cdr (cdr root)))
   
       ;;;
       ;;; Skip nodes until we reach <data> node
       ;;;
       (while (and children-nodes (not found-data-node))
         (setq data-node (car children-nodes))
         (setq children-nodes (cdr children-nodes))
         (if (string= "data" (car data-node))
      	   (setq found-data-node t)
         )
       )

       (setq value (wddx-deserialize-data-node data-node))
       value
   )
)

(defun wddx-deserialize-data-node (data-node)
   "Deserializes a WDDX data node"

;   (message (format "-- wddx-deserialize-data-node: data-node=%S" data-node))

   (let ((value nil))
      
     ;;;
     ;;; Skip name and attributes
     ;;;
     (setq value-node (car (cdr (cdr data-node))))
     (setq value (wddx-deserialize-value-node value-node))
      value
   )
)

(defun wddx-deserialize-value-node (value-node)
;   (message (format "-- wddx-deserialize-value-node: value-node=%S" value-node))
   (let ((value nil) (value-type nil) (value-attrs nil))
     (setq value-type (car value-node))
     (setq value-attrs (car (cdr value-node)))
     (setq value-descr (cdr (cdr value-node)))
     (if (string= value-type "struct")
 	(setq value (wddx-deserialize-struct-value-node value-descr)))
     (if (string= value-type "array")
 	  (setq value (wddx-deserialize-array-value value-descr)))
     (if (string= value-type "number")
 	  (setq value (wddx-deserialize-number-value value-descr)))
     (if (string= value-type "string")
 	  (setq value (wddx-deserialize-string-value value-descr)))      
     value
   )  
)

(defun wddx-deserialize-var-node (var-node)
;   (message (format "-- wddx-deserialize-var-node: var-node=%S" var-node))
   (let ((value nil) (key nil))
      (setq key (cdr (elt (elt var-node 1) 0)))
      (setq value-node (elt var-node 2))
      (setq value (wddx-deserialize-value-node value-node))
      (setq value (list key value))
;      (message (format "-- wddx-deserialize-var-node: returning value=%S" value))
      value
   )
)

(defun wddx-deserialize-struct-value-node (struct-value-node)
;  (message (format "-- wddx-deserialize-struct-value-node: struct-value-node=%S" struct-value-node))

  (let ((value nil) (hash-entries nil) (a-hash-entry nil))
    (setq hash-entries (mapcar 'wddx-deserialize-var-node struct-value-node))
    (setq value (make-hash-table :test 'string=))
    (while hash-entries
       (setq a-hash-entry (car hash-entries))
       (setq hash-entries (cdr hash-entries))
       (setq key (elt a-hash-entry 0))
       (setq val (elt a-hash-entry 1))
       (cl-puthash key val value)
    )
    value
  )
)

(defun wddx-deserialize-array-value (value-descr)
;  (message (format "-- wddx-deserialize-aray-value: value-attrs=%S, value-descr=%S" value-attrs value-descr))

  (let ((value nil))
    (setq value (mapcar 'wddx-deserialize-value-node value-descr))
    value
  )
)

(defun wddx-deserialize-number-value (value-descr)
;  (message (format "-- wddx-deserialize-number-value: value-descr=%S" value-descr))

  (let ((value nil) (str-number))
    (setq str-number (elt value-descr 0))
    (setq value (string-to-number str-number))
    value
  )
)

(defun wddx-deserialize-string-value (value-descr)
;  (message (format "-- wddx-deserialize-string-value: value-descr=%S" value-descr))

  (let ((value nil))
    (setq value (elt value-descr 0))
    value
  )
)

(defun wddx-serialize-data (data)
   "Encodes a LISP data structure into a series of WDDX tags"

   (let (data-type (wddx-string ""))
     (cond 
      ((numberp data) (setq wddx-string (wddx-serialize-number data)))
      ((and (listp data) (not (hash-table-p data))) (setq wddx-string (wddx-serialize-list data)))
      ((hash-table-p data) (setq wddx-string (wddx-serialize-hash data)))
      ((stringp data) (setq wddx-string (wddx-serialize-string data)))
      (t (error (format "Error: wddx-serialize-data cannot serialize item of type %s.\nItem was: %S" (type-of data) data)))
     )
     wddx-string
   )
)

(defun wddx-serialize-number (number-data)
;   (message (format "-- wddx-serialize-number: number-data=%S" number-data))
   (let ((number-str nil))
      (setq number-str (format "<number>%S</number>" number-data))
;      (message (format "-- wddx-serialize-number: returning number-str=%S" number-str))
      number-str
   )
)

(defun wddx-serialize-string (str-data)
   (let ((string-str nil))
      ;;; Replace special characters like <, > to &lt;, &gt; etc...
      (setq str-data (xml-reverse-substitute-special str-data))

      (setq string-str (format "<string>%s</string>" str-data))
      string-str
   )
)

(defun wddx-serialize-list (list-data)
;   (message (format "-- wddx-serialize-list: list-data=%S" list-data))
   (let ((list-str nil))
     (setq list-str (format "<array length=\"%S\">" (length list-data)))
     (while list-data
       (setq an-elt (car list-data))
       (setq list-data (cdr list-data))
       (setq list-str (concat list-str (wddx-serialize-data an-elt)))
     )
     (setq list-str (concat list-str "</array>"))
;     (message (format "-- wddx-serialize-list: returning list-str=%S" list-str))
     list-str
   )
)

(defun wddx-serialize-hash (hash-data)
;   (message (format "-- wddx-serialize-hash: hash-data=%S" hash-data))
   (let ((hash-str nil) (items nil))
     (setq items (hash-items hash-data))
;     (message (format "-- wddx-serialize-hash: items=%S" items))
     (setq hash-str "<struct>")
     (while items
       (setq an-item (car items))
       (setq items (cdr items))
;       (message (format "-- wddx-serialize-hash: an-item=%S" an-item))
       (setq hash-str (concat hash-str (format "<var name=\"%s\">%s</var>" (elt an-item 0) (wddx-serialize-data (elt an-item 1)))))
     )
     (setq hash-str (concat hash-str "</struct>"))
;    (message (format "-- wddx-serialize-hash: returning hash-str=%S" hash-str)) 
     hash-str
   )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some utilities for manipulating hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar collected-items nil)

(defun collect-hash-items (key value)
;  (message (format "-- collect-hash-items: key=%S, value=%S" key value))
  (push (list key value) collected-items)
)
(defun hash-items (hash-table)
   "Returns a list of (key value) pairs contained in a hash"

   (setq collected-items (list))
   (cl-maphash 'collect-hash-items hash-table)
;   (message "-- hash-items: collected-items=%S" collected-items)
   (copy-list collected-items)   
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some XML utilities which are not in xml.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun old-xml-reverse-substitute-special (string)

  "Return STRING, after subsituting special XML sequences (opposite
   direction from function xml-substitute-special, e.g. \"<\" -> \"&lt;\")."

  (let  ((match-from-pos 0))
      ;;;
      ;;; For &amp -> &, need to make sure we move up one character after
      ;;; substitution, otherwise you forever match the & in the last 
      ;;; inserted &amp;
      ;;;
    (while (string-match "&" string match-from-pos)
      (progn
	(setq string (replace-match "&amp;"  t nil string))
	(setq match-from-pos (1+ (match-end 0)))
	)
    )
  )

  (while (string-match "<" string)
    (setq string (replace-match "&lt;"  t nil string)))
  (while (string-match ">" string)
    (setq string (replace-match "&gt;"  t nil string)))
  (while (string-match "'" string)
    (setq string (replace-match "&apos;"  t nil string)))
  (while (string-match "\"" string)
    (setq string (replace-match "&quot;" t nil string)))
  string
)

(defun my-suball (original regex replacement)
  "return a new string with all matches of regex replaced with
  replacement"
  (save-match-data
    (let ((match-from-pos 0) (new-string ""))
      (while (string-match regex original match-from-pos)
        (progn
          (setq new-string 
               (concat new-string 
                       (substring original match-from-pos (match-beginning 0))
                       replacement
               )
          )
          (setq match-from-pos (match-end 0))
        )
      )
     (concat new-string (substring original match-from-pos))
     )
  )
)

(defun my-alt-suball (original regex replacement)
  "return a new string with all matches of regex replaced with
  replacement"
;  oops this is bad: split-string doesn't include empty strings for
;  multiple consecutive matches, or matches at the start and end of the
; string
  (save-match-data
    (mapconcat 'identity (split-string original regex) replacement)
  )
)

(defun my-xml-reverse-substitute-special (string)

  "Return STRING, after subsituting special XML sequences (opposite
   direction from function xml-substitute-special, e.g. \"<\" -> \"&lt;\")."

  (let ((new-string nil))
    (setq new-string (my-suball string "&" "&amp;"))
    (setq new-string (my-suball new-string "<" "&lt;"))
    (setq new-string (my-suball new-string ">" "&gt;"))
    (setq new-string (my-suball new-string "'" "&apos;"))
    (setq new-string (my-suball new-string "\"" "&quot;"))
    new-string
  )
)

(defun xml-reverse-substitute-special (string)

  "Return STRING, after subsituting special XML sequences (opposite
   direction from function xml-substitute-special, e.g. \"<\" -> \"&lt;\")."

  (let ((new-string nil))
    (setq new-string (replace-regexp-in-string "&" "&amp;" string))
    (setq new-string (replace-regexp-in-string "<" "&lt;" new-string))
    (setq new-string (replace-regexp-in-string ">" "&gt;" new-string))
    (setq new-string (replace-regexp-in-string "'" "&apos;" new-string))
    (setq new-string (replace-regexp-in-string "\"" "&quot;" new-string))
    new-string
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WDDX encodes Python None value as an empty string. Use these functions to
;;; to coerce values that may include such empty strings, into the actual
;;  Lisp nil value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wddx-coerce-int (value)
  "Coerce the value into an int or 'nil if it is the empty string"
  (let ((coerced-value value))
    (if (stringp value)
	(if (string= "")
	    (setq coerced-value nil)
	  (setq coerced-value (string-to-int value))
	  )
      )
    coerced-value
    )    
)

(defun wddx-coerce-int-list (value)
  "Coerce elements of a list into int or 'nil (the later in the case where"
  (let ((coerced-value (list)) (element))
    (while value
      (setq value (cdr value))
      (setq element (car value))
      (setq corced-value (append coerced-value (wddx-coerce-int element)))
       )
    coerced-value
    )
)

(defun wddx-coerce-string (value)
 "Coerce the value into a string, or the empty string if it's nil."
 (let ((coerced-value value))
   (if (not coerced-value)
       (setq coerced-value "")
     )
   coerced-value
   )
)
