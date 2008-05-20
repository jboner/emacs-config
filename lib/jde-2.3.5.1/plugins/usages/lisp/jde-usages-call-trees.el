;;; jde-usages-call-trees.el --- usage (call tree) commands

;;; Commentary:
;; 


;;; History:
;; 

(require 'jde-usages-bsh)
(require 'jde-usages-util)
(require 'jde-usages-eval-type-of)

;;; Code:

(defcustom jde-usages-always-check-callers nil
  "To go to the exact location of a method call on a line 
jde-usages-dct-goto-caller uses a regexp to match the method
name. If however there is more than one match on the line
jde-usages-dct-goto-caller uses jde-parse-java-variable-at-point
and jde-jde-usages-parse-eval-type-of to determine which location
is the correct method call. Seting
`jde-usages-always-check-callers' to non-nil enables these checks
even if there is only one match on the line for the method
name. This is useful if the source file has changed after
compiling."
  :group 'jde-usages
  :type 'boolean)


(defvar jde-usages-related-classes nil
"Stores the list of \"related\" classes for the last signature
for which callers were found. Its value is set in
ASMUsages.findUsages(String,Usages.Signature,boolean).")

(defvar jde-usages-call-tree-keymap (make-keymap)
  "Keymap for *usages* buffers.")

(set-keymap-parent jde-usages-call-tree-keymap widget-keymap)

(define-key jde-usages-call-tree-keymap "q" 'delete-window)
(define-key jde-usages-call-tree-keymap "?" 'jde-usages-call-trees-help)
(define-key jde-usages-call-tree-keymap "n" 'jde-usages-buffers-next-pos)
(define-key jde-usages-call-tree-keymap "p" 'jde-usages-buffers-prev-pos)


(defun jde-usages-call-trees-help ()
  "Display help for various jde-usages call tree buffer commands."
  (interactive)
  (if (eq last-command 'jde-usages-class-trees-help)
      (describe-function 'jde-usages-call-trees-help)
    (message
     (substitute-command-keys
      "`\\[jde-usages-next-pos]':next caller `\\[jde-usages-prev-pos]':previous caller \
`\\[delete-window]':delete window"))))

(defun jde-usages-display-call-tree (&optional strict initial-signature-func)
  "Display an interactive call tree.
STRICT should be true if the callers of interfaces to a function,
or calls to a superclass which may result in a virtual function
call to the subclass should not be considered.  In other words,
if STRICT is true, then only calls that are definitely to the
requested function are considered.  This function calls
INITIAL-SIGNATURE-FUNC to get the method for which the call tree
is generated.  If initial-signature-func is nil,
`jde-usages-get-current-signature' is used."
  (interactive "P")

  (labels ((jde-usages-dct-goto-caller (caller calling)
             (let ((ret (jde-usages-find-class-source (car caller) t)))
               (if (not ret)
                   (message "Could not find sourcefile for %s" (car caller))
                 ;; else
                 (goto-line (nth 4 caller))
                 ;; try and navigate to method call
                 (let*
                       ((begin (point)) ;; goto-line places point at the beginning of the line
                        (end (progn (end-of-line) (point)))
                        (count (+ 1 (nth 6 caller))) ;; method count for that line
                        (method-name (nth 2 calling))
                        (is-constructor (equal "<init>" method-name))
                        (method-name-or-constructor (cond 
                                                     (is-constructor
                                                      (concat "new\\s *[[:word:]$.]*"
                                                              ;; get the unqualified classname
                                                              (jde-usages-get-simple-class-name (nth 1 calling))))
                                                     ((equal "" method-name) "\\w+")
                                                     (t method-name)))
                        (method-name-regexp (concat "\\b" method-name-or-constructor
                                                    "\\b"))
                        )
                   (goto-char begin)
                   (let (mb me)
                     (when (if (or jde-usages-always-check-callers (save-excursion (re-search-forward method-name-regexp end t 2)))
                               ;; this method name occurs on this line more than once, so use jde-parse-eval-type-of to find the one(s) we want
                               (progn 
                                 (while  (and (> count 0)
                                              (re-search-forward method-name-regexp end t))
                                   (setq mb (match-beginning 0))
                                     (setq me (match-end 0))
                                     (if is-constructor
                                         (decf count)
                                       (if (and (not (jde-usages-at-comment-or-string (point))) ;; point is not in a comment or string
                                                (let* ((parse-data (jde-parse-java-variable-at-point))
                                                       (object (if (and (car parse-data) (not (equal "" (car parse-data))))
                                                                   (car parse-data)
                                                                 "this"))
                                                       (class-name (jde-usages-parse-eval-type-of object)))
                                                  (if jde-usages-related-classes
                                                      (member (replace-regexp-in-string "\\." "/" class-name)  jde-usages-related-classes)
                                                    (equal class-name (nth 1 calling)))))
                                           (decf count))))
                                   (eq count 0))
                               (re-search-forward method-name-regexp end t))
                         (jde-usages-momentarily-highlight-last-match mb me))
                       )))
               ret))
           (jde-usages-dct-tree-get-children-from-tree (tree)
             (jde-usages-dct-tree-get-children (widget-get tree :sig) (widget-get tree :strict)))

           (jde-usages-dct-caller-to-sig (caller)
             (list 'function (nth 0 caller) (nth 1 caller) (when (nth 2 caller) (jde-parse-get-unqualified-name (nth 2 caller)))
                   (mapcar 'jde-parse-get-unqualified-name (nth 3 caller))))

           (jde-usages-dct-tree-get-children (sig &optional strict)
             (when sig
               (let ((callers (jde-usages-get-callers sig strict)))
                 (if (stringp callers)
                     ;; this can only happen when sig is a static final var
                     callers
                   (mapcar
                    (lambda (caller)
                      (if (listp caller)
                          (lexical-let*
                              ((sig sig)
                               (caller caller)
                               (caller-sig (jde-usages-dct-caller-to-sig caller))
                               (method-name (nth 1 caller))
                               (open (and (> (length method-name) 7) (equal "access$" (substring method-name 0 7))))
                               (tag (concat (when (eq 'variable (nth 0 sig)) (if (nth 5 caller) "W " "R "))
                                            (jde-usages-signature-to-string caller-sig) 
                                            ":" 
                                            (number-to-string (nth 4 caller)))))
                            (jde-usages-make-usages-tree-text tag (jde-usages-dct-goto-caller caller sig)
                                                              :dynargs 'jde-usages-dct-tree-get-children-from-tree
                                                              :sig caller-sig
                                                              :strict strict
                                                              ;; check for added accessor methods and expand them automatically
                                                              :open open
                                                              :has-children t
                                                              nil))
                        (list 'item :tag caller))) ;; class for next set of usages
                    callers))))))
    
    (lexical-let* ((initial-signature-func (or initial-signature-func 'jde-usages-get-current-signature))
                   (sig (apply initial-signature-func nil))
                   (buf-name "*usages*") ;; (concat "*" (jde-usages-signature-to-string sig) "*"))
                   (buf (progn
                          (if  (get-buffer buf-name)  (kill-buffer buf-name))
                          (get-buffer-create buf-name)))
                   (children (jde-usages-dct-tree-get-children sig strict)))
      (cond
       ((null children) (message "Cannot find usages for %s." (jde-usages-signature-to-string sig)))
       ((stringp children) (message children))
       (t (with-current-buffer buf
            (erase-buffer)
            (jde-mode)
            (apply 'widget-create (jde-usages-make-usages-tree-text (jde-usages-signature-to-string sig)
                                                                    (jde-usages-goto-sig nil sig)
                                                                    :open t
                                                                    :sig sig
                                                                    :strict strict
                                                                    children))
            (use-local-map jde-usages-call-tree-keymap)
            (widget-setup)
            (toggle-read-only)
            (goto-char (point-min))
            (jde-usages-goto-first-pos))
          (jde-usages-switch-to-buffer buf)
          (if jde-usages-window-height
              (jde-usages-set-window-height (get-buffer-window buf) jde-usages-window-height)))))))

;;; Alternate signature generation functions
(defun jde-usages-get-signature-of-thing-at-point ()
  "Return the jde-usages signature of the method definition around point."
  (if (jde-open-functions-exist)
      (let* ((thing-of-interest (thing-at-point 'symbol))
             (pair (save-excursion (end-of-thing 'symbol)
                                   (jde-parse-java-variable-at-point)))
             (args (save-excursion (end-of-thing 'symbol)
                                   (condition-case nil ;; will catch "return field;" fixed by Dag H. Wanvik <Dag.Wanvik@sun.com>
                                       (progn 
                                         (forward-sexp)
                                         (backward-sexp)
                                         (sexp-at-point))
                                     (error t))))
             (class (if (listp args) 'function 'variable))
             (args-length (if (consp args)
                              (if (> (length args) 1)
                                  ;; length = number of commas + 1
                                  (let ((count 1))
                                    (mapc (lambda (x)
                                            (when (and (symbolp x) (string-match ",$" (symbol-name x)))
                                              (incf count)))
                                          args)
                                    count)
                                1) ;; (length args) = 1
                            0))
             (class-to-open (if (equal "" (car pair))
                                (jde-usages-get-current-class)
                                (jde-open-get-class-to-open pair thing-of-interest))))
        
        (if (and class-to-open (stringp class-to-open))
            (list class class-to-open thing-of-interest "" args-length)
          (error "Cannot determine the class of the symbol at point!")))
    (message "You need JDE >= 2.2.6 and Senator for using this feature!")))

(defun jde-usages-get-signature-interactively (&optional class-name method-name args-length)
  "Return a interactively specified method signature.
Prompts the user for a CLASS-NAME, METHOD-NAME and the number of
arguments to this method, ARGS-LENGTH, if these are not specified
when calling this function."
  (let
      ((class-name (or class-name (jde-usages-read-class-with-completion "Class or interface:" nil (list (or class-name (jde-usages-get-current-class))))))
       (method-name (or method-name (read-from-minibuffer "Method name:" "<init>")))
       (args-length (or args-length (string-to-number (read-from-minibuffer "Number of args:" "0")))))
  (list 'function class-name method-name "" args-length)))


(defun jde-usages-display-call-tree-for-thing-at-point (&optional strict)
  "Displays all the callers to the method at point.
It uses the function
`jde-usages-display-call-tree-for-specified-class' to guess the
signature of the method at point.  Optional argument STRICT If
non-nil only callers to which use this exact class will be
listed."
  (interactive "P")
  (jde-usages-display-call-tree strict 'jde-usages-get-signature-of-thing-at-point))

(defun jde-usages-display-call-tree-for-specified-class (&optional strict)
  "Display a call tree for an interactively specified method.
Accepts from the minibuffer a class name, a method name and the
number of args for this method and then uses
`jde-usages-display-call-tree' to get the call tree for this
signature.  Optional argument STRICT If non-nil only callers to
which use this exact class will be listed."
  (interactive "P")
  (jde-usages-display-call-tree strict 'jde-usages-get-signature-interactively))

(defun jde-usages-get-callers (sig &optional strict)
  "Call the java code to get the callers for this signature.
Argument SIG : method signature.  Optional argument STRICT : if
non-nil only returns callers to the exact class mentioned in SIG,
otherwise return calls to super and sub classes that could result
in a call to SIG."
  (let* ((class-name (nth 1 sig))
         (method-name (nth 2 sig))
         (args (nth 4 sig))
         (args-str (cond ;; the sig could have a number instead f the args list
                    ((numberp args) args)
                    (args (list (concat "new String[] {\"" (mapconcat 'identity  args "\", \"") "\"}")))
                    (t (list "new String[] {}"))))
         (bsh-eval-timeout 100)
         (is-field-str (if (eq 'variable (nth 0 sig)) 'true 'false))
         (strict-str (if strict 'true 'false))
         )
    (if (and method-name (not (equal method-name "")))
        (jde-usages-jeval-r 'jde.util.Usages.findUsages class-name method-name args-str is-field-str strict-str 'System.out)
      (jde-usages-jeval-r 'jde.util.Usages.findUsages class-name strict-str 'System.out))))


(defun jde-usages-get-class-which-defines-sig (sig)
  "Call the java code to get the class where this signature is defined.
Argument SIG : method signature."
  (let* ((class-name (nth 1 sig))
         (method-name (nth 2 sig))
         (args (nth 4 sig))
         (args-str (cond ;; the sig could have a number instead f the args list
                    ((numberp args) args)
                    (args (list (concat "new String[] {\"" (mapconcat 'identity  args "\", \"") "\"}")))
                    (t (list "new String[] {}"))))
         (bsh-eval-timeout 100)
         (is-field-str (if (eq 'variable (nth 0 sig)) 'true 'false))
         )
    (jde-usages-jeval-r 'jde.util.Usages.getClassWithSignatureDefinition class-name method-name args-str is-field-str 'System.out)))


(provide 'jde-usages-call-trees)
;;; jde-usages-call-trees.el ends here
