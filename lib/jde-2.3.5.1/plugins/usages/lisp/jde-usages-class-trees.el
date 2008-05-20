;;; jde-usages-class-trees.el --- subtype, supertype and type hierarchy display commands

;;; Commentary:
;; 


;;; History:
;; 

(require 'jde-usages-bsh)
(require 'jde-usages-util)

;;; Class tree display funcitons
;;; Code:

(defvar jde-usages-class-tree-keymap (make-keymap)
  "Keymap for call tree (*type-hierarchy*,*subclasses* and
  *superclasses*) buffers")

(set-keymap-parent jde-usages-class-tree-keymap widget-keymap)

(define-key jde-usages-class-tree-keymap "?" 'jde-usages-class-trees-help)
(define-key jde-usages-class-tree-keymap "q" 'delete-window)
(define-key jde-usages-class-tree-keymap "t" 'jde-usages-class-trees-display-type-hierarchy)
(define-key jde-usages-class-tree-keymap "a" 'jde-usages-class-trees-display-supers)
(define-key jde-usages-class-tree-keymap "d" 'jde-usages-class-trees-display-subs)
(define-key jde-usages-class-tree-keymap "n" 'jde-usages-buffers-next-pos)
(define-key jde-usages-class-tree-keymap "p" 'jde-usages-buffers-prev-pos)



(defun jde-usages-display-class-tree (classes buf-name &optional null-message)
  "Display a class hierarchy CLASSES in buffer BUF-NAME.
Optional parameter NULL-MESSAGE is displayed if it is present and CLASSES is null."
  (if (and null-message (null classes))
      (message null-message)
    ;; else
    (let ((buf (get-buffer-create buf-name)))
      (jde-usages-switch-to-buffer buf)
      (jde-usages-erase-usages-buffer)
      (erase-buffer)
      (apply 'widget-create
             (jde-usages-create-classes-widget-tree classes))
      
      (use-local-map jde-usages-class-tree-keymap)
      (widget-setup)
      (toggle-read-only)
      (goto-char (point-min))
      (widget-forward 1)
      (if jde-usages-window-height
          (jde-usages-set-window-height (get-buffer-window buf) jde-usages-window-height)))))

(defun jde-usages-class-trees-help ()
  "Display help for various jde-usages class tree buffer commands."
  (interactive)
  (if (eq last-command 'jde-usages-class-trees-help)
      (describe-function 'jde-usages-class-trees-help)
    (message
     (substitute-command-keys
      "`\\[jde-usages-class-trees-display-type-hierarchy]':type hierarchy \
`\\[jde-usages-class-trees-display-supers]':superclasses (ancestors) \
`\\[jde-usages-class-trees-display-subs]':subclasses (descendants)\
`\\[jde-usages-buffers-next-pos]':next class `\\[jde-usages-buffers-prev-pos]':previous class\
`\\[delete-window]':delete window"))))

(defun jde-usages-class-trees-get-class ()
  "Return the class name of the class widget under point if any."
  (let ((w (widget-at (point))))
    (when w
      (or (widget-get w :class) (widget-get (widget-get w :parent) :class)))))

(defun jde-usages-class-trees-display-subs ()
  (interactive)
  (jde-usages-display-subs-and-implementers t t (jde-usages-class-trees-get-class) t))

(defun jde-usages-class-trees-display-supers ()
  (interactive)
  (jde-usages-display-superclasses (jde-usages-class-trees-get-class) t))

(defun jde-usages-class-trees-display-type-hierarchy ()
  (interactive)
  (jde-usages-display-type-hierarchy (jde-usages-class-trees-get-class) t))


(defun jde-usages-get-subclasses (get-subs get-impls class-name)
  "Return the subclasses (if GET-SUBS is non-nil), and/or implementers (if GET-IMPLS is non-nil and CLASS-NAME is the name of an interface) of CLASS-NAME."
  (let* ((bsh-eval-timeout 100))
    (jde-usages-jeval-r 'jde.util.Usages.findSubs class-name  (if get-subs 'true 'false)  (if get-impls 'true 'false) 'System.out)))

(defun jde-usages-get-superclasses (class-name)
  "Return the superclasses of CLASS-NAME."
  (let* ((bsh-eval-timeout 100))
    (jde-usages-jeval-r 'jde.util.Usages.findSupers class-name  'System.out)))


(defun jde-usages-get-type-hierarchy (class-name)
  "Return the type hierarchy for CLASS-NAME."
  (let* ((bsh-eval-timeout 100))
    (jde-usages-jeval-r 'getTypeHierarchy class-name 'System.out)))

(defun jde-usages-display-subs-and-implementers (&optional get-subs-parm get-impls-parm class-name dont-prompt)
  "Display the subclasses and/or implementers of a class.
Optional argument GET-SUBS-PARM show derived classes.
Optional argument GET-IMPLS-PARM show implementors
Optional argument CLASS-NAME class name.
If optional argument DONT-PROMPT is non-nil the user is not prompted for a class name."
  (interactive)
  (let* ((class-name (if (and dont-prompt class-name)
                         class-name
                       (jde-usages-read-class-with-completion "Class or interface:" nil (list (or class-name (jde-usages-get-current-class))))))
         (buf-name "*subclasses*") ;;(concat "Subclasses for " class-name))
         ;; Treat the case where both get-subs and get-impls are nil the same as both of them being true.
         ;; This makes a call to (jde-usages-display-subs-and-implementers) the same as (jde-usages-display-subs-and-implementers t t)
         (get-impls (or get-impls-parm (not (and get-impls-parm get-subs-parm))))
         (get-subs (or get-subs-parm (not (and get-impls-parm get-subs-parm))))
         (subclasses (jde-usages-get-subclasses get-subs get-impls class-name))
         )
    (jde-usages-display-class-tree subclasses buf-name (format "%s has no subclasses" class-name))))

(defun jde-usages-display-superclasses (&optional class-name dont-prompt)
  "Display all the superclasses for CLASS-NAME.
If optional argument DONT-PROMPT is non-nil the user is not prompted for a class name."
  (interactive)
  (let* ((class-name (if (and dont-prompt class-name)
                         class-name
                       (jde-usages-read-class-with-completion "Class or interface:" nil (list (or class-name (jde-usages-get-current-class))))))
         (buf-name "*superclasses*")
         (superclasses (jde-usages-get-superclasses class-name)))
    (jde-usages-display-class-tree superclasses buf-name (format "%s has no superclasses" class-name))))

(defun jde-usages-display-type-hierarchy (&optional class-name dont-prompt)
  "Display all the type hierarchy for CLASS-NAME.
This command displays a view which combines some of the
information in the subclasses and superclass views. It shows all
the subtypes - all derived classes/interfaces and classes that
implement the interfaces, and the direct super classes. The
interfaces implemented by this class are not shown in this view,
only the superclasses view shows them.
If optional argument DONT-PROMPT is non-nil the user is not
prompted for a class name."
  (interactive)
  (let* ((class-name (if (and dont-prompt class-name)
                         class-name
                       (jde-usages-read-class-with-completion "Class or interface:" nil (list (or class-name (jde-usages-get-current-class))))))
         (buf-name "*type-hierarchy*")
         (classes (jde-usages-get-type-hierarchy class-name)))
    (jde-usages-display-class-tree classes buf-name (format "%s has neither subclasses nor superclasses" class-name))
    (when classes
      ;; move point to the node which represents class-name
      (goto-char (point-min))
      (when (re-search-forward (concat "\\b" (replace-regexp-in-string "\\(\\$\\|\\.\\)" "\\\\\\&" class-name) "\\b"))
        (goto-char (match-beginning 0))))))

(defun jde-usages-create-classes-widget-tree (classes)
  "Display a tree of CLASSES, clicking on a classname takes you to the class source."
  (if  (consp classes)
      (lexical-let* ((rest (cdr classes))
                     (name-and-disp (jde-usages-get-class-name-and-disp-name (car classes)))
                     (class-name (car name-and-disp))
                     (disp-name (cdr name-and-disp)))
        (jde-usages-make-usages-tree-text disp-name
                                          (progn 
                                            (if (jde-usages-find-class-source class-name t)
                                                (jde-usages-momentarily-highlight-last-match)
                                              (message "Could not find source for %s" class-name)))
                                          :open t
                                          :has-children t
                                          :class class-name
                                          (mapcar 'jde-usages-create-classes-widget-tree rest)))
    ;; else
    (lexical-let* ((name-and-disp (jde-usages-get-class-name-and-disp-name classes))
                   (class-name (car name-and-disp))
                   (disp-name (cdr name-and-disp)))
      (jde-usages-make-usages-tree-text disp-name
                                        (progn 
                                          (if (jde-usages-find-class-source class-name t)
                                              (jde-usages-momentarily-highlight-last-match)
                                            (message "Could not find source for %s" class-name)
                                            ))
                                        :class class-name))))


(defun jde-usages-get-subs-implementing-method (sig)
  "Return the subclasses implementing the method signature SIG."
  (let* ((class-name (nth 1 sig))
         (method-name (nth 2 sig))
         (args (nth 4 sig))
         (args-str (cond ;; the sig could have a number instead f the args list
                    ((numberp args) args)
                    (args (list (concat "new String [] {\"" (mapconcat 'identity  args "\", \"") "\"}")))
                    (t (list "new String [] {}"))))
         (bsh-eval-timeout 100)
         )
    (jde-usages-jeval-r 'jde.util.Usages.findSubs class-name method-name args-str 'System.out)))

(defun jde-usages-display-subs-implementing-method (&optional sig)
  "Display the subclasses implementing the method signature SIG."
  (interactive)
  (labels ((jde-usages-dsim-notify (widget child &optional event)
             (jde-usages-goto-sig (widget-get widget :class) (widget-get widget :sig)))
  
           (jde-usages-dsim-create-widget-tree (subclasses sig)
             (lexical-let ((sig sig))
             (cond
              ((atom subclasses) ;; "class"
               (let* ((name-and-disp (jde-usages-get-class-name-and-disp-name subclasses))
                      (class-name (car name-and-disp))
                      (disp-name (cdr name-and-disp)))
                 (list 'item :tag disp-name :class class-name))
               ;; (error "Should not happen")
               )
              ((and (eq (cdr subclasses) nil) (atom (car subclasses))) ;; ("class")
               (lexical-let* ((name-and-disp (jde-usages-get-class-name-and-disp-name (car subclasses)))
                      (class-name (car name-and-disp))
                      (disp-name (cdr name-and-disp)))
                 (jde-usages-make-usages-tree-text disp-name
                                                   (jde-usages-goto-sig class-name sig)
                                                   :class class-name)))

              ((atom (car subclasses)) ;; ("class" ....)
               (let* ((name-and-disp (jde-usages-get-class-name-and-disp-name (car subclasses)))
                      (class-name (car name-and-disp))
                      (disp-name (cdr name-and-disp))
                      (rest (cdr subclasses)))
                 (append (list
                          'tree-widget
                          :tag disp-name
                          :class class-name
                          :open t
                          :has-children t)
                         (mapcar (lambda (sub) (jde-usages-dsim-create-widget-tree sub sig))   rest)))
               )
              ((atom (caar subclasses)) ;; (("class") ... )
               (lexical-let* ((name-and-disp (jde-usages-get-class-name-and-disp-name (caar subclasses)))
                      (class-name (car name-and-disp))
                      (disp-name (cdr name-and-disp))
                      (rest (cdr subclasses)))
                 (jde-usages-make-usages-tree-text disp-name
                                                   (jde-usages-goto-sig class-name sig)
                                                   :open t
                                                   :has-children t
                                                   :class class-name
                                                   (mapcar (lambda (sub) (jde-usages-dsim-create-widget-tree sub sig))   rest))))))))

    (let* ((sig (or sig (jde-usages-get-current-signature)))
           (buf-name (concat "Subclasses implementing " (jde-usages-signature-to-string sig)))
           (buf (progn
                  (if  (get-buffer buf-name)  (kill-buffer buf-name))
                  (get-buffer-create buf-name)))
           (subclasses (jde-usages-get-subs-implementing-method sig))
           )
      (if (eq subclasses nil)
          (message "Method %s has no reimplementations" (jde-usages-signature-to-string sig))
        ;; else
        (jde-usages-switch-to-buffer buf)
        (erase-buffer)
        (apply 'widget-create
               (jde-usages-dsim-create-widget-tree subclasses sig))
      
        (use-local-map widget-keymap)
        (widget-setup)
        (toggle-read-only)
        (goto-char (point-min))
        (jde-usages-goto-first-pos)
        (if jde-usages-window-height
            (jde-usages-set-window-height (get-buffer-window buf) jde-usages-window-height))
        ))))

(provide 'jde-usages-class-trees)
;;; jde-usages-class-trees.el ends here
