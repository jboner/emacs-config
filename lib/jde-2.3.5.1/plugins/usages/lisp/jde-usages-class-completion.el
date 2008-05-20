;;; jde-usages-class-completion.el --- jde-open-class-source-with-completion and related functions

;;; Commentary:
;; 


;;; History:
;; 

(require 'jde-usages-bsh)

;;; Code:

;; method/field completion using jde-usages
(defun jde-usages-complete-invoke-get-class-info (name access)
  "Invoke the method jde.util.Usages.getClassInfo(String, int)"
  (jde-jeval-r
   (format "jde.util.Usages.getClassInfo(\"%s\",%d);" name access)))

;; (defalias 'jde-complete-invoke-get-class-info 'jde-usages-complete-invoke-get-class-info)

;; Classname completion stuff
(defvar jde-usages-known-classes (make-hash-table :test 'equal)
  "A hashtable storing the list of classes for each project.
The class names in the list are in a form that make for easier
completion when typing in the name.  The class full.package.Class
is listed as \"Class|full.package\"."  )



(defvar jde-usages-old-completing-read
  (not (string-match "table can be an list of strings" (documentation 'completing-read))))

;; class completion
(defun jde-usages-get-all-known-classes ()
  "Return a list of classes in the current project."
  (let ((classes (gethash jde-current-project jde-usages-known-classes)))
    (when (jde-bsh-running-p)
      (jde-jeval (jde-create-prj-values-str)))
    (let ((need-refresh (jde-jeval "jde.util.Usages.getAllClasses (null);" t)))
      (when (or need-refresh (not classes))
        (setq classes     (sort (let ((classes-file (replace-regexp-in-string  "\\\\" "/" 
                                                                               (if jde-xemacsp
                                                                                   (make-temp-name (expand-file-name "jde-usages" (temp-directory)))
                                                                                 (make-temp-file "jde-usages")))))
                                  (jde-jeval (concat "out = new java.io.PrintStream (new java.io.BufferedOutputStream (new java.io.FileOutputStream (new java.io.File (\""
                                                     classes-file "\"))));"))
                                  (jde-jeval "jde.util.Usages.getAllClasses (out);")
                                  (jde-jeval "out.close();")
                                  (with-temp-buffer
                                    (insert-file-contents-literally classes-file)
                                    (delete-file classes-file)
                                    (read (current-buffer)))) 'string<))
        (puthash jde-current-project classes jde-usages-known-classes)))
    (cond 
     ((and (stringp (car classes)) (eq (jde-usages-completing-read-function) 'completing-read) jde-usages-old-completing-read)
      (setq classes (mapcar (lambda (x) (list x)) classes)) ;; list of strings -> list of single string element lists
      (puthash jde-current-project classes jde-usages-known-classes))
     ((and (not (atom (car classes))) (eq (jde-usages-completing-read-function) 'ido-completing-read))
      (setq classes (mapcar (lambda (x) (car x)) classes)) ;; list of single string element lists -> list of strings 
      (puthash jde-current-project classes jde-usages-known-classes)))
    classes))

(defsubst jde-usages-completing-read-function  ()
    "Return the completing function used in jde-usages.
This is normally `completing-read' but returns `ido-completing-read' if ido mode is active."
  (if (and (boundp 'ido-mode) ido-mode) 'ido-completing-read 'completing-read))

(defun jde-usages-read-class-with-completion (&optional prompt exact-match initial-value)
"Read a class name from the minibuffer.
Provides completion with the list of classes from
'jde-usages-get-all-known-classes.  Returns a pair of classname
and fully qualified classname, if the user did not pick a class
from the completion list the cdr of the return value is nil
Optional argument EXACT-MATCH if non-nil, force the user to return a class that is in the all-known-users list for this project."
  (interactive)
  (let* ((initial-class-name (car initial-value))
         (munged-class-name (if (or (not initial-class-name) (string-match "[ |]" initial-class-name))
                                initial-class-name
                              (let ((last-classname-separator (string-match "\\$\\|\\.[^$.]*$" initial-class-name)))
                                (concat (substring initial-class-name (+ 1 last-classname-separator))
                                        " (in "
                                        (substring initial-class-name 0 last-classname-separator) ")"))))
         (ido-enable-prefix t) ;; turn on prefix matching by default, as this is usually more useful for class names. It can be turned off by hitting C-p
         )
  (jde-usages-get-fq-class-name-from-all-classes-list 
   (apply (jde-usages-completing-read-function) prompt (jde-usages-get-all-known-classes) nil exact-match (list munged-class-name))))
  )

(defun jde-open-class-source-with-completion ()
  "A wrapper around 'jde-open-class-source providing class name completion.
The list of classnames is obtained from the jde-usages java
component by calling jde.util.Usages.getAllClasses() which
returns all the classes in the 'jde-global-classpath for the
current project.
            
This command checks if any classes were added or removed since
the last call, and will update the list of classes accordingly."
  (interactive)
  (let* ((class-name (jde-usages-read-class-with-completion "Class:")))
    (if (or (string-match "\\." class-name) (jde-usages-parse-class-exists class-name))
        (jde-find-class-source class-name)
      (jde-open-class-source class-name))))

(defun jde-usages-get-fq-class-name-from-all-classes-list (display-name)
  "Convert a class name entry, REVERSED-CLASS-NAME,  in `jde-usages-known-classes' to a fully qualified class name."
  (if (string-match "\\(.*\\) (in \\(.*?\\)\\(\\$?\\))" display-name)
      (concat (match-string 2 display-name) 
              (if (= (match-beginning 3) (match-end 3)) 
                  "." 
                (match-string 3 display-name)) 
              (match-string 1 display-name))
    display-name))


(defun jde-usages-reload-class-list-for-project ()
  "Throw away the old list of classed for this project and get it again."
  (interactive)
  (puthash jde-current-project (jde-usages-get-all-known-classes) jde-usages-known-classes)
)

(provide 'jde-usages-class-completion)
;;; jde-usages-class-completion.el ends here
