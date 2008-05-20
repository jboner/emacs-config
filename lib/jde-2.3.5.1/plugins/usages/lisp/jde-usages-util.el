;;; jde-usages-util.el --- miscellaneous functions that are used in
;;; multiple modules and don't fit in any one place

;;; Commentary:
;; 


;;; History:
;; 

;;; Code:

;;; set the window height for usages windows
(defcustom jde-usages-window-height 12
  "The height of the usages and class hierarchy buffers.  Nil means leave the height unchanged."
  :group 'jde-usages :type 'integer
  )

(defun jde-usages-set-window-height (window height)
  "Set the height of WINDOW to HEIGHT."
;; adapted from compile.el
  (and height
       (eq (window-width window) (frame-width (window-frame window)))
       ;; If window is alone in its frame, aside from a minibuffer,
       ;; don't change its height.
       (not (eq window (frame-root-window (window-frame window))))
       ;; Stef said that doing the saves in this order is safer:
       (save-excursion
         (save-selected-window
           (select-window window)
           (enlarge-window (- height (window-height)))))))


;;; java "signature" related functions
(defsubst jde-usages-ends-with (str substr)
  "Return t if string STR ends with the string SUBSTR."
  (equal (substring str (- (length str) (length substr)))
         substr))

(defun jde-usages-class-and-token-to-signature (class tag)
"Convert a class name CLASS and semantic TAG to a jde-usages signature type."
  (let ((ttype  (semantic-tag-type tag))
        (tclass (semantic-tag-class tag))
        (tname  (semantic-tag-name tag))
        (template-specifiers   (apply 'append 
                                      (mapcar (lambda (tag) 
                                                (semantic-tag-get-attribute tag :template-specifier))  
                                              (semantic-find-tag-by-overlay (point))))))
    (list tclass
          class
          (if (semantic-tag-function-constructor-p tag)
              "<init>"
            tname)
          (when (eq tclass 'function)
            (if (or (not ttype) (equal ttype "void"))
                nil
              (jde-usages-get-unqualified-name ttype template-specifiers)))
          (cond
           ((eq tclass 'function) (mapcar 
                                   (lambda (arg)
                                     (jde-usages-get-unqualified-name 
                                      (semantic-tag-type arg)
                                      template-specifiers))
                                   (semantic-tag-function-arguments tag)))
           ((eq tclass 'variable) nil)
           (t (list (jde-parse-get-unqualified-name ttype)))))))

(defsubst jde-usages-get-unqualified-name (name template-specifiers)
  "Get the unqualified name of class NAME.
If NAME is a template specifer return \"Object\"."
  (setq name (replace-regexp-in-string "<.*>" "" name))
  (if (member name template-specifiers)
      "Object"
    (jde-parse-get-unqualified-name name)))


(defun jde-usages-get-current-class ()
  "Return the innermost class name around point."
  (let ((package-name  (semantic-tag-name (car (semantic-find-tags-by-class
                                                'package
                                                (semantic-something-to-tag-table (current-buffer))))))
        (class-name (jde-usages-get-class-at-point)))
    (if class-name
        (concat package-name (when  package-name ".") (replace-regexp-in-string "\\." "$" class-name)))))


(defun jde-usages-get-class-around-tag (&optional tag)
"Get the fully qualified name of the innermost class around TAG.
If TAG is not specified it finds the innermost class at point."
(let* ((all-tags (semantic-something-to-tag-table
                  (if tag (semantic-tag-buffer tag) (current-buffer))))
         (package-tag (car (semantic-find-tags-by-class 'package all-tags)))
         (package-name (if package-tag (semantic-tag-name package-tag)))
         (ret "")
         (type-tags (or (semantic-find-tag-by-overlay (semantic-tag-start tag) (semantic-tag-buffer tag))
                        (semantic-find-tags-by-class 'type all-tags)))
         )
    (dolist (tag type-tags ret)
      (if (eq 'type (semantic-tag-class tag))
          (setq ret (if (equal ret "") (semantic-tag-name tag)
                      (concat ret "$" (semantic-tag-name tag))))))
    (concat package-name (when (and package-name ret) ".") ret)))


(defun jde-usages-tag-to-signature (tag)
  "Convert TAG into a signature."
  (jde-usages-class-and-token-to-signature (jde-usages-get-class-around-tag tag) tag))

(defun jde-usages-get-current-signature ()
  "Reurn the signature at point."
  (let ((nt (semantic-current-tag)))
    (unless (member (semantic-tag-class nt) '(function variable))
      (error "The cursor must be in a function or class variable to get the callers"))
    (jde-usages-class-and-token-to-signature   (jde-usages-get-current-class) nt   )))



;;; 'motion-sensitive' buffers
(defvar jde-usages-motion-senstive-timer nil
  "Stores the idle timer object which opens the source buffers
  for when `jde-usages-buffers-are-motion-sensitive' is on.")

;; install the idle timer to browse to the class source in the other
;; window if point is in one of the usages buffers


(defcustom jde-usages-idle-timer-interval 0.4
  "The time for which Emacs should be idle before attempting to
  browse to the source at point if point is in one of the usages
  buffer."
  :group 'jde-usages
  :type 'float)

(defun jde-usages-set-motion-sensitive-flag (symbol value)
  "Set the value of `jde-usages-buffers-are-motion-sensitive'.
Also sets or cancels the idle timer which does this action accordingly."
  (if (and (not value) jde-usages-motion-senstive-timer)
      (cancel-timer jde-usages-motion-senstive-timer)
    (setq jde-usages-motion-senstive-timer 
          (run-with-idle-timer jde-usages-idle-timer-interval t 
                               'jde-usages-open-buffers-when-idle)))
  (custom-set-default symbol value))



(defcustom jde-usages-buffers-are-motion-sensitive t
  "If set to nil the tree widgets in the usages, type hierarchy
  and overridden methods buffers will require you press <enter>
  to take you to the call, type or method declaration that the
  widget represents.  If set to t, moving point to the widget
  automatically makes the corresponding line of source visible in
  the other window."  
  :group 'jde-usages 
  :type 'boolean 
  :set 'jde-usages-set-motion-sensitive-flag)


(defvar jde-usages-motion-sensitive-buffers-data nil)

(defmacro jde-usages-make-usages-tree-text (tag notify-expr &rest other-properties)
  "Create a tree or leaf node for the usage tree.
The widget created has a push button and if
`jde-usages-tree-widgets-use-push-buttons' is non-nil ,has text
with point-entered properties.
TAG - title text that goes on this tree node.
NOTIFY-EXPR - a single expression which is the body of the notify
function for this widget.
OTHER-PROPERTIES - a series of key and value objects which are
additional properties for this tree widget.
If OTHER-PROPERTIES has an odd number of elements then the last
element is a list of this tree node's children, if
  it is even this node is a leaf."
(let ((properties `(list
                    'push-button
                    :tag (propertize ,tag 'point-entered
                                      (lambda (x y)
                                        (if jde-usages-buffers-are-motion-sensitive
                                            (setq jde-usages-motion-sensitive-buffers-data (cons (current-buffer) (lambda () ,notify-expr))))))
                    :format "%[%t%]\n"
                    :notify (lambda (widget child &optional event) (let ((buff (current-buffer))) 
                                                                     (if (and ,notify-expr (get-buffer-window buff)) 
                                                                         (delete-window (get-buffer-window buff)))))))
      )
  (if (eq 0 (mod (length other-properties) 2))
      `(append ,properties (list ,@other-properties))
    ;; else
    (let* ((tail  (last other-properties 2))
           (last (cadr tail)))
      (setcdr tail nil)
      `(append (list 'tree-widget :node ,properties ,@other-properties) ,last)))))

(defun jde-usages-open-buffers-when-idle()
  (when (and jde-usages-motion-sensitive-buffers-data (eq (current-buffer) (car jde-usages-motion-sensitive-buffers-data)))
    (let ((inhibit-point-motion-hooks t))
      (condition-case e
          (apply (cdr jde-usages-motion-sensitive-buffers-data) nil)
        (error (message "jde-usages-open-buffers-when-idle:%S" e)))
      (if (fboundp 'reveal-post-command)
          (reveal-post-command))
      (pop-to-buffer (car jde-usages-motion-sensitive-buffers-data))))
  (setq jde-usages-motion-sensitive-buffers-data nil))

;;; other helper functions
(defvar jde-usages-buffers '("*usages*" "*type-hierarchy*" "*subclasses*" "*superclasses*"))

(defun jde-usages-switch-to-buffer (buf)
  "Switch to buffer BUF.
If the current buffer is a jde-usages buffer use the same window as it, otherwise use the other window."
  (when (not (eq (current-buffer) buf))
    (if (member (buffer-name) jde-usages-buffers)
        (switch-to-buffer buf)
      (switch-to-buffer-other-window buf))))


(defsubst jde-usages-get-simple-class-name (fully-qualified-class-name)
  "Return the simple class name (with no package or outer class) for FULLY-QUALIFIED-CLASS-NAME."
  (substring fully-qualified-class-name (string-match "[^.$]*$" fully-qualified-class-name)))

(defun jde-usages-goto-sig (class-name sig)
  "Try and position point at the method and CLASS-NAME specified by SIG.
This function uses `jde-usages-find-class-source to get
  to the correct buffer and 'jde-usages-dsim-goto-function-regexp to
  navigate to the appropriate line in the buffer."
  (let* ((member-type (nth 0 sig))
         (class-name (or class-name (nth 1 sig))) ;; 
         (method-name (nth 2 sig))
         (source-method-name (if (equal method-name "<init>") (jde-usages-get-simple-class-name class-name)  method-name))
         (return-type (or (nth 3 sig) "void"))
         (args (nth 4 sig)))

    (if (jde-usages-find-class-source class-name t)

     ;; use the semantic tag for this class to navigate to the method declaration
     (let ((tags (semantic-find-tags-by-name source-method-name (semantic-tag-type-members (semantic-current-tag-of-class 'type)))))
       (cond
        ;; if there is only one method in the class with the same name, go there
        ((and (eq 1 (length tags)) (eq (semantic-tag-class (car tags)) member-type))
         (jde-usages-goto-and-highlight (car tags)))
        ;; there's more than one method with this name, look for a
        ;; match the on the return type and argument types
        ((> (length tags) 1)
         (semantic-go-to-tag (car tags))
         (setq tags
               (semantic--find-tags-by-function
                (lambda (method)
                  (let ((method-args (semantic-tag-function-arguments method)))
                    (or
                     (and (numberp args) (eq args (length method-args)))
                     (and
                      (equal return-type (semantic-tag-type method))
                      (eq (length method-args) (length args))
                      (equal args (mapcar (lambda (x) (jde-parse-get-unqualified-name (semantic-tag-type x))) method-args))))))
                tags))
         (when (car tags)
           (jde-usages-goto-and-highlight (car tags)))
         (if (not (eq (length tags) 1))
             ;; TODO : args may be a number
             (message "Can't find method name %s with args %S in class %s" source-method-name args class-name)))
        ((jde-usages-ends-with class-name source-method-name)
         (message "Default constructor for %s" class-name))
        ;; didn't find any member with this name, warn
        (t (message "Can't find method name %s in class %s" source-method-name class-name))))
     
     ;; else
     (message "Could not find source file for %s" class-name))))

(defun jde-usages-goto-and-highlight (tag)
  (semantic-go-to-tag tag)
  (re-search-forward (semantic-tag-name tag))
  (jde-usages-momentarily-highlight-last-match))

(defun jde-usages-get-class-name-and-disp-name (class)
  "Convert the sexp representation of class name CLASS into a sing class name string and a displayable string."
(let*
      ((name (if (vectorp class) (elt class 0) class))
       (type (if (vectorp class) (elt class 1) t))
       (class-name (replace-regexp-in-string "/" "." name))
       (disp-name (case type
                    ('interface (concat class-name " (I)"))
                    ('abstract (concat class-name " (A)"))
                    (t class-name))))
    (cons class-name disp-name)))

(defun jde-usages-signature-to-string (sig)
  "Return a display string for this jde-usages signature type, SIG."
  (concat (or (nth 3 sig) "void") " " (cadr sig) "."
          (if (equal (nth 2 sig) "<init>")
              (jde-parse-get-unqualified-name (cadr sig))
            (nth 2 sig))
          (when (eq (car sig) 'function)
            (if (numberp (nth 4 sig))
                (concat "[" (number-to-string (nth 4 sig))"]")
                (concat "("
                        (mapconcat (lambda (x) x) (nth 4 sig) ",") ")")))))

(defmacro jde-usages-profile (&rest body) 
  "Display the time required to run BODY."
  `(let ((start-time (current-time)) 
         (ret ,@body)
         end-time)
     (setq end-time (current-time))
     (message "jde-usages-profile : %d" (+ (* (- (nth 1 end-time) (nth 1 start-time)) 1000)) (- (nth 2 end-time) (nth 2 start-time)))
    ret))


;; call highlighting. adapted from semantic-decorate.el
(defcustom jde-usages-highlight t
  "Momentarily highlight a tag or method call before moving to
  it. When `jde-usages-buffers-are-motion-sensitive' is on the
  highlighting happens in the source buffer while moving around
  in the usages buffer, which remains the current buffer."
  :group 'jde-usages
  :type 'boolean
  )

(defun jde-usages-highlight-overlay (ov &optional face)
  "Specify that overlay OV should be highlighted.
Optional FACE specifies the face to use."
  (semantic-overlay-put ov 'old-face
                        (cons (semantic-overlay-get ov 'face)
                              (semantic-overlay-get ov 'old-face)))
  (semantic-overlay-put ov 'face (or face 'highlight)))

(defun jde-usages-unhighlight-overlay (ov)
  "Unhighlight overlay OV, restoring it's previous face."
  (semantic-overlay-put ov 'face (car (semantic-overlay-get ov 'old-face)))
  (semantic-overlay-put ov 'old-face (cdr (semantic-overlay-get ov 'old-face)))
  )

(defun jde-usages-momentary-unhighlight-overlay (ov)
  "Unhighlight overlay OV, restoring it's previous face."
  (jde-usages-unhighlight-overlay ov)
  (remove-hook 'pre-command-hook
	       `(lambda () (jde-usages-momentary-unhighlight-overlay ',ov))))

(defun jde-usages-momentary-highlight-overlay (ov &optional face delete-when-done)
  "Highlight overlay OV, removing highlighting when the user hits a key.
Optional argument FACE is the face to use for highlighting.
If FACE is not specified, then `highlight' will be used."
  (jde-usages-highlight-overlay ov face)
  (add-hook 'pre-command-hook
	    `(lambda () 
               (jde-usages-momentary-unhighlight-overlay ',ov)
               (if ',delete-when-done
                   (semantic-overlay-delete ',ov))
               )))

(defun jde-usages-momentarily-highlight-last-match (&optional b e)
"Highlight the last matched regular expression until the user presses a key.
If the optional parameters B and E are specified they are taken
as the beginning and end of the region to highlight."
(if jde-usages-highlight
    (let ((ov (semantic-make-overlay (or b (match-beginning 0)) (or e (match-end 0)))))
      (jde-usages-momentary-highlight-overlay ov nil t))))
                  

(defun jde-usages-momentarily-highlight-tag (tag)
  "Call `semantic-momentary-highlight-tag' if the
  `jde-usages-highlight' flag is on."
  (if (and tag jde-usages-highlight)
      (semantic-momentary-highlight-tag tag)))



;;; *usages* buffer navigation
(defun jde-usages-next-pos (&optional stay-in-usages-buffer backward)
  "Navigate to the next source-code position listed in the current jde-usages buffer.
If the current buffer is not a jde-usages buffer then this command uses the *usages* buffer."
  (interactive)
  (let ((usages (if (member (buffer-name) jde-usages-buffers) 
                    (current-buffer)
                  (get-buffer "*usages*"))))
    (if (not usages)
        (message "No *usages* buffer.")
      (unless (eq (current-buffer) usages)
        (jde-usages-switch-to-buffer usages))
      (jde-usages-find-next-usage 'jde-usages-goto-first-pos backward)
      (unless stay-in-usages-buffer
        (widget-button-press (point))))))

(defun jde-usages-goto-first-pos (&optional backward)
  "Navigate to the *first* source-code position listed in a usage or type hierarchy buffer."
  (if backward
      (goto-char (point-max))
    (progn ; for clarity
      (goto-char (point-min))
      (if (equal (buffer-name) "*usages*")
          (widget-forward 1))))
  (jde-usages-find-next-usage (lambda (backward) (error "No usages found")) backward))

(defun jde-usages-find-next-usage (bob &optional backward)
  "Go the next push-button widget in the tree-widget in the current buffer.
Argument BOB a function that is to be called if we reach the end of this buffer."
  (condition-case err
      (progn
        (widget-forward (if backward -1 1))
        (let ((start (point)))
          (while (not (eq (car (widget-at)) 'push-button))
            (widget-forward (if backward -1 1))
            (if (eq start (point))
              (error "No usages found")))))
    (beginning-of-buffer
     (goto-char (point-min))
     (funcall bob))
    (error
     (message "Error:%S" err))))

(defun jde-usages-buffers-next-pos ()
  "Move to next push button widget in the current jde-usages buffer."
  (interactive)
  (jde-usages-next-pos t nil))

(defun jde-usages-buffers-prev-pos ()
  "Move to previous push button widget in the current jde-usages buffer."
  (interactive)
  (jde-usages-next-pos t t))




(defun jde-usages-erase-usages-buffer ()
  "Erase the current jde-usages buffer by deleting the widget tree in it."
  (goto-char (point-min))
  (when (> (point-max) (point-min))
     (widget-forward 1)
     (widget-delete (widget-get (widget-at (point)) :parent))))

;; (defvar jde-usages-string-or-comment-faces
;; (cons 'font-lock-string-face jde-java-font-lock-comment-faces)
;; "A list of faces that font-lock uses for comments or strings")

;; adapted from jde-java-font-lock-at-comment
(defmacro jde-usages-at-comment-or-string (pos)
  "Return non-nil if POS is in a comment or string."
  `(let ((face (get-text-property ,pos 'face)))
     (or (eq face 'font-lock-string-face)
         (memq face
               jde-java-font-lock-comment-faces))))

;; from jde-find
(defun jde-usages-find-class-source (class &optional other-window)
  "*Find the source file for a specified CLASS.
Calls `jde-find-class-source-file' to do the search.
If it finds the source file, it opens the file in a buffer.
It also tries to go the begining of the class definition.
Optional argument OTHER-WINDOW If non-nil opens CLASS in the other window."
  (let ((source (jde-find-class-source-file class)))

    (when source
      (if (not (string-equal (buffer-file-name)  source))
	  (if other-window
	      (find-file-other-window source)
	    (find-file source)))
      
      (if (fboundp 'senator-re-search-forward)

          (let*
              ((class-name (car (last (split-string class "\\."))))
               (classes (split-string class-name "\\$"))
               (tags (semantic-find-tags-by-class 'type (semantic-something-to-tag-table (current-buffer))))
               tag
               )
            
            (dolist (c classes) 
                    (if (> (string-to-number c) 0)
                        ;; inner class
                        (progn 
                          (loop for var from 1 to (string-to-number c) by 1
                                do
                                (re-search-forward "\\Wnew[ \t\n]+\\(\\w\\|\\.\\)+[ \t\n]*([ \t\n]*)[ \t\n]*{")
                                finally
                                (backward-char)
                                )
                          )
                      ;; else
                      (setq tag (car (semantic-find-tags-by-name c tags)))
                      (if tag
                          (semantic-go-to-tag tag))
                      (setq tags (semantic-find-tags-by-class 'type (semantic-tag-type-members tag))))
                    )
            ))
        t ;; return non-nil, we could find the source file
      )))

;;; unit tests
(defvar tests-that-are-new)
(defvar tests-passed)
(defvar tests-failed)

(defun jde-usages-run-tests ()
  "Run the test suite for jde-usages."
  (interactive)
  (load "../tests/lisp/jde-usages-etest.el")
  (setq tests-passed nil)
  (setq tests-failed nil)
  (setq tests-that-are-new nil)
  (cd (file-name-directory (find-library-name (symbol-file 'etest-run-test))))
;;  (load (expand-file-name  "./tests/testUsages.el"))
  (if (jde-usages-profile
;;       (etest-run-tests-in-file "tests/testUsagesAfterClasspathChanges.el" 'etest-compare-test-result-acceptor))
       (etest-run-tests-in-directory "tests" 'etest-compare-test-result-acceptor))
      (message "All tests passed"))
  (switch-to-buffer "*test-results*")
  (goto-char (point-max))
  (insert "Tests Passed:")
  (cl-prettyprint tests-passed)
  (insert "\n\nTests Failed:")
  (cl-prettyprint tests-failed)
  (insert "\n\nNew tests")
  (cl-prettyprint tests-that-are-new)
  (if (and tests-that-are-new
           (y-or-n-p "Found new tests.  Save these test results? "))
      (etest-update-test-results tests-that-are-new)
    )
  )


(defun jde-usages-get-class-at-point ()
 (mapconcat 'semantic-tag-name (semantic-find-tags-by-class 'type (semantic-find-tag-by-overlay (point))) "."))


(defun jde-usages-goto-definition-of-thing-at-point () 
  (interactive)
  (let* ((sig (jde-usages-get-signature-of-thing-at-point))
         (class-name (jde-usages-get-class-which-defines-sig sig)))
    (if class-name
        (jde-usages-goto-sig class-name sig)
      (jde-open-class-at-point))))

(provide 'jde-usages-util)
;;; jde-usages-util.el ends here
