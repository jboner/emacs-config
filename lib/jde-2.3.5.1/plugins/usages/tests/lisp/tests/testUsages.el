;; tests 
(defun TEST-usages (test-function)
  (lexical-let ((test-function test-function))
    (testUsages-visit-tags-in-all-files 
     (lambda (file tag)   
       (let ((signature (jde-usages-get-current-signature)))
         (funcall test-function (list signature :strict) 
                  (lambda () (jde-usages-get-callers signature t)))
         (funcall test-function (list signature :notstrict) 
                  (lambda () (jde-usages-get-callers signature nil))))
       ))))

(defun TEST-subs-implementing-method (test-function) 
  (lexical-let ((test-function test-function))
    (testUsages-visit-tags-in-all-files
     (lambda (file tag) 
       (let ((sig (jde-usages-get-current-signature)))
         (funcall test-function (list sig)
                  (lambda ()
                   (jde-usages-get-subs-implementing-method sig))
                  )))
     '(function)
     )))

(defun TEST-usages-at-point (test-function)
  (jde-find-class-source "testproject.Main")
  (beginning-of-buffer)
  (senator-next-tag)
  (font-lock-fontify-buffer)
  (while (re-search-forward "\\." (point-max) t)
    (unless (in-string-p)
      (funcall test-function (list "testproject.Main" (point))
               (lambda () (jde-usages-get-callers (jde-usages-get-signature-of-thing-at-point)))))
    )
  )

(defun TEST-class-list (test-function)
  (jde-find-class-source "testproject.Main")
  (funcall test-function nil (lambda () (jde-usages-get-all-known-classes)))
  )

(defun TEST-subs (test-function)
  (jde-find-class-source "testproject.Main")
  (let ((classes (jde-usages-get-all-known-classes)))
    (dolist (thing classes)
      (let ((class (jde-usages-get-fq-class-name-from-all-classes-list  thing)))
        (funcall test-function (list 'subclasses class) (lambda () (jde-usages-get-subclasses t nil class)))
        (funcall test-function (list 'implementers class) (lambda () (jde-usages-get-subclasses nil t class)))
        (funcall test-function (list 'both class) (lambda () (jde-usages-get-subclasses t t class))))
      )))

(defun TEST-supers (test-function)
  "Test the jde-usages-get-superclasses functionality."
  (jde-find-class-source "testproject.Main")
  (let ((classes (jde-usages-get-all-known-classes)))
    (dolist (thing classes)
      (let ((class (jde-usages-get-fq-class-name-from-all-classes-list  thing)))
        (funcall test-function (list class) (lambda () (jde-usages-get-superclasses class)))
      ))))

(defun TEST-type-hierarchy (test-function)
  "Test the jde-usages-get-superclasses functionality."
  (jde-find-class-source "testproject.Main")
  (let ((classes (jde-usages-get-all-known-classes)))
    (dolist (thing classes)
      (let ((class (jde-usages-get-fq-class-name-from-all-classes-list  thing)))
        (funcall test-function (list class) (lambda () (jde-usages-get-type-hierarchy class)))
      ))))



;; setup /teardown 

(defun tests-setup ()
  (jde-bsh-exit)
    ;; load a file from the test project
  (etest-find-file-revert (concat (file-name-directory (symbol-file 'tests-setup)) "../../src/testproject/Main.java"))
  (jde-load-project-file)
  
  ;; build the project
  (setq jde-usages-test-build-done nil)
  (add-hook 'jde-ant-build-hook 'jde-usages-test-build-done-function)
  (jde-ant-build (jde-ant-interactive-get-buildfile) "clean jar")
  (while (not jde-usages-test-build-done)
;;     (accept-process-output (car compilation-in-progress) 1)
     (sit-for 1)
    )
;;    (let ((count 0))
;;      (while compilation-in-progress
;; ;;        (accept-process-output (car compilation-in-progress) 5 10)
;; ;;        (sleep-for 1)
;;        (sit-for 1 t)
;;        (incf count)
;;        (message "%S %S %S" count (get-buffer-process (current-buffer)) compilation-in-progress)
;;        ))
  (remove-hook 'jde-ant-build-hook 'jde-usages-test-build-done-function)
  ;; return build status
  (equal jde-ant-build-status "0")
  )

(defun jde-usages-test-build-done-function (bug msg) 
  (setq jde-usages-test-build-done t))

(defun tests-teardown ()
  ;;(jde-bsh-exit)
    (mapc (lambda (file) 
            (if (get-file-buffer file)
                (kill-buffer (get-file-buffer file)))
            )
          (etest-directory-files-recursive "../../src/testproject" ".*\\.java$"))
  t
  )


;; helper funtions 

(defun testUsages-visit-tags-in-all-files (visitor &optional tag-classes)
  (mapc (lambda (file) (testUsages-visit-tags file visitor tag-classes))
        (etest-directory-files-recursive "../../src/testproject" ".*\\.java$")))

(defun testUsages-visit-tags (file visitor &optional tag-classes)
  (let ((tag-classes (or tag-classes '(function variable))))
  (save-excursion
    (when (and (file-readable-p file) (not (file-directory-p file)))
      (etest-find-file-revert file)
      (beginning-of-buffer)
      (semantic-fetch-tags)
      ;; find all interesting tags in file and run 
      ;; (jde-usages-get-callers (jde-usages-get-current-signature) t) (jde-usages-get-callers (jde-usages-get-current-signature) nil)
      ;; at each point of interest
      (while (etest-semantic-next-tag-of-type tag-classes)
        (funcall visitor file (semantic-current-tag))
        )
      (etest-find-file-revert file)
      ))))


(defun etest-semantic-next-tag-of-type (types)
  "Finds the next tag in the current buffer whose tag class is listed in TYPES."
  (let ((tag (semantic-find-tag-by-overlay-next)))
    (when tag 
        (goto-char (semantic-tag-start tag))
        (if (not (member (semantic-tag-class tag) types))
            (etest-semantic-next-tag-of-type types)
        tag)
        )
    ))

(defun etest-directory-files-recursive  (dir pattern)
  "elisp equivalent of \"find DIR -name PATTERN\""
  (if (string-match "^.*/\\.+$" dir) ;; skip . and ..
      nil
    (if (file-directory-p dir)
        (mapcan (lambda (dir) (etest-directory-files-recursive  dir pattern))
                (directory-files dir t))
      (if (string-match pattern (file-name-nondirectory dir))
          (list dir)
        nil))))

