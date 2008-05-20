(defun jde-usages-test-build-done-function (bug msg) 
  (setq jde-usages-test-build-done t))


(defun tests-setup ()
  (let* (
         (lisp-src-dir (file-name-directory (symbol-file 'tests-setup)))
         (java-src-dir (concat lisp-src-dir "../../src/testproject/"))
        )
    (rename-file (concat java-src-dir "Main.java")        (concat lisp-src-dir "Main.java") t)
    (copy-file   (concat lisp-src-dir "NewMain.java") (concat java-src-dir "Main.java") t)
    (copy-file   (concat lisp-src-dir "AClass.java")  java-src-dir t)
    (rename-file (concat java-src-dir "A.java")        (concat lisp-src-dir "A.java") t)

    (etest-find-file-revert (concat (file-name-directory (symbol-file 'tests-setup)) "../../src/testproject/Main.java"))
    (jde-load-project-file)
   
    ;; build the project
    (setq jde-usages-test-build-done nil)
    (add-hook 'jde-ant-build-hook 'jde-usages-test-build-done-function)
    (jde-ant-build (jde-ant-interactive-get-buildfile) "clean-classes build")
    (while (not jde-usages-test-build-done)
      (sit-for 1)
      )
    (remove-hook 'jde-ant-build-hook 'jde-usages-test-build-done-function)
    (sleep-for 10)
    ;; return build status
    (equal jde-ant-build-status "0")))


(defun TEST-usages-after-adding-deleting-classes (test-function)
  (etest-run-tests-in-group '(TEST-usages TEST-subs-implementing-method TEST-class-list TEST-subs) test-function)
  )


(defun TEST-usages-after-changing-classpath (test-function)
  (jde-find-class-source "testproject.Main")
  (let ((saved-cp jde-global-classpath))
    (setq jde-global-classpath (list "./classes/"))
    (jde-save-project)
    (etest-run-tests-in-group '(TEST-usages TEST-subs-implementing-method TEST-class-list TEST-subs) test-function)
    (setq jde-global-classpath saved-cp)
    (jde-save-project)
    ))


(defun tests-teardown ()
  (let* (
         (lisp-src-dir (file-name-directory (symbol-file 'tests-setup)))
         (java-src-dir (concat lisp-src-dir "../../src/testproject/"))
        )
    (delete-file (concat java-src-dir "Main.java"))
    (delete-file (concat java-src-dir "AClass.java"))
    (copy-file   (concat lisp-src-dir "A.java") java-src-dir t)  
    (copy-file   (concat lisp-src-dir "Main.java") java-src-dir t)

    (mapc (lambda (file) 
            (if (get-file-buffer file)
                (kill-buffer (get-file-buffer file)))
            )
          (etest-directory-files-recursive "../../src/testproject" ".*\\.java$"))
    t
  ))



;; (etest-run-tests-in-file  (symbol-file 'TEST-all) 'etest-compare-test-function)