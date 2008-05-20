;;; jde-usages-etest.el --- Simple unit test framework in elisp.

;;; Commentary:
;;; A test function takes a single function, the test result acceptor
;;; as parameter. 

;;; History:
;; 

;;; Code:
(defalias 'TESTS 'tests)
(defalias 'SETUP 'tests-setup)
(defalias 'TEARDOWN 'tests-teardown)

(defun etest-run-test (test test-result-acceptor)
"Run test function TEST.

TEST-RESULT-ACCEPTOR takes two parameters: KEY is a list the
first element of which is the function called to run this
test.  If this function is called multiple times the cdr of KEY
should make it unique.
RESULT is the result from running the test with id KEY."
  (lexical-let ((test-result-acceptor test-result-acceptor)
                (test test))
    (funcall test (lambda (key result) (funcall test-result-acceptor (cons test key) result)))))


(defun etest-run-tests-in-group (tests test-result-acceptor)
  "Run all the test functions in TESTS. Returns true if all the tests succeded, nil otherwise."
  (let ((final-result t) test-result)
    (while (and tests (listp tests))
      (setq test-result (etest-run-test (car tests) test-result-acceptor))
      (setq final-result (and final-result test-result))
      (setq tests (cdr tests)))
    final-result))


(defun etest-tests-in-file (file)
  "Load FILE and then return  a list of defuns defined in FILE."
  (load-file file)
  (mapcan (lambda (symbol)
            (cond
             ;; the input is a list of symbols
             ((and (symbolp symbol) (string-match "^TEST-" (symbol-name symbol))) (list symbol))
             ;; newer style load-history format with pairs insead of symbols
             ((and  (listp symbol) (eq (car symbol) 'defun) (string-match "^TEST-" (symbol-name (cdr symbol))))  (list (cdr symbol)))
             ))
               (assoc (expand-file-name file) load-history)))

(defun etest-run-tests-in-file (file test-result-acceptor)
  "Run all the tests in FILE.
Functions named tests-setup and tests-teardown are run before and after the tests respectively."
  ;; (message "run-tests-in-file %S" file)
  ;; remove any old setup or teardown definitions
  (defun tests-teardown () nil)
  (defun tests-setup () nil)
  ;; we need to call tests-in-file before running tests-setup, because
  ;; it loads the new funciton definitions in FILE
  (let ((tests (etest-tests-in-file file)) return)
    (unwind-protect
        (progn
          (tests-setup)
          (setq return (etest-run-tests-in-group tests test-result-acceptor)))
      (tests-teardown))
    return))


(defun etest-run-tests-in-directory (dir test-result-acceptor)
  "Runs all the tests in directory DIR.
Elisp files with names that start with 'test' are treated as test files.
Argument DIR"
  (lexical-let ((old-dir default-directory)
                  (test-result-acceptor test-result-acceptor)
                  results)
    (unwind-protect
        (let ((default-directory (expand-file-name dir)))
          (setq results
                (mapcar
                 (lambda (file) (etest-run-tests-in-file file test-result-acceptor))
                 (directory-files "." t "test.*.el$"))))
      ;; finally restore current-directory
      (setq default-directory old-dir))
    (not (memq nil results))))
   
;;;
;;; check test result against expected result
;;;

(defun etest-get-results-file (filename)
  "The file in which the test result for elisp file FILENAME are to be stored."
  (concat filename "-results"))


(defvar etest-test-results-hash (make-hash-table :test 'equal)
"The global mapping between elisp file names and the hash containing the results for the tests in the file.")

;;; Serialize a hash to disk, and read it back (stolen from jde-xref)
(defun etest-pickle-hash (hash filename)
  "Store HASH in the file FILENAME.
The hash can be retrieved by calling `etest-load-hash'."
  (save-excursion
    (let ((buf (find-file-noselect (jde-normalize-path filename))))
      (set-buffer buf)
      (goto-char (point-min))
      (erase-buffer)
      (maphash (lambda (key val)
                 (cl-prettyprint key)
                 (cl-prettyprint val)
                 (insert "\n\n"))
               hash)
      (save-buffer)
      ;; (kill-buffer buf)
      )))


(defun etest-unpickle-hash (hash filename)
  "Populate a hashtable HASH by loading the contents of FILENAME.
FILENAME must be created by `etest-pickle-hash'"
  (if (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents-literally filename)
        (condition-case err
            (while (< (point) (point-max))
              (puthash (read (current-buffer)) (read (current-buffer)) hash))
          (error t)))))

(defun etest-get-results-hash-for-function (function)
  "Get the hash with the expected results for test function FUNCTION.
This function reads the hash from disk if it is not present in `etest-test-results-hash'"
  (let* ((filename (symbol-file function))
         (hash (gethash filename etest-test-results-hash)))
    (unless hash
      (setq hash (make-hash-table  :test 'equal))
      (puthash filename hash etest-test-results-hash)
      (etest-unpickle-hash hash (etest-get-results-file filename)))
    hash))

(defun etest-get-expected-result-for-key (key)
"Get the expected result for the test with id KEY.
KEY is a list the first element of which is the function called
to run this test.  This is enforced in `etest-run-tests'.  IF this
function is called multiple times the cdr of KEY should make it
unique."
  (gethash key (etest-get-results-hash-for-function (car key)))
  )

(defun etest-set-result-for-key (key result)
  (puthash key result (etest-get-results-hash-for-function (car key)))
  )


(defun etest-no-result-for-key (key)
  (and
   (eq (gethash key (etest-get-results-hash-for-function (car key)) t) t)
   (eq (gethash key (etest-get-results-hash-for-function (car key)) nil) nil)
   ))


;; test-result-acceptor paramter to etest-run-test to compare actual test results against those stored in a file
(defun etest-compare-test-result-acceptor (key result)
  (let* ((expected-result (etest-get-expected-result-for-key key))
         (actual-result  (funcall result))
         (test-passed (equal actual-result expected-result))
         (new-test (etest-no-result-for-key key))
         )
    (message "Running test:%S" key)
    ;; record test results in tests-passed or tests-failed
    (if new-test
        (setq tests-that-are-new (cons (list key :test-output actual-result) tests-that-are-new))
      (if test-passed
          (setq tests-passed (cons (list key) tests-passed))
        (setq tests-failed (cons (list key :test-ouput actual-result :expected-ouput expected-result) tests-failed))))
    ;; return test result
    test-passed))

(defun etest-update-test-results (tests-to-be-updated)
  (let ((files-to-be-saved))
    (dolist (thing tests-to-be-updated)
      (let* ((key (car thing))
             (filename (symbol-file (car key))))
        (etest-set-result-for-key key (nth 2 thing))
        (unless (member filename files-to-be-saved)
          (setq files-to-be-saved (cons filename files-to-be-saved)))))
    (mapc (lambda (filename)
            (etest-pickle-hash   (gethash filename etest-test-results-hash) (etest-get-results-file filename)))
          files-to-be-saved)))


(defsubst etest-find-file-revert (file-name)
  "Kill the buffer visiting FILE-NAME if any and then open a new buffer visiting it.
This is basically to prevent getting any \"<file-name> has changed since it was last visited. Revert?\" prompts while running unit tests."
  (let ((buf (get-file-buffer file-name)))
    (if buf
        (kill-buffer buf))
    (find-file file-name)))



(provide 'jde-usages-etest)
;;; jde-usages-etest.el ends here
