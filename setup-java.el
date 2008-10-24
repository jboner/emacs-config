;;; JAVA STUFF ---------------------------------------------
;; JDE
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/jde-2.3.5.1/lisp"))
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/elib-1.0"))
(load-file (substitute-in-file-name "$EMACS_LIB/lib/cedet-1.0pre3/common/cedet.el"))
(require 'jde)

;;; ------------------------------------------------
;; Xrefactory
;(load "xrefactory")

;;; ------------------------------------------------
;; Flymake for Java 
;(require 'flymake)
 
;;; ------------------------------------------------
;;; for jde eclipse compiler server
;(setq compilation-error-regexp-alist
;  (cons '("----------\n[0-9]+. \\(ERROR\\|WARNING\\) in \\(.*\\)\n (at line \\([0-9]+\\))\n\\(\\(.*\n\\)+?\\).*^+\n\\(.*\n\\)" 2 3) ;compilation-error-regexp-alist))

;;; ------------------------------------------------
;(defun flymake-java-ecj-init ()
;  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;                       'jde-ecj-create-temp-file))
;         (local-file  (file-relative-name
;                       temp-file
;                       (file-name-directory buffer-file-name))))
    ;; Change your ecj.jar location here
;    (list "java" (list "-jar" "$EMACS_LIB/lib/ecj.jar" "-Xemacs" "-d" "c://tmp" 
;                       "-source" "1.5" "-target" "1.5" "-proceedOnError"
;                       "-sourcepath" (car jde-sourcepath) "-classpath" 
;                       (jde-build-classpath jde-global-classpath) local-file))))
;(defun flymake-java-ecj-cleanup ()
;  "Cleanup after `flymake-java-ecj-init' -- delete temp file and dirs."
;  (flymake-safe-delete-file flymake-temp-source-file-name)
;  (when flymake-temp-source-file-name
;    (flymake-safe-delete-directory (file-name-directory flymake-temp-source-file-name))))
 
;(defun jde-ecj-create-temp-file (file-name prefix)
;  "Create the file FILE-NAME in a unique directory in the temp directory."
;  (file-truename (expand-file-name (file-name-nondirectory file-name)
;                                   (expand-file-name  (int-to-string (random)) (flymake-get-temp-dir)))))
 
;(push '(".+\\.java$" flymake-java-ecj-init flymake-java-ecj-cleanup) flymake-allowed-file-name-masks)
 
;(push '("\\(.*?\\):\\([0-9]+\\): error: \\(.*?\\)\n" 1 2 nil 2 3 (6 compilation-error-face)) compilation-error-regexp-alist)
;(push '("\\(.*?\\):\\([0-9]+\\): warning: \\(.*?\\)\n" 1 2 nil 1 3 (6 compilation-warning-face)) compilation-error-regexp-alist)

