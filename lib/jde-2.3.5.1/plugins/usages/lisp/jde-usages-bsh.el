;;; jde-usages-bsh.el --- convenience functions top of jde-jeval and jde-jeval-r

;;; Commentary:
;; convenience functions top of jde-jeval and jde-jeval-r which let
;; you make calls like
;;    (jde-usages-jeval-r 'jde.util.Usages.findUsages class-name method-name args-str is-field-str strict-str 'System.out)
;;  instead of
;;    (jde-jeval-r (concat "jde.util.Usages.findUsages (\"" class-name "\" ,\"" method-name "\", " args-str "," is-field-str "," strict-str ", System.out);"))


;;; History:
;; 

(require 'beanshell)

;;; Code:
(defun jde-usages-jeval (method &rest args)
  "Call `jde-jeval' on the java expression METHOD (ARGS)."
  (jde-jeval (apply 'jde-usages-jeval-make-string method args)))

(defun jde-usages-jeval-r (method &rest args)
  "Call `jde-jeval-r' on the java expression METHOD (ARGS)."
  (jde-jeval-r (apply 'jde-usages-jeval-make-string method args)))


(defun jde-usages-jeval-make-string (method &rest args)
  "Make command string to pass to beanshell to invoke java METHOD with parameters ARGS."
  (labels ((to-jeval-string (arg)
             (cond
              ((stringp arg) (concat "\"" arg "\"")) ;; add quotes around strings
              ((symbolp arg) (symbol-name arg))  ;; a symbol name is just converted to a string, eg 'System.out
              ((numberp arg) (number-to-string arg))  ;; a number is made a string so concat can accept it
              ;; a list with a single string element is a complex java expression
              ((and (listp arg) (= 1 (length arg)) (stringp (car arg)))
               (car arg)) ;; extract the expression
              (t (error "Jde-usages-jeval:Don't know how to translate %S" arg))
              )))
    (when (jde-bsh-running-p)
      (jde-jeval (jde-create-prj-values-str)))
    (let ((eval-string (concat (symbol-name method)
                               "("
                               (mapconcat 'to-jeval-string
                                          args ", ") ");")))
;;      (message "%s %S --> %s" method args eval-string)
      eval-string)))

(provide 'jde-usages-bsh)
;;; jde-usages-bsh.el ends here
