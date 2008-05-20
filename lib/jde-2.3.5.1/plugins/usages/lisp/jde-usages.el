;;; jde-usages.el --- JDEE plugin which provides usage information on java methods and fields
;; $Id: jde-usages.el,v 1.43 2006/03/15 22:49:51 surajacharya Exp $


;;; Commentary:
;; 


;;; History:
;; 

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar jde-usages-version "0.9.1"
  "Jde usages plugin version.")

(if (locate-library "reveal")
    (require 'reveal))

(require 'jde-usages-bsh)
(require 'jde-usages-redefines)
(require 'jde-usages-eval-type-of)

(require 'jde-usages-call-trees)
(require 'jde-usages-class-trees)
(require 'jde-usages-class-completion)

(require 'jde-usages-ecb)

(defun jde-usages-locate-class (&optional class-name)
  "Find the jar or directory that CLASS-NAME is located in."
  (interactive)
  (if (not class-name)
      (setq class-name (jde-usages-read-class-with-completion "Class or interface:" nil (list (or class-name (jde-usages-get-current-class))))))
  (jde-usages-jeval-r  'getClassLocation class-name))

;; all done, now register plugin
(jde-pi-register
 (jde-plugin
  "usages"
  :bsh-cp (list
           (expand-file-name "classes" (jde-pi-get-plugin-dir "usages"))
           (expand-file-name "lib/usages.jar" (jde-pi-get-plugin-dir "usages"))
           )
  :menu-spec (list
              (list
               "jde-usages"
               (list "Usages/Callers for"
                      ["Method or field definition at point" jde-usages-display-call-tree :active (member (semantic-tag-class (semantic-current-nonterminal)) '(function variable))]
                      ["Method call or field reference at point" jde-usages-display-call-tree-for-thing-at-point :active t]
                      ["Method or field specified interactively" jde-usages-display-call-tree-for-specified-class :active t])
                (list "Inheritance trees"
                      ["Type hierarchy" jde-usages-display-type-hierarchy :active t]
                      ["Subtypes" jde-usages-display-subs-and-implementers :active t]
                      ["Supertypes" jde-usages-display-superclasses :active t])
                (list "Misc"
                      ["Classes overriding/implementing this method" jde-usages-display-subs-implementing-method :active (member (semantic-tag-class (semantic-current-nonterminal)) '(function variable))]
                      ["Open class (with name completion)" jde-open-class-source-with-completion :active t]
                      ["Locate class"  jde-usages-locate-class :active t]
                      ["Goto defn"  jde-usages-goto-definition-of-thing-at-point :active t]
                      )
                (list "Options"
                      ["Load ECB integration" jde-usages-load-ecb-integration :active t]
                      ["\"Motion-sensitive\" buffers (jde-usages-buffers-are-motion-sensitive)"  
                       (jde-usages-set-motion-sensitive-flag 'jde-usages-buffers-are-motion-sensitive (not jde-usages-buffers-are-motion-sensitive))
                       :style toggle
                       :selected jde-usages-buffers-are-motion-sensitive]
                      ["Use highlighting (jde-usages-highlight)" (set-default 'jde-usages-highlight (not jde-usages-highlight)):style toggle :selected jde-usages-highlight]
                      )
                ["Home-page" (browse-url "http://jde-usages.sourceforge.net")]
               )
              )))


(provide 'jde-usages)
;;; jde-usages.el ends here
