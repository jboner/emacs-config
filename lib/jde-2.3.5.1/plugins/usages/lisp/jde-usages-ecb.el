;;; jde-usages-ecb.el --- ecb mode integration

;;; Commentary:
;; This module adds a jde-usages submenu to the ECB methods buffer
;; right-click menu with commands to display method callers, method
;; overriders, type hierarchy, subclasses and superclasses. Right now
;; the menu is not context sentive - all the commands are available
;; irrespective of whether you right-clickec on a method or class
;; name. The class tree commands work on the class containing the
;; method clicked on, while the method (usage and overriders) commands
;; will just report nothing found if applied on a class name.
;;
;; To use this module just say (require 'jde-usages-ecb) in your init
;; file anywhere after your (require 'jde) line.

(require 'jde-usages-call-trees)
(require 'jde-usages-class-trees)
(require 'jde-usages-class-completion)

;;; Code:
(defvar jde-usages-ecb-methods-menu-extension-name "jde-usages")

(defun jde-usages-ecb-make-methods-menu-extension ()
  "Return the usages menu for the ecb methods buffer."
  `(,jde-usages-ecb-methods-menu-extension-name
    (jde-usages-ecb-show-callers "Method callers")
    (jde-usages-ecb-show-callers "Method overriders")
    (jde-usages-ecb-show-type-hierarchy "Type hierarchy")
    (jde-usages-ecb-show-subclasses "Only subclasses")
    (jde-usages-ecb-show-superclasses "All superclasses")
    )
  )

(defun jde-usages-add-ecb-extensions ()
  "Add a jde-usages submenu to the ECB methods buffer menu."
  (interactive)
  (when (and
         (functionp 'ecb-activate)
         (not (assq jde-usages-ecb-methods-menu-extension-name
                    ecb-methods-menu-user-extension)))
      
    (tree-buffer-defpopup-command jde-usages-ecb-show-type-hierarchy
      "Show type hierarchy."
      (lexical-let ((node node))
       (jde-usages-display-type-hierarchy (jde-usages-get-class-around-tag (tree-node->data node)) t)))

    (tree-buffer-defpopup-command jde-usages-ecb-show-subclasses
      "Show subclasses."
      (lexical-let ((node node))
       (jde-usages-display-subs-and-implementers t t (jde-usages-get-class-around-tag (tree-node->data node)) t)))

    (tree-buffer-defpopup-command jde-usages-ecb-show-superclasses
      "Show superclasses."
      (lexical-let ((node node))
       (jde-usages-display-superclasses (jde-usages-get-class-around-tag (tree-node->data node)) t)))

    (tree-buffer-defpopup-command jde-usages-ecb-show-callers
      "Show callers."
      (lexical-let ((node node))
       (jde-usages-display-call-tree nil (lambda () (jde-usages-tag-to-signature (tree-node->data node))))))
    
    (tree-buffer-defpopup-command jde-usages-ecb-show-overrides
      "Show overrides."
      (lexical-let ((node node))
       (jde-usages-display-subs-implementing-method (jde-usages-tag-to-signature (tree-node->data node)))))

    (setq ecb-methods-menu-user-extension
          (cons (jde-usages-ecb-make-methods-menu-extension)
                ecb-methods-menu-user-extension))))

(defun jde-usages-load-ecb-integration ()
  "Make jde-usages easier to use with ECB.
For now this does the following things:
1) Adds a jde-usages submenu for the ECB methods buffer.
2) Adds the jde-usage buffer names to
   ecb-compilation-buffer-names so they appear in same location
   as a compilation buffer.
3) Sets jde-usages-window-height to nil so that the height of the
   jde-usages windows is only controlled by
   ecb-compile-window-height."
  (interactive)
  (require 'ecb)
  (jde-usages-add-ecb-extensions)
  (setq jde-usages-window-height nil)
  (dolist (buffer jde-usages-buffers)
    (unless (aget ecb-compilation-buffer-names buffer)
      (add-to-list 'ecb-compilation-buffer-names (list buffer)))))

;; (defun ecb-set-usages-buffer ()
;;   (let ((buffer-name 
;;          "*usages*"
;;          ;; "*type-hierarchy*"
;;          ;; "*subclasses*"
;;          ;; "*superclasses*"
;;          ))
;;   (ecb-with-dedicated-window
;;       buffer-name
;;       'ecb-set-example-buffer
;;     (switch-to-buffer (get-buffer-create buffer-name)))))

(provide 'jde-usages-ecb)
;;; jde-usages-ecb.el ends here
