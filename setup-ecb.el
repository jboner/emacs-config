;;; ECB AND SEMANTIC ------------------------------------------
;; Load CEDET
(load-file (substitute-in-file-name "$EMACS_LIB/lib/cedet-1.0pre3/common/cedet.el"))

;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; Select one of the following
;; (semantic-load-enable-code-helpers)
;; (semantic-load-enable-guady-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)

;; Enable this if you develop in semantic, or develop grammars
;; (semantic-load-enable-semantic-debugging-helpers)

;; ECB
(add-to-list 'load-path  (substitute-in-file-name "$EMACS_LIB/lib/ecb-2.32"))
(require 'ecb) 

(custom-set-variables
; '(ecb-options-version "2.32")
 '(ecb-source-path (quote ("~/src")))
 '(ecb-tree-indent 1)
 '(global-semantic-decoration-mode t nil (semantic-decorate-mode))
 '(global-semantic-highlight-edits-mode t nil (semantic-util-modes))
 '(global-semantic-idle-completions-mode t nil (semantic-idle))
 '(global-semantic-idle-scheduler-mode t nil (semantic-idle))
 '(global-semantic-idle-summary-mode t nil (semantic-idle))
 '(global-semantic-show-parser-state-mode nil nil (semantic-util-modes))
 '(global-semantic-show-unmatched-syntax-mode t nil (semantic-util-modes))
 '(global-semantic-stickyfunc-mode t nil (semantic-util-modes))
 '(global-senator-minor-mode t nil (senator))
 '(py-indent-offset 3)
; '(semanticdb-global-mode t nil (semanticdb))
 '(which-function-mode t nil (which-func)))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )
;(ecb-activate)

