;;; lisp ------------------------------
(load-file (substitute-in-file-name "$EMACS_LIB/lib/misc/parenface.el"))
(load-file (substitute-in-file-name "$EMACS_LIB/lib/misc/clojure-mode.el"))
(require 'parenface)
(add-to-list 'auto-mode-alist '("\\.arc\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
