;;; ---------------------------------------------
;;; Clojure
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/slime"))
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/clojure/clojure-mode"))
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/clojure/swank-clojure))
(load-file (substitute-in-file-name "$EMACS_LIB/lib/misc/parenface.el"))

(require 'parenface)

(require 'slime) 
(slime-setup) 

;(setq swank-clojure-binary "/Users/jboner/src/clojure/clojure-extra/sh-script/clojure") 
(setq swank-clojure-binary "clojure")

(require 'clojure-auto)
(require 'swank-clojure-autoload)

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

