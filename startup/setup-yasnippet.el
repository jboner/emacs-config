;; yasnippet setup

(load-file "$EMACS_LIB/lib/yasnippet/yasnippet.el")
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/yasnippet"))

(require 'yasnippet)
(yas/initialize)
;(yas/load-directory "$EMACS_LIB/lib/yasnippet/snippets")
(yas/load-directory "C:/home/jboner/workspace/config/emacs-lib/lib/yasnippet/snippets")

