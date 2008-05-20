;;; ------------------------------------------------
;;; tags-tree

(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/tree"))
(eval-after-load "tree-widget"
  '(if (boundp 'tree-widget-themes-load-path)
       (add-to-list 'tree-widget-themes-load-path "~/.emacs.d/")))

(autoload 'imenu-tree "imenu-tree" "Imenu tree" t)
(autoload 'tags-tree "tags-tree" "TAGS tree" t)

(require 'tree-mode)
(require 'imenu-tree)
(require 'tags-tree)
