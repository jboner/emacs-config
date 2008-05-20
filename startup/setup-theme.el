;;; COLOR SCHEME AND FONTS ------------------------
;(set-default-font "-outline-ProFont-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
(set-default-font "-outline-ProggySquareTT-normal-r-normal-normal-16-97-96-96-c-*-iso8859-1")
;(set-default-font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")

;--------------------
;; Use font Monaco 10 for Mac OS X -- 
;(create-fontset-from-fontset-spec 
; (concat
;  "-apple-monaco-medium-r-normal--12-*-*-*-*-*-fontset-monaco,"
;  "ascii:-apple-monaco-medium-r-normal--12-100-*-*-m-100-mac-roman,"
;  "latin-iso8859-1:-apple-monaco-medium-r-normal--12-100-*-*-m-100-mac-roman"))
; Use fontset-monaco in all frames
;(setq initial-frame-alist `((width . 170) (height . 50) 
;                            (font . "fontset-monaco")))
;(setq default-frame-alist initial-frame-alist)

;--------------------
;; Color theme
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/color-theme-6.6.0"))
(require 'color-theme)
(color-theme-initialize)
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/misc"))
(require 'theme-manager)


