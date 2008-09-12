;;; COLOR SCHEME AND FONTS ------------------------
;(set-default-font "-outline-ProFont-normal-r-normal-normal-11-97-96-96-c-*-iso8859-1")
;(set-default-font "-outline-ProggySquareTT-normal-r-normal-normal-18-97-96-96-c-*-iso8859-1")
;(set-default-font "-outline-ProggyCleanTT-normal-r-normal-normal-16-97-96-96-c-*-iso8859-1")
;(set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
;(set-default-font "-raster-Fixedsys-normal-r-normal-normal-12-90-96-96-c-80-iso8859-1")

;--------------------
;; Color theme
;(load-file (substitute-in-file-name "$EMACS_LIB/lib/color-theme-6.6.0/themes/manoj-colors.el"))
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/color-theme-6.6.0"))
(require 'color-theme)
(color-theme-initialize)
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/misc"))
(require 'theme-manager)
;(color-theme-manoj-dark)
;(color-theme-manoj-font-lock)
;(color-theme-vim-colors)
(color-theme-scintilla)

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


