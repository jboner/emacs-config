(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/misc"))

(load-file "$EMACS_LIB/load-directory.el")

(mapcar 'load-directory
        '("$EMACS_LIB/startup"))

(switch-to-buffer "*scratch*")
(delete-other-windows)
;(create-cache-window)

(message "Emacs is ready")
 