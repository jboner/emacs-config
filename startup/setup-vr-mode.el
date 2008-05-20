;;; VR Mode
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/vrmode_r011"))
(autoload 'vr-mode "$EMACS_LIB/lib/vrmode_r011/vr.el" "" t) 
(setq vr-command "C://home/jboner/workspace/config/emacs-lib/lib/vrmode_r011/vr.exe")

;(setq vr-win-class nil)
;(setq vr-win-title nil)

;(setq vr-class nil)
;(setq vr-window nil)

;(setq vr-host "192.168.179.128")
;(setq vr-port 1234)
(vr-mode t)
