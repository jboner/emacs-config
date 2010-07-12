;;; --------------------------------
;;; These are Common-Lisp extensions required by VoiceCode
(require 'cl)

 ;;; This enables pc-selection-mode which makes Emacs behave 
 ;;; a PC w.r.t. to text selection. This is necessary for 
 ;;; VoiceCode to work.
 (pc-selection-mode)

 ;;; This is the `VCode-Mode` file itself
 (load-file (substitute-in-file-name "$VCODE_HOME/Environments/Emacs/vcode-mode.el"))

 ;;; VoiceCode needs Emacs to put python buffers in python-mode. 
 (load-file (substitute-in-file-name "$VCODE_HOME/Environments/Emacs/python-mode.el"))
  (setq auto-mode-alist
        (cons '("\\.py$" . python-mode) auto-mode-alist))
  (setq interpreter-mode-alist
        (cons '("python" . python-mode)
              interpreter-mode-alist))
  (autoload 'python-mode "python-mode" "Python editing mode." t)

;;; --------------------------------
;;; VR-mode
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/vrmode_r011"))
(autoload 'vr-mode "$EMACS_LIB/lib/vrmode_r011/vr" "" t nil)
(setq vr-command "c:/home/jboner/emacs-config/lib/vrmode_r011/vr.exe")

;(setq vr-win-class "Emacs")
;(setq vr-win-title "Emacs")

(setq-default abbrev-mode t)
(load "abbrev-cmds")

