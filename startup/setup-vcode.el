;;; VOICE CODE ------------------------------
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
;(setq vr-deprecated-host ""192.168.179.128"")

(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
              interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

