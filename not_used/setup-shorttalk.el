;;; EMACSLISTEN  ------------------------------
(when (eq window-system 'w32)

  (add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/listen"))

  ;; ADJUST THIS PATH IF NECESSARY --->
  (setq listen-path "$EMACS_LIB/lib/listen")
  (setq-default load-path (cons listen-path load-path))

  ;; EmacsListen will turn it on anyway; best to get used to it
  (transient-mark-mode 1)

  ;; LOAD VR-MODE
  (load (concat listen-path "/vr-mode/vr"))

  ;; These commands below may not be desired if you are
  ;; already using vr-mode's voice commands

  (setq vr-activate-minibuffer nil)
  (setq vr-activation-list nil)
  (setq vr-voice-command-list nil)
  (setq vr-command (concat listen-path "/vr-mode/vr.exe"))

  ;;; LOAD EMACSLISTEN
  (require 'listen-mode)

  ;; Load speech engine specific interface, here Barry Jaspan's vr-mode
  ;; for ScanSoft NaturallySpeaking as modified to work with EmacsListen
  (require 'listen-vr-additions)

  ;; (Optional) Load listen-peek (display of yank, search histories,
  ;; and color coded excerpts of recent views of files)
;  (require 'listen-peek)
   ;; (Optional) Activate listen-peek mode
;  (listen-peek-mode 1)
  ;; (Optional) Load key and mouse definitions
  (load "listen-keys")

  ;; Recommended (make garbage collection occur less often)
  (setq gc-cons-threshold 10000000)
)

