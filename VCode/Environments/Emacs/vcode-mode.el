;;
;; Vcode Mode - integration of GNU Emacs and VoiceCode
;;    http://voicecode.iit.nrc.ca/VoiceCode
;;
;; Based on vr-deprecated Mode by Barry Jaspan
;;
;; Copyright 1999 Barry Jaspan, <bjaspan@mit>.  All rights reserved.
;;
;; $Id: vc.el,v 1.9 2002/08/05 22:59:58 alain_desilets Exp$ 
;;
;; This file is part of Emacs vr-deprecated Mode.
;;
;; Emacs vcode Mode is free software; you cltan redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; Emacs vr-deprecated Mode is distributed in the hope that it will be useful, butdeemof

;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs vr-deprecated Mode; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;;
;;; VCode assumes the following.
;;;
(require 'cl)
(pc-selection-mode)
(load-file (substitute-in-file-name "$VCODE_HOME/Environments/Emacs/python-mode.el"))
(custom-set-variables
 '(py-indent-offset 4))


(setq vr-deprecated-activation-list 
           (list "\.py$" "\.c$" "\.cpp$" "\.h$" "\.pl$" "\.R$" "\.hpp$" "\.java$"
                 "\.js$" "*Completions*" "\.php$" "\.scala$"))


;;; Change this if you want to see more traces
(setq message-log-max 5000)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-deprecated-command "vr-deprecated.exe" "*The \"vr-deprecated.exe\" program to be
invoked as the vr-deprecated mode sub-process.  This can be just the name, if the
program is in your PATH, or it can be a full path.")

(defvar vr-deprecated-host nil "*The name of the computer running the vr-deprecavted Mode
process.  If nil, the process will be started on this computer.  See
also vr-deprecated-port.")
(setq vr-deprecated-host ""127.0.0.1"")

(defvar vr-deprecated-port 0 "*The port on which to connect to vr-deprecated-host.  If
vr-deprecated-host is nil, this can be zero to tell the vr-deprecated Mode process to use
any available port.")
(setq vr-deprecated-port 45770)


(defvar vr-deprecated-win-class nil
  "*Class name of the Windows window for which vr-deprecated Mode will accept
voice input.  Whenever a window matching vr-deprecated-win-class and vr-deprecated-win-title
(which see) is the foreground window, dictation and commands spoken
into the microphone will be executed by vr-deprecated Mode.")
(defvar vr-deprecated-win-title "emacs"
  "*Title of the Windows window for which vr-deprecated Mode will accept voice
input.  Whenever a window matching vr-deprecated-win-class (which see) and
vr-deprecated-win-title is the foreground window, dictation and commands spoken
into the microphone will be executed by vr-deprecated Mode.")

(defvar vcode-previous-frame-title-format frame-title-format
  "*Store previous frame-title-format so we can restore it when vcode
  exits (or the mediator disconnects)"
)

(defvar vr-deprecated-activation-list nil
  "*A list of buffer name patterns which vr-deprecated Mode will voice activate.
Each element of the list is a REGEXP.  Any buffer whose name matches
any element of the list is voice activated.  For example, with

(setq vr-deprecated-activation-list '(\"^\\*scratch\\*$\" \"\\.txt$\"))

the buffer named \"*scratch*\" and any buffer whose name ends with
\".txt\" will be voice-activated.  Note that voice activation of the
minibuffer is controlled by vr-deprecated-activate-minibuffer.")

(defvar vr-deprecated-activate-minibuffer nil
  "*Flag controlling whether the minibuffer is voice-activated.")

(defvar vr-deprecated-voice-command-list '(vr-deprecated-default-voice-commands)
  "*The list of Emacs interactive commands that can be invoked by
voice.  Each element can be a command, a CONS cell containing
spoken text and a command or key sequence, or the special symbol
'vr-deprecated-default-voice-commands, which implicitly includes the voice
commands in vr-deprecated-default-voice-command-list (which see).

For example:

(setq vr-deprecated-voice-command-list
      '(vr-deprecated-default-voice-commands
	my-command
	(\"other command\" . my-other-command)
	(\"prefix shell command\" . [\?\\C-u \?\\M-\\S-!])))

sets up the voice commands

	Spoken			Invokes
	===============		=============
	my command		M-x my-command
	other command		M-x my-other-command
	prefix shell command	C-u M-S-! (i.e. C-u M-x shell-command)

along with all the commands on vr-deprecated-default-voice-command-list.")

(defconst vr-deprecated-default-voice-command-list
  '(

    ;; Lists
    (list "0to20" "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20")
    
    ;; vr-deprecated Mode commands
    ("activate buffer" . vr-deprecated-add-to-activation-list)

    ;; general emacs commands
    ("quit" . [?\C-g])
    ("undo" . undo)
    ("undo that" . undo)

    ;; keystrokes that often should not be self-inserted
    ("enter" . [?\C-j])
    ("tab" . [?\C-i])
    ("space" . [? ])

    ;; Repeat control commands.  These must be invoked with funcall, not M-x,
    ;; since M-x (or any non-RET event) terminates the repeat.
    ("faster" . "vr-deprecated-repeat-mult-rate 0.5")
    ("slower" . "vr-deprecated-repeat-mult-rate 2")
    ("stop" . "vr-deprecated-repeat-stop nil")

    ;; Repeat that.
    ("repeat that <0to20> times" . vr-deprecated-repeat-that)

    ;; files
    find-file
    save-buffer
    ("save file" . save-buffer)
    find-file-other-window
    find-file-other-frame
    
    ;; buffers
    switch-to-buffer
    kill-buffer
    switch-to-buffer-other-window
    switch-to-buffer-other-frame
    ("resynchronize" .  vr-deprecated-resynchronize)
    
    ;; windows
    ("split window" . split-window-vertically)
    other-window
    delete-window
    delete-other-windows
    
    ;; frames
    
    ;; cursor movement
    ("forward char <0to20>" . forward-char) 
    ("backward char <0to20>" . backward-char )
    ("forward word <0to20>" . forward-word)
    ("backward word <0to20>" . backward-word)
    ("next line <0to20>" . next-line)
    ("previous line <0to20>" . previous-line)
    ("forward paragraph <0to20>" . forward-paragraph)
    ("backward paragraph <0to20>" . backward-paragraph)
    ("scroll up" . scroll-up)
    ("scroll down" . scroll-down)
    ("page down" . scroll-up)
    ("page up" . scroll-down)
    ("beginning of line" . beginning-of-line)
    ("end of line" . end-of-line)
    ("beginning of buffer" . beginning-of-buffer)
    ("end of buffer" . end-of-buffer)

    ("move up" . vr-deprecated-repeat-move-up-s)
    ("move up slow" . vr-deprecated-repeat-move-up-s)
    ("move up fast" . vr-deprecated-repeat-move-up-f)
    ("move down" . vr-deprecated-repeat-move-down-s)
    ("move down slow" . vr-deprecated-repeat-move-down-s)
    ("move down fast" . vr-deprecated-repeat-move-down-f)
    ("move left" . vr-deprecated-repeat-move-left-s)
    ("move left slow" . vr-deprecated-repeat-move-left-s)
    ("move left fast" . vr-deprecated-repeat-move-left-f)
    ("move right" . vr-deprecated-repeat-move-right-s)
    ("move right slow" . vr-deprecated-repeat-move-right-s)
    ("move right fast" . vr-deprecated-repeat-move-right-f)

    ("move up <0to20>" . previous-line)
    ("move down <0to20>" . next-line)
    ("move left <0to20>" . backward-char)
    ("move right <0to20>" . forward-char)
    ("move left <0to20> words" . backward-word)
    ("move right <0to20> words" . forward-word)
    ("move left <0to20> sentences" . backward-sentence)
    ("move right <0to20> sentences" . forward-sentence)
    ("move left <0to20> paragraphs" . backward-paragraph)
    ("move right <0to20> paragraphs" . forward-paragraph)
    ("back <0to20>" . backward-char)
    ("forward <0to20>" . forward-char)
    ("back <0to20> words" . backward-word)
    ("forward <0to20> words" . forward-word)
    ("back <0to20> sentences" . backward-sentence)
    ("forward <0to20> sentences" . forward-sentence)
    ("back <0to20> paragraphs" . backward-paragraph)
    ("forward <0to20> paragraphs" . forward-paragraph)

    ;; deleting text
    ("delete char <0to20>" . delete-char)
    ("kill word <0to20>" . kill-word)
    ("backward kill word <0to20>" . backward-kill-word)
    ("kill line <0to20>" . kill-line)
    ("repeat kill line" . "vr-deprecated-repeat-kill-line 0.5")
    yank
    yank-pop
    ;; assumes yank-pop has key binding, else last-command==self-insert-command
    ("yank again" . yank-pop)
    ;; requires a key binding for yank, repeat yank to work!
    ("repeat yank" . vr-deprecated-repeat-yank)

    ;; Searching
    ("I search forward" . isearch-forward)
    ("I search backward" . isearch-backward)
    ("repeat I search forward" . vr-deprecated-repeat-search-forward-s)
    ("repeat I search backward" . vr-deprecated-repeat-search-backward-s)

    ;; formatting
    fill-paragraph
    
    ;; modes
    auto-fill-mode
    exit-minibuffer
    )
  "*A list of standard Emacs voice commands.  This list is used as-is
whenever vr-deprecated-voice-command-list (which see) includes the symbol
'vr-deprecated-default-voice-commands, or it can be appended explicitly in a
custom vr-deprecated-voice-command-list.")

(defvar vr-deprecated-log-do nil "*If non-nil, vr-deprecated mode prints debugging information
in the 'vr-deprecated-log-buff-name buffer.")
(setq vr-deprecated-log-do nil)

; DCF -- allows my Emacs 21.2 to work
; define hash-table-test for string=, if it isn't already defined
; (and if define-hash-table-test itself is defined)
(if (fboundp 'define-hash-table-test)
    (if (eq (get 'string= 'hash-table-test) nil)
	(define-hash-table-test 'string= 'string= 'sxhash)
    )
)

(defvar vcode-language-name-map  (make-hash-table :test 'string=)
    "Maps file extensions to programming languages"
)
(cl-puthash "py" "python" vcode-language-name-map)
(cl-puthash "c" "C" vcode-language-name-map)
(cl-puthash "h" "C" vcode-language-name-map)
(cl-puthash "cpp" "C" vcode-language-name-map)
(cl-puthash "hpp" "C" vcode-language-name-map) 
(cl-puthash "java" "java" vcode-language-name-map)
(cl-puthash "scala" "scala" vcode-language-name-map)
(cl-puthash "pl" "perl" vcode-language-name-map)
(cl-puthash "js" "javascript" vcode-language-name-map)
(cl-puthash "php" "php" vcode-language-name-map)
(cl-puthash "R" "C" vcode-language-name-map)

(defvar vcode-traces-on (make-hash-table :test 'string=)
"Set entries in this hashtable, to activate traces with that name.")

(cl-puthash "vcode-cmd-delete" 1 vcode-traces-on)

(defvar vr-deprecated-log-send nil "*If non-nil, vr-deprecated mode logs all data sent to the vr-deprecated
subprocess in the 'vr-deprecated-log-buff-name buffer.")

(defvar vr-deprecated-log-read nil "*If non-nil, vr-deprecated mode logs all data received
from the vr-deprecated subprocess in the 'vr-deprecated-log-buff-name buffer.")

(defvar vr-deprecated-log-buff-name "*Messages*" "Name of the buffer where vr-deprecated log messages are 
sent."
)
(setq message-log-max 1000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code for running in testing mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vcode-close-all-buffers ()
   (interactive)
   (let ((all-buffs (buffer-list)) (curr-buffer))
     (while all-buffs
       (setq curr-buffer (car all-buffs))
       (setq all-buffs (cdr all-buffs))
       (kill-buffer curr-buffer)
     )
   )
)

(defun vcode-mode (&optional status)
   (interactive "P")
   (vcode-configure-for-regression-testing nil)
   (vcode-mode-toggle-to status "vcode")
)


(defun vcode-interactive-test ()
  (interactive)
  (setq debug-on-error t)
  (setq debug-on-quit t)
  (vcode-mode 1)
)

(defun vcode-log-all ()
  (interactive)
  (setq vr-deprecated-log-do t)
  (setq vr-deprecated-log-read t)
  (setq vr-deprecated-log-send t)
)

(defun vcode-test ()
  (interactive)
  (setq debug-on-error t)
  (setq debug-on-quit t)

  ;;;
  ;;; Set number of lines and columns to standard value, otherwise
  ;;; tests involving pageUp and pageDown will have different results
  ;;; depending on the initial size of the frame.
  ;;;
  (setq default-frame-alist
      '((width . 80) (height . 40)))
  (set-frame-width (selected-frame) 80)
  (set-frame-height (selected-frame) 40)
 
  (vcode-close-all-buffers)
  (setq vr-deprecated-activation-list 
           (list "\.py$" "\.c$" "\.cpp$" "\.h$" "\.pl$" "\.R$" "\.hpp$" "\.java$"
                 "\.js$" "*Completions*" "\.php$" "\.scala$"))
  (vcode-configure-for-regression-testing t)
  (vcode-mode-toggle-to 1 "vcode-test")
  (setq vcode-is-test-editor t)
;  (vcode-configure-for-regression-testing nil)
;  (vcode-mode-toggle-to nil)
)

(defun vcode-config-py-mode-for-regression-testing ()
; DCF - tracing indentation problems (at Alain's suggestion)
   (vcode-trace "vcode-config-py-mode-for-regresion-testing" 
   "invoked, (current-buffer)=%S" (current-buffer))

   (setq py-smart-indentation nil) 
   (setq py-indent-offset 4)
   (setq tab-width 999)
   (auto-fill-mode 0)
; DCF - tracing indentation problems (at Alain's suggestion)
   (vcode-trace "vcode-config-py-mode-for-regresion-testing" 
   "py-indent-offset=%S" py-indent-offset)


)

(defun vcode-configure-for-regression-testing (status)
   (if status
       (progn 
; the "t" makes add-hook append, so that our test settings will override
; any user settings for things like auto-fill-mode
	 (add-hook 'python-mode-hook 
		   'vcode-config-py-mode-for-regression-testing t)
       )
     (remove-hook 'python-mode-hook 
		   'vcode-config-py-mode-for-regression-testing)
   )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-deprecated-mode-setup-hook nil
  "Hooks that are run after vr-deprecated Mode is enabled but before vr-deprecated.EXE is
successfully started (or connected to).  See also
vr-deprecated-mode-startup-hook, called later.")

(defvar vr-deprecated-mode-cleanup-hook nil
  "Hooks that are run after vr-deprecated Mode is disabled and after vr-deprecated.EXE has
exited or been told to exit.")

(defvar vr-deprecated-mode-startup-hook nil
  "Hooks that are run after vr-deprecated Mode is enabled, vr-deprecated.EXE is
successfully started (or connected to), and after vr-deprecated.EXE is initialized
with any per-connection state such as voice commands.  See also
vr-deprecated-mode-setup-hook, called earlier.")

(defvar vr-deprecated-mode-modified-hook nil
  "Hooks that are called whenever a voice activated buffer is modifed
for any reason, invoked by the 'modified-hooks property of vr-deprecated-overlay.
Arguments provided are OVERLAY AFTER BEG END LEN.  If any hook returns
non-nil, vr-deprecated Mode will *not* perform its normal modification processing
(ie: telling vr-deprecated.EXE/DNS about the change).

If vr-deprecated-changes-caused-by-sr-cmd is not nil, the hook has been invoked inside
vr-deprecated-cmd-make-changes, which means the current change comes from DNS,
not from the keyboard or elsewhere.

Danger, Will Robinson!")


(defvar vr-deprecated-wait-for-handshake-hook nil
   "This hook is invoked after opening a first network connection to the
speech server. It should wait until Emacs has shaken hands with the speech
server on that first connection."
)

(defvar vr-deprecated-deserialize-message-hook nil
   "This hook is used to deserialize a string message (received from the speech server) into a Lisp data structure."
)

(defvar vr-deprecated-serialize-message-hook nil
  "This hook is used to serialize a Lisp data structure into a string 
message that can be sent to the speech server."
)

(defvar vr-deprecated-serialize-changes-hook nil
  "This hook is used to serialize a list of changes to a buffer as a string
message that can be sent to the speech server"
)

(defvar vr-deprecated-send-activate-buffer-hook nil
  "This hook is used to tell the speech server that Emacs wants a particular
buffer to be speech enabled"
)

(defvar vr-deprecated-send-deactivate-buffer-hook nil
  "This hook is used to tell the speech server that Emacs wants a particular
buffer to be speech disabled"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-deprecated-mode nil
  "Non-nil turns on vr-deprecated (Voice Recognition) mode.  DO NOT SET THIS
VARIABLE DIRECTLY.  Call M-x vr-deprecated-mode instead.")

(defvar vr-deprecated-internal-activation-list nil
  "The working copy of vr-deprecated-activation-list.  Keeping it separate allows
re-starting vr-deprecated Mode to undo vr-deprecated-add-to-activation-list.")

(defvar vcode-mode-line " vr-deprecated"
  "String displayed in the minor mode list when vr-deprecated mode is enabled.
In the dictation buffer, the format is vr-deprecated:<micstate>.")
(make-variable-buffer-local 'vcode-mode-line)
(if (not (assq 'vr-deprecated-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(vcode-mode-toggle-to vcode-mode-line)
				 minor-mode-alist)))

(defvar vcode-mode-mic-state "not connected"
  "String storing the microphone state for display in the mode line.")

(defvar vr-deprecated-process nil "The vr-deprecated mode subprocess.")

(defvar vr-deprecated-emacs-cmds nil "The socket connection used to send messages 
initiated by Emacs, and to get responses from the SR server.")

(defvar vr-deprecated-dns-cmds nil "The socket connection used to receive messages
initiated by the SR server, and to send replies to them.
")

(defvar vr-deprecated-reading-string nil "Storage for partially-read commands
from the vr-deprecated subprocess.")

(defvar vr-deprecated-buffer nil "The current voice-activated buffer, or nil.
See vr-deprecated-activate-buffer and vr-deprecated-switch-to-buffer.")

(defvar vr-deprecated-ignore-changes nil "see comment in vr-deprecated-overlay-modified")
(defvar vr-deprecated-changes-caused-by-sr-cmd nil "see comment in vcode-report-insert-delete-change")
(defvar vcode-queued-changes nil "see comment in vcode-report-insert-delete-change")
(defvar vr-deprecated-dont-report-sr-own-changes t 
  "If t, then we don't report the changes that have been caused directly
by the SR. However, we do report changes done automatically by Emacs
in response to a change done by the SR (e.g. auto-fill).")

(defvar vr-deprecated-cmd-executing nil
  "If non-nil, the command symbol heard by NaturallySpeaking and
currently being executed by vr-deprecated Mode, for which vr-deprecated.EXE is expecting a
reply when done.")

(defvar vr-deprecated-resynchronize-buffer nil)
(make-variable-buffer-local 'vr-deprecated-resynchronize-buffer)

;; all of these variables have to do with abbreviation expansion functions
(defvar deferred-function nil)
(defvar vr-deprecated-deferred-deferred-function nil)
(defvar vr-deprecated-deferred-deferred-deferred-function nil)
;; this is necessary if people aren't using the abbreviation functions
(if (not (boundp 'fix-else-abbrev-expansion))
    (defun fix-else-abbrev-expansion () nil))

;(define-hash-table-test 'string= 'string= 'sxhash)
(defvar vr-deprecated-message-handler-hooks (make-hash-table :test 'string=)
  "This hash table associates command names with functions used to 
process the command."
)

(defvar vcode-cached-buffers (make-hash-table :test 'eq)
"Table of flags indicating for which VoiceCode-enabled buffers 
we have already sent an initial buffer_contents update.  
These buffers should be cached, so from now on we can just send 
insert and delete updates.  The keys of the hash are buffer objects"
)

(defconst vr-deprecated-nonlocal-exit-commands
  '(exit-minibuffer minibuffer-complete-and-exit)
  "These commands never exit and can't be executed in the make-changes
loop without screwing up the I/O.") 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixes for miscellaneous interaction issues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for some reason activating a buffer in vr-deprecated mode sometimes makes this
;; function fail to substitute every character in the region, which in
;; turn makes fill-paragraph end up in an infinite loop.  The "fix"
;; for this is to search the region after the command has completed,
;; and rerun it if it didn't work correctly.  It would be better to
;; figure out why it happens in the first place, but I have no idea.
(defadvice subst-char-in-region (after fix-mysterious-substitution-bug activate
				       compile )
  "make sure that the substitution worked"
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (if (search-forward (char-to-string fromchar) nil t)
	  ;; failed, retry
	  (progn
	    (message "advice redoing substitution")
	    (subst-char-in-region start end fromchar tochar noundo))
	))))

;; when else-mode expands a placeholder, the buffer frequently gets
;; out of sync.  We advise the piece of function that does this, and
;; ask for a manual resynchronization.
(defadvice else-replicate-placeholder-string (after
					      resynchronize-it
					      activate compile)
  "make vr-deprecated mode resynchronize the buffer after a placeholder has been
expanded, since they often make it go out of sync."

  (message "Resynchronizing vr-deprecated-buffer")
  (call-interactively 'vr-deprecated-resynchronize)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-deprecated-prefix-map nil "Prefix key used to access vr-deprecated mode commands.")
(defvar vr-deprecated-map nil)
(if vr-deprecated-map
    nil
  (setq vr-deprecated-map (make-sparse-keymap))
  (define-key vr-deprecated-map "ws" 'vr-deprecated-show-window)
  (define-key vr-deprecated-map "wh" 'vr-deprecated-hide-window)
  (define-key vr-deprecated-map "B" 'vr-deprecated-add-to-activation-list)
  (define-key vr-deprecated-map "b" 'vr-deprecated-switch-to-buffer)
  (define-key vr-deprecated-map "m" 'vr-deprecated-toggle-mic)
  (define-key vr-deprecated-map "q" 'vr-deprecated-quit)
  (define-key vr-deprecated-map "\C-\M-y" 'vr-deprecated-repeat-yank)
  )

(if vr-deprecated-prefix-map
    nil
  (setq vr-deprecated-prefix-map (make-sparse-keymap))
  (define-key vr-deprecated-prefix-map "\C-cv" vr-deprecated-map))

(if (not (assq 'vr-deprecated-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
	  (cons (cons 'vr-deprecated-mode vr-deprecated-prefix-map) minor-mode-map-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry points for global hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-deprecated-enter-minibuffer ()
  (if (and vr-deprecated-emacs-cmds vr-deprecated-activate-minibuffer)
      (vr-deprecated-activate-buffer (current-buffer))))

(defun vr-deprecated-post-command ()
  (vr-deprecated-log "--** vr-deprecated-post-command: invoked\n")
  (add-hook 'post-command-hook 'vr-deprecated-post-command)
  (if vr-deprecated-emacs-cmds
      (progn
	(vr-deprecated-maybe-activate-buffer (current-buffer))
	(if (and vr-deprecated-cmd-executing t) ;  (eq vr-deprecated-cmd-executing this-command))
; apparently this-command is not always set to the name of the
; command, for example kill-line is executed with "kill-region" in
; this-command, so this check doesn't really work
	    (progn
	      (vr-deprecated-send-cmd (format "command-done %s" vr-deprecated-cmd-executing))
	      (setq vr-deprecated-cmd-executing nil)))
	))
  (vr-deprecated-log "--** vr-deprecated-post-command: exited\n")
)

(defun vr-deprecated-kill-buffer ()
  (if (vr-deprecated-activate-buffer-p (current-buffer))
      (progn
        (cl-remhash (current-buffer) vcode-cached-buffers)
	(run-hooks 'vr-deprecated-send-kill-buffer-hook)
	)
    )
)

(defun vcode-send-suspended ()
  "sends suspended message to the mediator to let it know that Emacs
  is (about to be) suspended"
  (let ((empty-resp (make-hash-table :test 'string=)))
    (vr-deprecated-send-cmd 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "suspended" empty-resp)))
    )

)

(defun vcode-send-resuming ()
  "sends resuming message to the mediator to let it know that Emacs
  has just resumed execution (from being suspended)"
  (let ((empty-resp (make-hash-table :test 'string=)))
    (vr-deprecated-send-cmd 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "resuming" empty-resp)))
    )

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer activation control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-deprecated-filter (pred in)
  (let (out el)
    (while in
      (setq el (car in))
      (setq in (cdr in))
      (if (funcall pred el)
	  (setq out (cons el out)))
      )
out))
  
(defun vr-deprecated-add-to-activation-list (buffer)
  "Adds BUFFER, which can be a buffer name or buffer, to the list of
buffers that are voice activated.  Called interactively, adds the
current buffer.

The only way to undo the effect of this function is to re-start vr-deprecated
Mode."
  ;; If called interactively, vr-deprecated-post-command will activate the
  ;; current buffer (so this function doesn't have to).
  (interactive (list (current-buffer)))
  (if (bufferp buffer)
      (setq buffer (buffer-name buffer)))
  (if (vr-deprecated-activate-buffer-p buffer)
      nil
    (setq vr-deprecated-internal-activation-list
	  (cons (concat "^" (regexp-quote buffer) "$")
		vr-deprecated-internal-activation-list))))

(defun vr-deprecated-resynchronize (buffer)
  "asks vr-deprecated mode to resynchronize this buffer, if it has gotten out of
sync.  (That shouldn't happen, in an ideal world, but...)"
  (interactive (list (current-buffer)))
  (set-buffer buffer)
  (setq vr-deprecated-resynchronize-buffer t))

(defun vr-deprecated-activate-buffer-p (buffer)
  "Predicate indicating whether BUFFER matches any REGEXP element and
does not match any '(not REGEXP) element of
vr-deprecated-internal-activation-list.  BUFFER can be a buffer or a buffer name."
  (if (bufferp buffer)
      (setq buffer (buffer-name buffer)))
  (save-match-data
    (if (string-match "^ \\*Minibuf-[0-9]+\\*$" buffer)
        vr-deprecated-activate-minibuffer
      (and (vr-deprecated-filter (lambda (r) (and (stringp r) (string-match r buffer)))
                      vr-deprecated-internal-activation-list)
           (not (vr-deprecated-filter (lambda (r) 
                             (and (consp r) (eq (car r) 'not)
                                  (string-match (car (cdr r)) buffer)))
                           vr-deprecated-internal-activation-list))))
  )
)

(defun vr-deprecated-maybe-activate-buffer (buffer)
  ;; Deactivate whenever isearch mode is active.  This is a
  ;; "temporary" solution until isearch mode can be supported.
;  (vr-deprecated-log "--** vr-deprecated-maybe-activate-buffer: invoked\n")
  (if (and (not isearch-mode) (vr-deprecated-activate-buffer-p (buffer-name buffer)))
      (if (eq buffer vr-deprecated-buffer)
	  nil
	(vr-deprecated-activate-buffer buffer))
    (if vr-deprecated-buffer 
	(vr-deprecated-activate-buffer nil)))
;  (vr-deprecated-log "--** vr-deprecated-maybe-activate-buffer: exited\n")
)

(defun vr-deprecated-switch-to-buffer ()
  "Select the current vr-deprecated mode target buffer in the current window."
  (interactive)
  (if (buffer-live-p vr-deprecated-buffer)
      (switch-to-buffer vr-deprecated-buffer)
    (error "vr-deprecated target buffer no longer exists; use vr-deprecated-activate-buffer")))


(defun vcode-set-hooks (status)
  (vcode-set-after-change-functions status)
  (vcode-set-kill-emacs-hook status)
)

(defun vcode-set-kill-emacs-hook (status)
  (if status
      (progn
	(add-hook 'kill-emacs-hook 'vcode-disconnecting)
	)
    (remove-hook 'kill-emacs-hook 'vcode-disconnecting)
    )
)


(defun vcode-set-after-change-functions (status)
  (vr-deprecated-log "--** vcode-set-after-change-functions: invoked, (current-buffer)=%S, status=%S\n" (current-buffer) status)
  (if status
      (progn
;	(make-local-hook 'after-change-functions)
	(add-hook 'after-change-functions 'vcode-report-insert-delete-change)
	)
    (remove-hook 'after-change-functions 'vcode-report-insert-delete-change)
    )
  (vr-deprecated-log "--** vcode-set-after-change-functions: upon exit, (current-buffer)=%S, after-change-functions=%S\n" (current-buffer) after-change-functions)  
)

(defun vr-deprecated-activate-buffer (buffer)
  "Sets the target BUFFER that will receive voice-recognized text.  Called
interactively, sets the current buffer as the target buffer."
  (interactive (list (current-buffer)))
  (if (buffer-live-p vr-deprecated-buffer)
      (save-excursion
	(set-buffer vr-deprecated-buffer)
	(kill-local-variable 'vcode-mode-line)))
  (set-default 'vcode-mode-line (concat " vr-deprecated-" vcode-mode-mic-state))
  (setq vr-deprecated-buffer buffer)
  (if buffer
      (save-excursion
	(set-buffer buffer)
	(setq vcode-mode-line (concat " vcode-mode:" vcode-mode-mic-state))
	(run-hooks 'vr-deprecated-send-activate-buffer)
	)
    (run-hooks 'vr-deprecated-send-deactivate-buffer)
    )
  (force-mode-line-update)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tracking changes to voice-activated buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar vr-deprecated-modification-stack () )


(defun vr-deprecated-change-is-delete (beg end &optional len)
  (and (> len 0) (eq beg end))
)

(defun vr-deprecated-change-is-self-insert (beg end &optional len)
  (and (eq len 0) 
       (eq (- end beg) 1) 
       (eq (char-after beg) last-command-char))
)

(defun vr-deprecated-execute-deferred-function ()
  (if deferred-function
      (progn
	(setq vr-deprecated-deferred-deferred-function deferred-function)
	(setq deferred-function nil)
	(delete-backward-char 2)
	(fix-else-abbrev-expansion)
	(if (not (eq vr-deprecated-deferred-deferred-function
		     'else-expand-placeholder))
	    (progn
	      ;;(call-interactively deferred-deferred-function)
	      (setq vr-deprecated-deferred-deferred-deferred-function
		    vr-deprecated-deferred-deferred-function )
	      (setq vr-deprecated-deferred-deferred-function nil)
	      (vr-deprecated-execute-command
	       vr-deprecated-deferred-deferred-deferred-function))

	  )
	))
)

(defun vcode-merge-or-prepend-change (new-change)
  "merges the new-change with the first change in 'vcode-queued-changes, if
  possible, otherwise prepends it to the list.

  changes can be merged if they are both of type 'change-is-insert and
  represent contiguous insertions or deletions.
  "
(if (null vcode-queued-changes)
  (setq vcode-queued-changes (cons new-change vcode-queued-changes))
  (let* ((previous-change (car vcode-queued-changes))
        (previous-type (car previous-change))
        (new-type (car new-change))
        (new-entry new-change)
       )
 ;
 ; if previous change and new change are both inserts...
 ;
    (if (and (eq 'change-is-insert previous-type)
             (eq 'change-is-insert new-type))
      (let* ((previous-change-params (car (cdr previous-change)))
            (new-change-params (car (cdr new-change)))
            (previous-buffer (nth 0 previous-change-params))
            (new-buffer (nth 0 previous-change-params)))

 ;
 ; ... and if same buffer...
 ;
        (if (eq previous-buffer new-buffer)
          (let* ((previous-start (nth 1 previous-change-params))
                (previous-end (nth 2 previous-change-params))
                (previous-text (nth 3 previous-change-params))
        ; middle-end is the position of the end of the previous
        ; change in the current coordinates
                (middle-end (+ previous-start 
                   (length previous-text)))
                (new-start (nth 1 new-change-params))
                (new-text (nth 3 new-change-params))
                (new-end (nth 2 new-change-params)))
 ; 
 ; then check if they are contiguous, in either order
 ; 
            (cond
              ; need to compare in consistent coordinates
              ((eq middle-end new-start)
                (let*
                  ((start previous-start)
                   (text (concat previous-text new-text))
                   ; length of old text replaced is total amount 
                   ; replaced by the two changes
                   (end (+ previous-end (- new-end new-start)))
                   (contents (list new-buffer start end text)))
                  ; combine changes, pop previous 
                  (setq vcode-queued-changes (cdr vcode-queued-changes))
                  (setq new-entry (list 'change-is-insert contents))
                )
              )
              ((eq new-end previous-start)
                (let*
                  ((start new-start)
                   (text (concat new-text previous-text))
                   (end previous-end)
                   (contents (list new-buffer start end text)))
                  ; combine changes, pop previous 
                  (setq vcode-queued-changes (cdr vcode-queued-changes))
                  (setq new-entry (list 'change-is-insert contents))
                )
              )
            )
          )
        )
      )
    )
    ; push combined or new
    (setq vcode-queued-changes (cons new-entry vcode-queued-changes))
  )
)
)


(defun vcode-report-insert-delete-change (inserted-start inserted-end deleted-len)
"Invoked whenever an insertion or deletion change happens on the current 
buffer (if it is voice enabled). 

Changes are put in a changes queue `vcode-queued-changes.

If 'vr-deprecated-changes-caused-by-sr-cmd is nil, the changes were not
done as a response to a voice command. In that case, send the 
queued message right away.

If 'vr-deprecated-changes-caused-by-sr-cmd not nil, the changes have
been generated by a command from the SR server. In that case,
leave the messages in the queue. The event handler for that command
will send the queued changes as a big reply message, when it's done
executing.
"

  (let ((the-change nil))
    (if (vr-deprecated-activate-buffer-p (current-buffer))
	(progn 
	  (vcode-trace "vcode-report-insert-delete-change" "inserted-start=%S inserted-end=%S deleted-len=%S\n" inserted-start inserted-end deleted-len)
          (if (and (cl-gethash (current-buffer) vcode-cached-buffers)
                   (not (string= (buffer-name (current-buffer))
                        "*Completions*")))
              (setq the-change
		(vcode-generate-raw-change-description 'change-is-insert (list (buffer-name) inserted-start inserted-end deleted-len))
	      )
              (setq the-change
		(vcode-generate-raw-change-description 'change-is-contents 
                    (list (buffer-name)))
	      )
              (cl-puthash (current-buffer) t vcode-cached-buffers)
          )
	  
	  (vcode-trace "vcode-report-insert-delete-change" "the-change=%S" the-change)
	  
          (vcode-merge-or-prepend-change the-change)
;	  (setq vcode-queued-changes (cons the-change vcode-queued-changes))
	  
	  (if (not vr-deprecated-changes-caused-by-sr-cmd)
	      (vcode-send-queued-changes)
	    )
	  
	  ;;
	  ;; What does this do? Is it still necessary if we disable dabbrevs
	  ;; and electric punctuation marks during executio of utterances?
	  (vr-deprecated-execute-deferred-function)
       )
    )
  )
)


(defun vcode-report-goto-select-change (buff-name sel-start sel-end)
  "Invoked whenever a change in the cursor position or marked selection 
happens on a buffer (if it is voice enabled). 

Changes are put in a changes queue `vcode-queued-changes.
"
  (vr-deprecated-log "--** vcode-report-goto-select-change: buffer %S, start, end = %S, %S\n"
  buff-name sel-start sel-end)
  (setq vcode-queued-changes 
	(cons 
;	   (list 'change-is-select buff-name sel-start sel-end)
	   (vcode-generate-raw-change-description  'change-is-select (list buff-name sel-start sel-end))
	   vcode-queued-changes))
)

(defun vcode-generate-raw-change-description (change-type change-data)

  (let ((change-desc) (buff-name) (inserted-start) (inserted-end) 
	(deleted-length) (deleted-start) (deleted-end) (inserted-text)
        (visible-range))
    (vcode-trace "vcode-generate-raw-change-description" "change-type=%S, change-data=%S" change-type change-data)
    (if (eq 'change-is-select change-type)
	  (setq change-desc (list change-type change-data))
      (if (eq 'change-is-insert change-type)
          (progn
              (setq buff-name (nth 0 change-data))
              (setq inserted-start (nth 1 change-data))
              (setq inserted-end (nth 2 change-data))
              (setq deleted-length (nth 3 change-data))	
              (setq deleted-start inserted-start)
; Emacs never calls the after-change-functions with a third argument of
; nil, so the following only applies to the direct call to 
; vcode-generate-raw-change-description from
; vcode-generate-whole-buffer-changes?  - DCF
; Yes - AD
              (if deleted-length
                  (setq deleted-end (+ deleted-start deleted-length))
                (setq deleted-end nil)
              )
              (vcode-trace "vcode-generate-raw-change-description" 
		   "buff-name=%S, inserted-end=%S, deleted-length=%S, deleted-start=%S, deleted-end=%S"
		   buff-name inserted-end deleted-length deleted-start 
		   deleted-end)
              (save-excursion
                (set-buffer buff-name)
; shouldn't this be set-buffer?  -- DCF
;	(switch-to-buffer buff-name)
;;; AD: We use 'buffer-substring-no-properties because VCode wouldn't know
;;;     what to do with character properties like font, color, etc...
;;;
                (setq inserted-text (buffer-substring-no-properties 
			     inserted-start inserted-end))
              )

              (vcode-trace "vcode-generate-raw-change-description" "** inserted-text=%S" inserted-text)
              (setq change-desc (list change-type (list buff-name deleted-start deleted-end inserted-text)))
              (vcode-trace "vcode-generate-raw-change-description" "** EXITING with change-desc=%S" change-desc)
          )
          (progn
              (vcode-trace "vcode-generate-raw-change-description"
                    "change is contents")
              (save-excursion
                (setq buff-name (nth 0 change-data))
                (set-buffer buff-name)
                (setq inserted-text (buffer-substring-no-properties 
			     (point-min) (point-max)))
              )
              (vcode-trace "vcode-generate-raw-change-description"
                    "found contents")
              (setq change-desc (list 'change-is-contents 
                (list buff-name inserted-text)))
          )
      )
    )
    (vcode-trace "vcode-generate-raw-change-description"
"change-type=%S, change-desc=%S" change-type change-desc)
    change-desc
  )
)

(defun vr-deprecated-string-replace (src regexp repl)
  (let ((i 0))
    (while (setq i (string-match regexp src))
      (setq src (concat (substring src 0 i)
			repl
			(substring src (match-end 0))))
      (setq i (match-end 0))))
  src)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard lockout during voice recognition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-deprecated-recognizing nil)

(defun vr-deprecated-sleep-while-recognizing ()
  (interactive)
  (let* ((first t) (count 0))
    (while (and (< count 200) vr-deprecated-recognizing (string= vcode-mode-mic-state "on"))
      (if first (message "Waiting for voice recognition..."))
      (setq first nil)
      (setq count (1+ count))
      (sleep-for 0 50))
    (if (eq count 200) 
	(message "Time out in vr-deprecated-sleep-while-recognizing!")
      (message nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subprocess communication.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun vcode-trace (trace-name format-string &rest s)
   (let ()
     (if (or (not trace-name) (is-in-hash trace-name vcode-traces-on))
        (progn

          ;;;
          ;;; Make sure message log buffer is large enoug (in case someone else
          ;;; changed its size on us
          ;;;
          (setq message-log-max 1000000)

          (setq format-string (concat "-- " trace-name ": " format-string "\n"))
;;          (message "-- vcode-trace: format-string=%S" format-string)
          (if s
              (progn
;;		 (message "-- vcode-trace: format-string=%S" format-string)
;;                 (message "-- vcode-trace: s=%S" s)
                 (setq args (append (list format-string) s))
;;                 (message "-- vcode-trace: args=%S" args)
                 (apply 'message args)
              )
            (message format-string)
           )
	 )
      )
   )
)



(defun vcode-trace-OLD (trace-name format-string &rest s)
   (let ((buf) (win))
     (setq buf (get-buffer-create vr-deprecated-log-buff-name))
     (setq win (get-buffer-window buf 't))
     (if (or (not trace-name) (is-in-hash trace-name vcode-traces-on))
        (progn
          (setq format-string (concat "-- " trace-name ": " format-string "\n"))
          (save-excursion
            (set-buffer buf)
            (goto-char (point-max))

            (if s
                (insert (apply 'format (append (list format-string) s)))
              (insert format-string)
              )
            (if win
                (set-window-point win (point-max)))
           )
        )
     )
   )
)
 
(defun vcode-trace-call-stack (trace-name)
  (let ((curr-buff (buffer-name))) 
    (if (or (not trace-name) (is-in-hash trace-name vcode-traces-on))
       (progn
	 (print "*** Call stack is:\n")
	 (backtrace)
	 (print "\n*** END OF Call stack\n")
       )
    )
  )
)

(defun vcode-warning (format-string &rest args)
   (setq format-string (format "** VCode WARNING: %S" format-string))
   (message (apply 'format (append (list format-string) args)))
)

(defun vr-deprecated-log (&rest s)
  (if vr-deprecated-log-do
      (let* (
	     (buf (get-buffer-create vr-deprecated-log-buff-name))
	     (win (get-buffer-window buf 't))
	     )
	(save-excursion
	  (set-buffer buf)
	  (goto-char (point-max))

 	  ;;;
	  ;;; If (length s) > 1, first argument is a format string, and the 
	  ;;; other arguments are objects to be formatted into that string.
	  ;;;
	  ;;; If (length s) == 1, its single element is a string that's
	  ;;; not meant to be used as a format string. Just insert it as
	  ;;; is instead of formatting it, because it might just happen to 
	  ;;; contain some format descriptions (e.g. %s), in which case
	  ;;; formatting it would cause "not enough arguments" error.
	  ;;;
	  (if (= 1 (length s))
	      (insert (nth 0 s))
	    (insert (apply 'format s))
	    )
	  (if win
	      (set-window-point win (point-max)))
	  )))
  t
)

(defun vr-deprecated-sentinel (p s)
  (if (equal s "finished\n")
      (progn
	(if (processp vr-deprecated-process)
	    (delete-process vr-deprecated-process))
	(if (processp vr-deprecated-emacs-cmds)
	    (delete-process vr-deprecated-emacs-cmds))
	(if (processp vr-deprecated-dns-cmds)
	    (delete-process vr-deprecated-dns-cmds))
	(setq vr-deprecated-process nil
	      vr-deprecated-emacs-cmds nil
	      vr-deprecated-dns-cmds nil))
    (error "vr-deprecated process exited with status \"%s\"" s)))




;; executes a command, and runs the appropriate hooks.  It's used by
;; heard-command and by the deferred-function executions.  vr-deprecated-command
;; can either be a symbol or a list.
(defun vr-deprecated-execute-command (vr-deprecated-command)
  (let ((cmd (or (and (listp vr-deprecated-command ) (car vr-deprecated-command))
		 vr-deprecated-command)))
	  (run-hooks 'pre-command-hook)
 	  (condition-case err
	      (if (and (listp vr-deprecated-command) 
		       (> (length vr-deprecated-command) 1))
		  (apply cmd (cdr vr-deprecated-command))
		(call-interactively cmd))
	    ('wrong-number-of-arguments
	     (ding)
	     (message
	      "vr-deprecated Mode: Wrong number of arguments calling %s"
	      vr-deprecated-command))
	    ('wrong-type-argument 'error
				  (ding)
				  (message "vr-deprecated Mode: %s calling %s"
					   (error-message-string err)
					   vr-deprecated-command )))
 	  (let ((this-command cmd))
	    (run-hooks 'post-command-hook)))
  t)

(defun vcode-execute-command-string (command-string)
  "Execute a string as though it was typed by the user.
"
  (let ((abbrev-mode-was))
    (vcode-trace "vcode-execute-command-string" "command-string=%S" command-string)
    (vr-deprecated-log "-- vcode-execute-command-string: command-string=%S" command-string)
    (if unread-command-events
;        (vcode-trace "vcode-execute-command-string"
;        "unread-command-events=%S" unread-command-events)
         (vr-deprecated-log "-- vcode-execute-command-string: unread-command-events=%S" 
             unread-command-events)
    )
    (if (input-pending-p)
;        (vcode-trace "vcode-execute-command-string"
;        "unread-command-events=%S" unread-command-events)
         (vr-deprecated-log "-- vcode-execute-command-string: input-pending" )
    )

;;; AD: Why set those? They make Emacs run in debug mode even
;;;     when users are using VCode for real.
;;    (setq debug-on-error t)
;;    (setq debug-on-quit t)

    ;;
    ;; Convert the string to a list of Emacs events
    ;;
    (setq unread-command-events
	  (append unread-command-events
		  (listify-key-sequence command-string)))

    ;;;
    ;;; Execute each event
    ;;;

    ;; turn off abbrevs temporarily
    (setq abbrev-mode-was abbrev-mode)
    (abbrev-mode 0)
    (while unread-command-events
      (let* ((event (read-key-sequence-vector nil))
 	     (command (key-binding event))
 	     (last-command-char (elt event 0))
 	     (last-command-event (elt event 0))
 	     (last-command-keys event)
 	     )
	(vcode-trace "vcode-execute-command-string" "last-command-char=%S, command=%S" last-command-char command)
	(run-hooks 'pre-command-hook)
	(command-execute command nil )
	(run-hooks 'post-command-hook)
      )
    )
    (if abbrev-mode-was
        (abbrev-mode 1))
  )
)



(defun vcode-send-queued-changes ()
  "Sends the message queue.

   If 'vr-deprecated-changes-caused-by-sr-cmd is not nil, then these changes happened
   in response to a SR command (and 'vr-deprecated-changes-caused-by-sr-cmd is the name
   of that command)."

  (let ((change-message nil))
    (vcode-trace "vcode-send-queued-changes" "vcode-queued-changes=%S" vcode-queued-changes)

;    (vcode-filter-queued-changes)
    (vcode-trace "vcode-send-queued-changes" "after vcode-filter-queued-changes, vcode-queued-changes=%S" vcode-queued-changes)

    (setq change-message
       (run-hook-with-args 'vr-deprecated-serialize-changes-hook
			   (nreverse vcode-queued-changes)))

    (vcode-trace "vcode-send-queued-changes" "** AFTER vr-deprecated-serialize-changes-hook")

    ;;;
    ;;; If these changes happened in response to a command, send them on 
    ;;; the reply channel.
    ;;; Otherwise, send them on the cmd channel.
    ;;;
    (if vr-deprecated-changes-caused-by-sr-cmd
	(vr-deprecated-send-reply change-message)
      (vr-deprecated-send-cmd change-message)
    )
    (setq vcode-queued-changes nil)

    (vcode-trace "vcode-send-queued-changes" "** EXITING")

  )
)

(defun vcode-filter-queued-changes ()
   "Filter queued changes to patch apparent bugs in Emacs' change reporting. 

There seem to be bugs in the way that Emacs reports changes to buffers
like *Buffer List* and *Completions*. Sometimes, the start and end position of a change do 
not take into account insertions that have been done in a previous change.

This function filters the queued changes, and replaces any change report for
such buffers by a unique change report at the end of queue, reporting the 
whole content of the buffer."

   (let ((filtered-changes nil) (a-change  nil) (buffer-list-has-changed nil)
         (buff-name nil) (whole-buff-list-change nil) ( buffer-list-len nil))
     (vcode-trace "vcode-filter-queued-changes" "upon entry, vcode-queued-changes=%S" vcode-queued-changes)
     (while vcode-queued-changes
       (setq a-change (car vcode-queued-changes))
       (setq vcode-queued-changes (cdr vcode-queued-changes))
       (setq buff-name (nth 0 (nth 1 a-change)))
       (vcode-trace "vcode-filter-queued-changes" "** buff-name=%S" buff-name)
       (if (string= buff-name "*Completions*")
           (setq buffer-list-has-changed t)
	 (setq filtered-changes (append filtered-changes (list a-change)))
	 (vcode-trace "vcode-filter-queued-changes" "** added this change to filtered-changes, filtered-changes=%S" filtered-changes)
       )       
     )

     (vcode-trace "vcode-filter-queued-changes" "After removing *Completions* changes, filtered-changes=%S" filtered-changes)
     (if buffer-list-has-changed 
	 (setq filtered-changes 
	       (append (vcode-generate-whole-buffer-changes "*Completions*")
		       filtered-changes))
     )
    (setq vcode-queued-changes filtered-changes)
    (vcode-trace "vcode-filter-queued-changes" "** UPON EXIT, vcode-queued-changes=%S" vcode-queued-changes)
   )
)

(defun vcode-generate-whole-buffer-changes (buff-name)
  "Generates a change report that sets the whole content of buff-name."
  (let ((change nil) (change-list nil) (selection nil))
    (vcode-trace "vcode-generate-whole-buffer-changes" "buff-name=%S" buff-name)
    (save-excursion
      (set-buffer buff-name)
      (setq buff-len (1+ (- (point-max) (point-min))))
      (setq change 
	    (vcode-generate-raw-change-description 
	     'change-is-insert 
	     (list buff-name (point-min) buff-len nil)))
      (setq change-list (cons change change-list))
      (setq selection (vcode-make-sure-no-nil-in-selection (point) 
	  (if mark-active (mark))))
      (setq change 
	    (vcode-generate-raw-change-description  
	     'change-is-select (list buff-name (nth 0 selection) 
				     (nth 1 selection))))
; this is incorrect: This range is the new range, which correctly
; retrieves the complete contents of the buffer.  However, in order to 
; ensure that VoiceCode replaces the entire contents of its cache, the 
; change message would need to include the OLD start and end of the
; buffer.  A better way is to use the new buffer contents action instead
; of insert.
; - DCF
      (setq change-list (cons change change-list))      
    )
    (vcode-trace "vcode-generate-whole-buffer-changes" "buff-name=%S, returning change=%S" buff-name change)
    change-list
  )
)

(defun vr-deprecated-execute-event-handler (handler vr-deprecated-request)
  (let ((vr-deprecated-changes-caused-by-sr-cmd (nth 0 vr-deprecated-request))
	(vr-deprecated-request-mess (nth 1 vr-deprecated-request)))
    (vcode-trace "vr-deprecated-execute-event-handler" "vr-deprecated-changes-caused-by-sr-cmd=%S, handler=%S\n" vr-deprecated-changes-caused-by-sr-cmd handler)

    ;;;
    ;;; Fix the message arguments that refer to buffer positions
    ;;; (Emacs counts from 1 while VCode counts from 1, and VCode
    ;;; may send some nil positions)
    ;;;
    (setq vr-deprecated-request 
	  (list vr-deprecated-changes-caused-by-sr-cmd
		(vcode-fix-positions-in-message 
		 vr-deprecated-request-mess 'emacs)
		))

    (if debug-on-error
	;;;
	;;; If in debug mode, let the debugger intercept errors.
	;;;
        (progn
	    (vcode-trace "vr-deprecated-execute-event-handler" "Executing handler in debug mode\n")
  	    (apply handler (list vr-deprecated-request)) 
	    (vcode-trace "vr-deprecated-execute-event-handler" "Finished executing handler in debug mode\n")
        )

      ;;; 
      ;;; Not debug mode. We intercept errors ourself.
      ;;;
      (condition-case err
          (progn 
	    (vcode-trace "vr-deprecated-execute-event-handler" "Executing handler in NON-debug mode\n")
	    (apply handler (list vr-deprecated-request))
	    (vcode-trace "vr-deprecated-execute-event-handler" "Finished Executing handler in NON-debug mode\n")
	  )
	('error 
	 (progn
	   (message (format "Error executing vr-deprecated request %s"
				       vr-deprecated-request))
	   (run-hook-with-args 'vr-deprecated-upon-cmd-error vr-deprecated-request)
	   )
	 )
	)
      )
    (setq vr-deprecated-changes-caused-by-sr-cmd nil)
    )
)


(defun  vcode-try-parsing-message (possibly-incomplete-mess)
  (let ((parsed nil) (idx))

    (condition-case err
	(progn
	  (vcode-trace "vcode-try-parsing-message" 
		       "trying to parse possibly-incomplete-mess=%S\n" 
		       possibly-incomplete-mess)
   
	  (setq parsed 
		(run-hook-with-args 
		 'vr-deprecated-deserialize-message-hook vr-deprecated-reading-string))
	  (setq idx (elt parsed 1))
	  (setq vr-deprecated-reading-string 
		(if (< idx (1- (length vr-deprecated-reading-string)))
		    (substring vr-deprecated-reading-string (1+ idx))
		  ""))
	  (vcode-trace "vcode-try-parsing-message" "parsing worked, idx=%S, vr-deprecated-reading-string=%S\n" idx vr-deprecated-reading-string)

	 )
      ('error
	    (vcode-trace "vcode-try-parsing-message" "parsing failed\n")
      )
    )
    (vcode-trace "vcode-try-parsing-message" "exiting\n")
    parsed
  )
)
		
(defun vr-deprecated-output-filter (p s)
  (vcode-trace "vr-deprecated-output-filter" "invoked\n")
  (setq vr-deprecated-reading-string (concat vr-deprecated-reading-string s))
  (vcode-trace "vr-deprecated-output-filter" "invoked\n")
  (let* ((handler) (parsed) (vr-deprecated-request) (vr-deprecated-cmd))
    
    (setq parsed (vcode-try-parsing-message vr-deprecated-reading-string))
    
    (if parsed
	(progn
	  (setq vr-deprecated-request (elt parsed 0))
	  (setq vr-deprecated-cmd  (elt vr-deprecated-request 0))
	  (setq handler (cl-gethash vr-deprecated-cmd vr-deprecated-message-handler-hooks))
          (vcode-trace "vr-deprecated-output-filter" 
            "received request %S" vr-deprecated-cmd)
	  (if handler
	      (vr-deprecated-execute-event-handler handler vr-deprecated-request)
  	      ;;;
	      ;;; Process should degrade gracefully if an unknown command is 
	      ;;; received
	      ;;;
	    (error "VCode Error: Received unknown command %S\n" vr-deprecated-cmd)
	    )
	  )
      )
    )
  (vcode-trace "vr-deprecated-output-filter" "exited\n")
)
     

(defun vr-deprecated-send-reply (msg)
  (if (and vr-deprecated-dns-cmds (eq (process-status vr-deprecated-dns-cmds) 'open))
      (progn
	(if (integerp msg)
	    (setq msg (int-to-string msg)))

	;;; Make sure the message is encoded in UTF-8
	(setq msg (encode-coding-string msg 'utf-8))

        (vcode-trace "vr-deprecated-send-reply" "sending reply %S..."
(substring msg 0 20))
	(if vr-deprecated-log-send
	    (vr-deprecated-log "<- r %s\n" msg))
;;; Alain what does that do? Should it be part of vr-deprecated-serialize-message?
;;; 	(process-send-string vr-deprecated-dns-cmds (vr-deprecated-etonl (length msg)))

	(process-send-string vr-deprecated-dns-cmds msg))
    (message "VCode Mode DNS reply channel is not open!"))
  )

(defun vr-deprecated-send-cmd (msg)
  (if (and vr-deprecated-emacs-cmds (eq (process-status vr-deprecated-emacs-cmds) 'open))
      (progn

        ;;; Make sure the message is encoded in UTF-8
        (setq msg (encode-coding-string msg 'utf-8))


	(if vr-deprecated-log-send
	    (vr-deprecated-log "<- c %s\n" msg))
;;; Should this be part of vr-deprecated-serialize-message???
;;;	(process-send-string vr-deprecated-emacs-cmds (vr-deprecated-etonl (length msg)))

	(process-send-string vr-deprecated-emacs-cmds msg))
    (message "VCode Mode command channel is not open: %s" msg)))

;; ewww
(defun vr-deprecated-etonl (i)
  (format "%c%c%c%c"
	  (lsh (logand i 4278190080) -24)
	  (lsh (logand i 16711680) -16)
	  (lsh (logand i 65280) -8)
	  (logand i 255)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subprocess commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-deprecated-quit ()
  "Turn off vr-deprecated mode, and cause the vr-deprecated mode subprocess to exit cleanly."
  (interactive)
  (vcode-mode-toggle-to 0))

(defun vr-deprecated-toggle-mic ()
  "Toggles the state of the Dragon NaturallySpeaking microphone:
off -> on, {on,sleeping} -> off."
  (interactive)
  (vr-deprecated-send-cmd "toggle-mic"))

(defun vr-deprecated-show-window ()
  (interactive)
  (vr-deprecated-send-cmd "show-window"))

(defun vr-deprecated-hide-window ()
  (interactive)
  (vr-deprecated-send-cmd "hide-window"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subprocess initialization, including voice commands.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vcode-connect (host port)
  (condition-case e
      (progn
 	(setq vr-deprecated-emacs-cmds (open-network-stream "vr-deprecated-emacs" nil
 						 host port))

	;;;
	;;; Connect an output filter to the Emacs commands network-stream
	;;; in case the speech server needs to to some handshaking on that
	;;; connection.
	;;;
	(set-process-filter vr-deprecated-emacs-cmds 'vr-deprecated-output-filter)
	(vr-deprecated-log "connecting to speech server %s\n" vr-deprecated-emacs-cmds)
	
	;;;
	;;; Possibly wait until Emacs has shaken hands with speech server
	;;; before opening second network stream.
	;;;
	(run-hooks 'vr-deprecated-wait-for-handshake-hook)

	(setq vr-deprecated-dns-cmds (open-network-stream "vr-deprecated-dns" nil host (1+ port)))
	(process-kill-without-query vr-deprecated-emacs-cmds)
	(process-kill-without-query vr-deprecated-dns-cmds)
	(set-process-filter vr-deprecated-dns-cmds 'vr-deprecated-output-filter)
	(if vr-deprecated-process
	    (set-process-filter vr-deprecated-process nil))
	t)

    ('error (progn
	      (message "vr-deprecated Mode: cannot connect to %s:%d" host port)
	      (message (format "Error condition was: %S" e))
	      (vcode-mode-toggle-to 0)
	      nil)))
)

;; functionp isn't defined in Win 95 Emacs 19.34.6 (!??!?)
(defun vr-deprecated-functionp (object)
  "Non-nil if OBJECT is a type of object that can be called as a function."
  (or (subrp object) (byte-code-function-p object)
      (eq (car-safe object) 'lambda)
      (and (symbolp object) (fboundp object))))

(defun vr-deprecated-startup ()
  "Initialize any per-execution state of the vr-deprecated Mode subprocess."

  (run-hooks 'vr-deprecated-initialize-server-hook)

  ;; don't set up these hooks until after initialization has succeeded
  (add-hook 'post-command-hook 'vr-deprecated-post-command)
  (add-hook 'minibuffer-setup-hook 'vr-deprecated-enter-minibuffer)
  (vr-deprecated-maybe-activate-buffer (current-buffer))
  (run-hooks 'vr-deprecated-mode-startup-hook)
  )

(defun vr-deprecated-kill-emacs ()
  (vcode-mode-toggle-to 0)
  (sleep-for 1)
  t)

(defun vr-deprecated-cmd-terminating (vr-deprecated-request)
  (let (vr-deprecated-emacs-cmds)
    (vcode-mode-toggle-to 0))
  (if vr-deprecated-host
      (vr-deprecated-sentinel nil "finished\n"))
  (message "vr-deprecated process terminated; vr-deprecated Mode turned off")
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vr-deprecated Mode entry/exit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vcode-mode-toggle-to (arg &optional speech-server)
  "Toggle vr-deprecated mode.  With argument ARG, turn vr-deprecated mode on iff ARG is
positive.

The optional argument 'speech-server gives the name of the speech
server to configure vr-deprecated Mode for (either 'vr-deprecated,  'vcode, or 'vcode-test). 

If not specified,
use whatever speech server vr-deprecated Mode is currently configured for.

vr-deprecated mode supports Dragon NaturallySpeaking dictation, Select 'N
Say(tm), and voice commands in Emacs buffers.  See README.txt for
instructions.

\\{vr-deprecated-map}"
 (interactive "P")

  (setq vr-deprecated-vcode-test-client 0)
  (if (null speech-server)
      (setq speech-server "vcode"))

  (cond
    ((string= speech-server "vr-deprecated") (vr-deprecated-mode-configure-for-vr-deprecated-server))
    ((string= speech-server "vcode") (vr-deprecated-mode-configure-for-vcode-server))
    ((string= speech-server "vcode-test") (setq vr-deprecated-vcode-test-client 1) (vr-deprecated-mode-configure-for-vcode-server))      
   )

  (vcode-mode-activate arg)
)

(defun vcode-mode-activate (arg)
  "Activates the vr-deprecated mode, after it has been configured for a 
   particular speech server"

  (setq desired-status 
	(if (null arg) (not vr-deprecated-mode)
 	  (> (prefix-numeric-value arg) 0)))

  (if (equal desired-status vr-deprecated-mode)
      ;;;
      ;;; Don't do anything if VCode is already in the 
      ;;; desired state
      ;;;
      (if desired-status 
	  (message "VCode already connected")
	(message "VCode already disconnected"))

    (setq vr-deprecated-mode desired-status)

    (if vr-deprecated-mode
	;; Entering vr-deprecated mode
	(progn
	  (vr-deprecated-log "starting vr-deprecated mode %s\n" vr-deprecated-host)
	  (setq vr-deprecated-reading-string nil)
	  (setq vcode-mode-mic-state "not connected")
	  (set-default 'vcode-mode-line (concat " vcode-mode" vcode-mode-mic-state))
	  (setq vr-deprecated-internal-activation-list vr-deprecated-activation-list)
	  (setq vr-deprecated-cmd-executing nil)
	  (add-hook 'kill-emacs-hook 'vr-deprecated-kill-emacs)
	  (run-hooks 'vr-deprecated-mode-setup-hook)

	  (if vr-deprecated-host
	      (vcode-connect vr-deprecated-host vr-deprecated-port)
	    (setq vr-deprecated-process (start-process "vr-deprecated" vr-deprecated-log-buff-name "python" "E:\\VoiceCode\\VCode.TCP_IP\\Mediator\\tcp_server.py"))
	    (process-kill-without-query vr-deprecated-process)
	    (set-process-sentinel vr-deprecated-process 'vr-deprecated-sentinel))
	  (vcode-set-hooks 1)
	  )
    
      ;; Leaving vr-deprecated mode
      (remove-hook 'post-command-hook 'vr-deprecated-post-command)
      (remove-hook 'minibuffer-setup-hook 'vr-deprecated-enter-minibuffer)
      (remove-hook 'kill-buffer-hook 'vr-deprecated-kill-buffer)
      (remove-hook 'suspend-hook 'vcode-send-suspended)
      (remove-hook 'suspend-resume-hook 'vcode-send-resuming)
      (setq frame-title-format 
	    `("%b -- " (, invocation-name "@" system-name)))
      (vr-deprecated-activate-buffer nil)
      (if vr-deprecated-host
; DCF - I think this is left over from vr-mode.  We should send an
; editor_disconnecting message (not just a string)
;      (vr-deprecated-send-cmd "exit")
	  (vcode-disconnecting))
      (vr-deprecated-sentinel nil "finished\n")
      (vcode-set-after-change-functions nil)
      (run-hooks 'vr-deprecated-mode-cleanup-hook)
      )
    (force-mode-line-update)
    )
)



(defun vcode-disconnecting ()
    "sends editor_disconnecting message to the mediator"
  (let ((empty-resp (make-hash-table :test 'string=)))
    (vr-deprecated-send-cmd 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "editor_disconnecting" empty-resp)))
    )

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration allowing vr-deprecated-mode to interact with the vr-deprecated.exe speech
;;; server.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-deprecated-deserialize-message (message)
   "Parse a message serialized send by vr-deprecated.exe as a sexp."

   (read-from-string message)
)

(defun vr-deprecated-serialize-message (message)
  "Serialises a LISP data structure into a message that can be parsed by
   vr-deprecated.exe" 

  (let ((mess-name (car message)) (mess-content (cdr message)) (cmd))
    (vcode-trace "vr-deprecated-serialize-message" "mess-name is %S"
mess-name)
    (cond

     ;;; Alain: Later on, put conditions for all the messages that vr-deprecated.exe
     ;;; might need to receive.
     ((eq mess-name "change-text")
      (let ((beg (elt 0 )) ())
        (setq cmd (format "%s \"%s\" %d %d %d %d %s" 
 			 (elt mess-content 0) (elt mess-content 2) 
 			 (elt mess-content 3) (elt mess-content 4)
 			 (elt mess-content 5)))
      ))
     )
    (setq cmd (format "%s %s" mess-name cmd))
    cmd
  )
)


;; (defun vr-deprecated-initialize-server ()
;;   "Initialize the vr-deprecated.exe speech server with a series of voice commands and 
;; grammars."
;;   (let ((l (lambda (x)
;; 	     (cond ((eq x 'vr-deprecated-default-voice-commands)
;; 		    (mapcar l vr-deprecated-default-voice-command-list))
;; 		   ((symbolp x)
;; 		    (vr-deprecated-send-cmd
;; 		     (concat "define-command "
;; 			     (vr-deprecated-strip-dash x) "|" (symbol-name x))))
;; 		   ((and (listp x) (eq (car x) 'list))
;; 		    (vr-deprecated-send-cmd
;; 		     (format "define-list %s|%s" (nth 1 x) (nth 2 x))))
;; 		   ((and (consp x) (vectorp (cdr x)))
;; 		    (vr-deprecated-send-cmd
;; 		     (format "define-command %s|%s" (car x) (cdr x))))
;; 		   ((and (consp x) (symbolp (cdr x)))
;; 		    (vr-deprecated-send-cmd
;; 		     (format "define-command %s|%s" (car x) (cdr x))))
;; 		   ((and (consp x) (stringp (cdr x)))
;; 		    (vr-deprecated-send-cmd
;; 		     (format "define-command %s|%s" (car x) (cdr x))))
;; 		   (t
;; 		    (error "Unknown vr-deprecated-voice-command-list element %s"
;; 			   x))
;; 		   )
;; 	     )))
;;     (mapcar l (if (eq vr-deprecated-voice-command-list t)
;; 		  vr-deprecated-default-voice-command-list
;; 		vr-deprecated-voice-command-list)))
;; )

(defun vr-deprecated-send-kill-buffer ()
   "Sends a 'kill-buffer message to vr-deprecated.exe"
   (let ()
     (vr-deprecated-send-cmd  (concat "kill-buffer " (buffer-name (current-buffer))))
   )
)

(defun vr-deprecated-send-activate-buffer ()
   "Sends a 'activate-buffer message to vr-deprecated.exe"
   (let ()
     (vr-deprecated-send-cmd  (concat "activate-buffer " (buffer-name (vr-deprecated-buffer))))
   )
)

(defun vr-deprecated-send-deactivate-buffer ()
   "Sends a 'deactivate-buffer message to vr-deprecated.exe"
   (let ()
     (vr-deprecated-send-cmd  (concat "deactivate-buffer " (buffer-name (vr-deprecated-buffer))))
   )
)



(defun vr-deprecated-cmd-initialize (vr-deprecated-request)
  "Function that is called when the vr-deprecated Mode command \"initialize\" is
received. The function receives a single argument, REQ,
which is the list representing the command and its arguments."
  (cond ((eq (nth 1 vr-deprecated-request) 'succeeded)
	 (vr-deprecated-startup))
	((eq (nth 1 vr-deprecated-request) 'no-window)
	 (vcode-mode-toggle-to 0)
	 (message "vr-deprecated process: no window matching %s %s"
		  vr-deprecated-win-class vr-deprecated-win-title))
	(t
	 (vcode-mode-toggle-to 0)
	 (message "vr-deprecated process initialization: %s"
		  (nth 1 vr-deprecated-request))))
  t)

(defun vr-deprecated-cmd-frame-activated (wnd)

  ;; This is ridiculous, but Emacs does not automatically change its
  ;; concept of "selected frame" until you type into it.  So, we have
  ;; the subprocess send us the HWND value and explcitly activate the
  ;; frame that owns it.  The HWND may not belong to any frame, for
  ;; example if vr-deprecated-win-class/title match a Windows window not
  ;; belonging to Emacs.  In that case, just ignore it.
  ;;
  (let* ((frame (car (vr-deprecated-filter
		      (lambda (f) (equal (cdr (assoc 'window-id
						     (frame-parameters f)))
					 wnd))
		      (visible-frame-list)))))

    (vr-deprecated-log "--** vr-deprecated-cmd-alt-frame-activated: init frame: %S\n"
        (selected-frame))
    (vr-deprecated-log "--** vr-deprecated-cmd-alt-frame-activated: init frame handle: %S\n"
        (cdr (assoc 'window-id (frame-parameters (selected-frame)))))
    (vr-deprecated-log "--** vr-deprecated-cmd-alt-frame-activated: init buffer: %S\n"
        (buffer-name (current-buffer)))
    (if frame
	(select-frame frame)
      (message "vr-deprecated Mode: %s is not an Emacs frame window handle; ignored."
	       wnd)))
    (vr-deprecated-log "--** vr-deprecated-cmd-alt-frame-activated: current frame: %S\n"
        (selected-frame))
    (vr-deprecated-log "--** vr-deprecated-cmd-alt-frame-activated: current frame handle: %S\n"
        (cdr (assoc 'window-id (frame-parameters (selected-frame)))))
    (vr-deprecated-log "--** vr-deprecated-cmd-alt-frame-activated: current buffer: %S\n"
        (buffer-name (current-buffer)))
  (vr-deprecated-maybe-activate-buffer (current-buffer))

  t)

(defun vcode-set-special-event-map-for-ignored-key ()
"sets up a special-event-map with a handler for the ignored key, f9

See vcode-cmd-prepare-for-ignored-key.
"
  (vcode-trace "vcode-set-special-event-map-for-ignored-key" "invoked")
  (setq vcode-was-special-event-map (copy-keymap special-event-map))
  (define-key special-event-map [f9] 'vcode-restore-special-event-map)
)

(defun vcode-restore-special-event-map ()
"restores the original value of the special-event-map.

See vcode-cmd-prepare-for-ignored-key.
"
  (interactive)
  (vcode-trace "vcode-restore-special-event-map" 
      "restoring original special event-map")
  (setq special-event-map vcode-was-special-event-map)
  (setq vcode-awaiting-ignored-key nil)
)

(defun vcode-cmd-prepare-for-ignored-key (vcode-request)
"experimental substitute for vr-deprecated-cmd-frame-activated

This is ridiculous, but Emacs does not automatically change its
concept of selected-frame until you type into it.  

The old solution to this, inherited from vr-mode, was to have
the server us the HWND of the foreground window and explcitly activate the
frame that owns it.  The problem with this solution is that it requires
that Emacs's window-id match the Windows HWNd, which won't be the case
if Emacs is running remotely (e.g. via Exceed).

Emacs lisp (Node: Focus Events) claims that a keyboard key or mouse
button should trigger a focus event (consistent with Barry Jaspan's
note above).  However, simulating keyboard input, at least in the way I
tried, doesn't seem to do the trick, because Emacs knows that the
simulated keystrokes didn't really come from a window.  In fact, I
suspect that even if we could insert key events into the event stream at
a lower level, we would have to explicitly include the window-id of the
foreground frame in those events, which brings us back to where we
started -- how do we find the window-id of the foreground frame when
selected-frame doesn't return the correct result?

So, what we're trying now is to have the VoiceCode server simulate
keystrokes (by way of the speech engine) to trigger the focus event.
The only problem with this approach is ensuring that those keystrokes
don't affect the current buffer (even if the minibuffer is active).

To accomplish that, we add this extra Emacs-specific message,
emacs_prepare_for_ignored_key, which temporarily replaces the 
special-event-map.  The replacement keymap binds the ignored key, f9, to
a function which doesn't affect the current buffer.  Strictly speaking,
the ignored key isn't actually ignored, it instead is bound to a
function which restores the original special-event-map.

Because the special-event-map bindings are applied immediately, before
any other keymap bindings, the receipt of the ignored key has no effect on the
current buffer, even if the minibuffer is active.  Fortunately, the
special-event-map doesn't remove the ignored key before triggering a
focus event, which is the desired effect.
"
  (let ((mess-cont (elt vcode-request 1)) 
	(empty-response (make-hash-table :test 'string=))
        )
    (vcode-trace "vcode-cmd-prepare-for-ignored-key" "invoked")
    (vcode-set-special-event-map-for-ignored-key)
; if we receive the emacs_prepare_for_ignored_key message, then the old
; vr-deprecated-cmd-frame-activated hack is redundant
    (setq vcode-frame-activated-necessary nil)
; set this flag so vcode-cmd-recognition-start can tell whether we
; received the ignored key as expected
    (setq vcode-awaiting-ignored-key t)
    (vcode-trace "vcode-cmd-prepare-for-ignored-key" 
      "setting special keymap to intercept ignored key")
    (vr-deprecated-send-reply 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list
      "emacs_prepare_for_ignored_key_resp" empty-response)))
   )
)


(defun vr-deprecated-cmd-heard-command (vr-deprecated-request)
  ;;
  ;; We want to execute the command after this filter function
  ;; terminates, so add the key sequence to invoke it to the end of
  ;; unread-command-events.  Add the key binding, if any, so we don't
  ;; get the "you can run the command <cmd> by typing ..." message.
  ;;
  ;; If the command has arguments, invoke it directly instead.  Also,
  ;; invoke pre-command-hook and post-command-hook so it looks as much
  ;; like a regular command as possible.
  ;;
  ;; Set vr-deprecated-cmd-executing so vr-deprecated-post-command (hook) will inform vr-deprecated.EXE
  ;; when the command is finished.  If cmd is an undefined key
  ;; sequence, no command will be executed, so complete immediately.
  ;;
  (let* ((cmd (nth 1 vr-deprecated-request))
	 (kseq (or (and (vectorp cmd) cmd)
		   (where-is-internal cmd nil 'non-ascii)
		   (concat "\M-x" (symbol-name cmd) "\n"))))
    (setq vr-deprecated-cmd-executing (if (vectorp cmd) (key-binding cmd) cmd))
    (if (not vr-deprecated-cmd-executing)
	(vr-deprecated-send-cmd "command-done undefined"))
    
    (if (not (vectorp cmd))
	(vr-deprecated-execute-command (cdr vr-deprecated-request))
      (vr-deprecated-log "running %s as key sequence:\n" cmd )
      (setq unread-command-events
	    (append unread-command-events
		    (listify-key-sequence kseq)))
      ) 
	)
  t)


(defun vr-deprecated-cmd-mic-state (vr-deprecated-request)
  (let ((state (car (cdr vr-deprecated-request))))
    (cond ((eq state 'off)
	   (setq vcode-mode-mic-state "off"))
	  ((eq state 'on)
	   (setq vcode-mode-mic-state "on"))
	  ((eq state 'sleep)
	   (setq vcode-mode-mic-state "sleep")))
    (vr-deprecated-activate-buffer vr-deprecated-buffer))
  t)


;; This function is called by Dragon when it begins/ends mulling over an
;; utterance; delay key and mouse events until it is done.  This
;; ensures that key and mouse events are not handled out of order
;; with respect to speech recognition events
(defun vr-deprecated-cmd-recognition (vr-deprecated-request)
  (let ((state (nth 1 vr-deprecated-request)))
    (progn
      (vr-deprecated-log "recognition %s: current buffer: %s vr-deprecated-buffer:%s\n"
	      state (buffer-name) vr-deprecated-buffer)
      (cond ((eq state 'begin)
					; if recognition starts and vr-deprecated
					; buffer is not the current
					; buffer, we might have a
					; potential problem with
					; synchronization.  In that
					; case, let's try calling
					; maybe-activate-buffer and
					; see if it's not already too
					; late.
	     (vr-deprecated-maybe-activate-buffer (current-buffer))
	     (run-at-time 0 nil 'vr-deprecated-sleep-while-recognizing)
	     (setq vr-deprecated-recognizing t))
	    ((eq state 'end)
	     (setq vr-deprecated-recognizing nil))
	    (t
	     (error "Unknown recognition state: %s" state)))))

  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Repeat that N times"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-deprecated-last-heard-command-request nil
  "If non-nil, the complete, most-recently-received heard-command
message from vr-deprecated.EXE")

(defun vr-deprecated-repeat-that-hook (vr-deprecated-request)
  (let ((cmd (nth 1 vr-deprecated-request)))
    (if (not (eq cmd 'vr-deprecated-repeat-that))
	(setq vr-deprecated-last-heard-command-request vr-deprecated-request)))
  nil)

(defun vr-deprecated-repeat-that (num)
  (interactive '(1))
  (if vr-deprecated-last-heard-command-request
      (progn
	(while (> num 0)
	  (run-hook-with-args-until-success 'vr-deprecated-cmd-heard-command-hook
					    vr-deprecated-last-heard-command-request)
	  (setq num (1- num))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repeating commands (based on code by Steve Freund).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar vr-deprecated-repeat-rate nil
  "The rate at which to repeat commands, in seconds.  If nil, any
currently repeating command will terminate.")

(defun vr-deprecated-repeat-cmd (freq cmd &rest args)
  "Every FREQ seconds, execute (CMD ARG ...), until the user
generates an input event such as a key press or mouse click (or
executes a voice command that does so).

If the event is RET (the return key), it terminates the repeat but is
then discarded.  Any other event terminates the repeat and is then
acted on as it normally would be."
  (let (ev)
    (discard-input)
    (setq vr-deprecated-repeat-rate freq)
    (while vr-deprecated-repeat-rate
      (apply cmd args)
      (sit-for vr-deprecated-repeat-rate)
      (if (input-pending-p)
	  (progn
	    (setq ev (read-event))
	    (setq vr-deprecated-repeat-rate nil))))
    (if (and ev (not (eq ev 'return)))
	(setq unread-command-events
	      (cons ev unread-command-events)))
    ))

(defun vr-deprecated-repeat-mult-rate (f)
  "Multiply the number of seconds between each execution of the current
repeating command by FACTOR."
  (setq vr-deprecated-repeat-rate (* vr-deprecated-repeat-rate f)))

(defun vr-deprecated-repeat-stop (d)
  "Terminate the current repeating command."
  (setq vr-deprecated-repeat-rate nil))

(defmacro vr-deprecated-make-repeat-cmd (name freq cmd &rest args)
  "Define an interactive repeating command called NAME that takes no
arguments and, every FREQ seconds, invokes the function CMD.  Uses
vr-deprecated-repeat-cmd."
  (let ((vrc 'vr-deprecated-repeat-cmd))
    (list 'defun name '()
	  (format "Invoke %s every %s seconds,\nusing vr-deprecated-repeat-cmd (which see)."
		  cmd freq)
	  '(interactive)
	  (list 'apply (list 'quote vrc) freq (list 'quote cmd)
		(list 'quote args)))))

(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-move-up-s 0.25 previous-line 1)
(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-move-up-f 0.05 previous-line 1)
(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-move-down-s 0.25 next-line 1)
(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-move-down-f 0.05 next-line 1)

(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-move-left-s 0.25 backward-char 1)
(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-move-left-f 0.05 backward-char 1)
(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-move-right-s 0.25 forward-char 1)
(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-move-right-f 0.05 forward-char 1)

(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-move-word-left-s 0.25 backward-word 1)
(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-move-word-left-f 0.05 backward-word 1)
(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-move-word-right-s 0.5 forward-word 1)
(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-move-word-right-f 0.05 forward-word 1)

(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-search-forward-s 0.75 isearch-repeat-forward)
(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-search-forward-f 0.25 isearch-repeat-forward)
(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-search-backward-s 0.75 isearch-repeat-backward)
(vr-deprecated-make-repeat-cmd vr-deprecated-repeat-search-backward-f 0.25 isearch-repeat-backward)

(defun vr-deprecated-repeat-kill-line (freq)
  "Invoke kill-line every FREQ seconds, using vr-deprecated-repeat-cmd (which see).
The lines killed with this command form a single block in the yank buffer."
  (kill-new "") 
  (vr-deprecated-repeat-cmd freq (function (lambda () (kill-line) (append-next-kill)))))

(defun vr-deprecated-repeat-yank (freq arg)
  "Perform a yank from the kill ring every FREQ seconds, using
vr-deprecated-repeat-cmd (which see).  This function cycles through the yank
buffer, doing the right thing regardless of whether the previous
command was a yank or not."
  (interactive (list 0.5 (prefix-numeric-value prefix-arg)))
  (vr-deprecated-repeat-cmd
   freq (function (lambda ()
		    (if (or (eq last-command 'yank) (eq this-command 'yank))
			(yank-pop arg)
		      (yank arg)
		      (setq last-command 'yank))
		    (undo-boundary)
		    ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration allowing vr-mode to interact with VoiceCode speech 
;;; server.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (append load-path (list (substitute-in-file-name "$VCODE_HOME/Environments/Emacs"))))
(load "messaging")

(defvar vcode-app-id nil 
"Unique ID assigned to this instance of Emacs by the VoiceCode server.")

(defvar vcode-instance-string nil
"Another unique ID assigned to this instance of Emacs."
)

(defvar vcode-is-test-editor nil
"Are we connected to VoiceCode to perform regression tests?"
)

(defvar vcode-test-file-name-was nil
"What was the visited-file-name corresponding to this buffer, before we set it
to nil?"
)
(make-variable-buffer-local 'vcode-test-file-name-was)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables used by the ignored key alternative to
;; vr-deprecated-cmd-frame-activated.
;;
;; See vcode-cmd-prepare-for-ignored-key for more details.

(defvar vcode-frame-activated-necessary t
"flag indicating whether vcode-cmd-recognition-start needs to call
vr-deprecated-cmd-frame-activated to ensure that selected-frame (and thus
current-buffer, etc.) return the correct value.

Currently, this is initially set to true, but will be cleared if we
receive an emacs_prepare_for_ignored_key message.  This allows us to
switch on the experimental ignored-key substitute for
vr-deprecated-cmd-frame-activated based on whether the VoiceCode server is using
it, rather than having to manually configure both VoiceCode and
vcode-mode to use matching settings.

If the experimental test works, vr-deprecated-cmd-frame-activated and this flag
will ultimately be removed.

See vcode-cmd-prepare-for-ignored-key for more details.
"
)

(defvar vcode-awaiting-ignored-key nil
"flag indicating that we have received the emacs_prepare_for_ignored_key
message, but have not yet received the ignored key

See vcode-cmd-prepare-for-ignored-key for more details.
"
)

(defvar vcode-was-special-event-map nil
"holds a backup copy of the special-event-map keymap, for restoration
following receipt of the ignored key.  

See vcode-cmd-prepare-for-ignored-key for more details.
"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vcode-make-all-keys-self-insert ()
  
)

(defun vr-deprecated-mode-configure-for-vcode-server ()
  "Configures vr-deprecated Mode for interacting with the VoiceCode speech server."

  ;;;
  ;;; VCode will do automatic indentation and stuff.
  ;;;
  (vcode-make-all-keys-self-insert)

  (setq vr-deprecated-dont-report-sr-own-changes nil)

  ;;;
  ;;; Hook function that waits for Emacs to handshake with first 
  ;;; socket connection before connecting a second time.
  ;;;
  (setq vcode-app-id nil)
  (setq vr-deprecated-wait-for-handshake-hook 'vcode-wait-for-handshake)

  ;;;
  ;;; Hook functions for parsing/generating messages to/from VCode server
  ;;;
  (setq vr-deprecated-deserialize-message-hook 'vcode-deserialize-message)
  (setq vr-deprecated-serialize-message-hook 'vcode-serialize-message)
  (setq vr-deprecated-serialize-changes-hook 'vcode-serialize-changes)

  ;;;
  ;;; Hook function for starting the VCode server
  ;;;
  (setq vr-deprecated-command (substitute-in-file-name "%VCODE_HOME%/Mediator/vcode.bat"))
  
  ;;; Functions for sending messages to vr-deprecated.exe
  (setq vr-deprecated-send-kill-buffer-hook 'vcode-send-kill-buffer)
  (setq vr-deprecated-send-activate-buffer-hook 'vcode-send-activate-buffer)
  (setq vr-deprecated-send-deactivate-buffer-hook 'vcode-send-deactivate-buffer)
  (add-hook 'kill-buffer-hook 'vr-deprecated-kill-buffer)
  (add-hook 'suspend-hook 'vcode-send-suspended)
  (add-hook 'suspend-resume-hook 'vcode-send-resuming)

  ;;; Function for handling errors in execution of commands received from 
  ;;; vr-deprecated.exe.
  (setq vr-deprecated-upon-cmd-error 'vcode-send-cmd-error-message)


  ;;;
  ;;; Hook functions for handling messages received from VCode
  ;;; 
  (cl-clrhash vr-deprecated-message-handler-hooks)

  ;;;
  ;;; These messages are sent by VCode during handshake part of the 
  ;;; protocol
  ;;;
  (cl-puthash 'send_app_name 'vcode-cmd-send-app-name vr-deprecated-message-handler-hooks)
  (cl-puthash 'your_id_is 'vcode-cmd-your-id-is vr-deprecated-message-handler-hooks)
  (cl-puthash 'send_id 'vcode-cmd-send-app-id vr-deprecated-message-handler-hooks)
  (cl-puthash 'terminating 'vr-deprecated-cmd-terminating vr-deprecated-message-handler-hooks)
  (cl-puthash 'test_client_query 'vcode-cmd-test-client-query vr-deprecated-message-handler-hooks)

  ;;; These messages are sent once by VCode immediately after the 
  ;;; handshake part of the protocol
  ;;;
  (cl-puthash 'set_instance_string 'vcode-cmd-set-instance-string vr-deprecated-message-handler-hooks)
; DCF: obsolete message 
;  (cl-puthash 'instance_string 'vcode-cmd-get-instance-string vr-deprecated-message-handler-hooks)
  (cl-puthash 'suspendable 'vcode-cmd-suspendable 
	      vr-deprecated-message-handler-hooks)
  (cl-puthash 'suspend_notification 'vcode-cmd-suspend-notification 
	      vr-deprecated-message-handler-hooks)
  (cl-puthash 'shared_window 'vcode-cmd-shared-window
	      vr-deprecated-message-handler-hooks)
  (cl-puthash 'multiple_windows 'vcode-cmd-multiple-windows
	      vr-deprecated-message-handler-hooks)


  (cl-puthash 'emacs_prepare_for_ignored_key 'vcode-cmd-prepare-for-ignored-key
	      vr-deprecated-message-handler-hooks)
  (cl-puthash 'recog_begin 'vcode-cmd-recognition-start 
	      vr-deprecated-message-handler-hooks)
  (cl-puthash 'recog_end 'vcode-cmd-recognition-end 
	      vr-deprecated-message-handler-hooks)
  (cl-puthash 'active_buffer_name 'vcode-cmd-active-buffer-name 
	      vr-deprecated-message-handler-hooks)
  (cl-puthash 'open_file 'vcode-cmd-open-file vr-deprecated-message-handler-hooks)
  (cl-puthash 'updates 'vcode-cmd-updates vr-deprecated-message-handler-hooks)
  (cl-puthash 'confirm_buffer_exists 'vcode-cmd-confirm-buffer-exists vr-deprecated-message-handler-hooks)
  (cl-puthash 'list_open_buffers 'vcode-cmd-list-open-buffers vr-deprecated-message-handler-hooks)
  (cl-puthash 'list_all_buffers 'vcode-cmd-list-all-buffers vr-deprecated-message-handler-hooks)
  (cl-puthash 'close_buffer 'vcode-cmd-close-buffer vr-deprecated-message-handler-hooks)
  (cl-puthash 'file_name 'vcode-cmd-file-name vr-deprecated-message-handler-hooks)
  (cl-puthash 'language_name 'vcode-cmd-language-name vr-deprecated-message-handler-hooks)
  (cl-puthash 'file_language_name 'vcode-cmd-file-language-name vr-deprecated-message-handler-hooks)
  (cl-puthash 'line_num_of 'vcode-cmd-line-num-of vr-deprecated-message-handler-hooks)
  (cl-puthash 'cur_pos 'vcode-cmd-cur-pos vr-deprecated-message-handler-hooks)
  (cl-puthash 'get_selection 'vcode-cmd-get-selection vr-deprecated-message-handler-hooks)
  (cl-puthash 'get_pos_selection 'vcode-cmd-get-pos-selection vr-deprecated-message-handler-hooks)
  (cl-puthash 'get_text 'vcode-cmd-get-text vr-deprecated-message-handler-hooks)
  (cl-puthash 'get_visible 'vcode-cmd-get-visible vr-deprecated-message-handler-hooks)
  (cl-puthash 'len 'vcode-cmd-len vr-deprecated-message-handler-hooks)
  (cl-puthash 'newline_conventions 'vcode-cmd-newline-conventions vr-deprecated-message-handler-hooks)
  (cl-puthash 'pref_newline_convention 'vcode-cmd-pref-newline-conventions vr-deprecated-message-handler-hooks)


  ;;;
  ;;; These messages are used by VCode to change the content of the buffer
  ;;;
  (cl-puthash 'set_selection 'vcode-cmd-set-selection vr-deprecated-message-handler-hooks)
;;; Is this needed?
;;;  (cl-puthash 'make_position_visible 'vcode-cmd-make-position-visible vr-deprecated-message-handler-hooks)
  (cl-puthash 'move_relative_page 'vcode-cmd-move-relative-page vr-deprecated-message-handler-hooks)  
  (cl-puthash 'insert 'vcode-cmd-insert vr-deprecated-message-handler-hooks)  
  (cl-puthash 'insert_indent 'vcode-cmd-insert-indent vr-deprecated-message-handler-hooks)  
  (cl-puthash 'set_text 'vcode-cmd-set-text vr-deprecated-message-handler-hooks)  
  (cl-puthash 'indent 'vcode-cmd-indent vr-deprecated-message-handler-hooks)  
  (cl-puthash 'decr_indent_level 'vcode-cmd-decr-indent-level vr-deprecated-message-handler-hooks)
  (cl-puthash 'incr_indent_level 'vcode-cmd-incr-indent-level vr-deprecated-message-handler-hooks)
  (cl-puthash 'delete 'vcode-cmd-delete vr-deprecated-message-handler-hooks)  
  (cl-puthash 'copy_selection 'vcode-cmd-copy-selection vr-deprecated-message-handler-hooks)  
  (cl-puthash 'cut_selection 'vcode-cmd-cut-selection vr-deprecated-message-handler-hooks)  
  (cl-puthash 'paste 'vcode-cmd-paste vr-deprecated-message-handler-hooks)  
  (cl-puthash 'backspace 'vcode-cmd-backspace vr-deprecated-message-handler-hooks)  
  (cl-puthash 'goto 'vcode-cmd-goto vr-deprecated-message-handler-hooks)  
  (cl-puthash 'goto_line 'vcode-cmd-goto-line vr-deprecated-message-handler-hooks)  
  (cl-puthash 'end_of_line 'vcode-cmd-end-of-line vr-deprecated-message-handler-hooks)  
  (cl-puthash 'beginning_of_line 'vcode-cmd-beginning-of-line vr-deprecated-message-handler-hooks)  
  (cl-puthash 'mediator_closing 'vcode-cmd-mediator-closing vr-deprecated-message-handler-hooks)  

  ;;; 
  ;;; These messages are used by VCode to control Emacs (e.g. switch buffer)
  ;;;
  (cl-puthash 'find_matching 'vcode-cmd-find-matching vr-deprecated-message-handler-hooks)  
  (cl-puthash 'beginning_of_statement 'vcode-cmd-beginning-of-statement vr-deprecated-message-handler-hooks)  
  (cl-puthash 'change_buff 'vcode-cmd-change-buff vr-deprecated-message-handler-hooks)  

  ;;;
  ;;; These ones are currently not handled by VCode, but they probably should
  ;;;
;  (cl-puthash 'heard-command 'vr-deprecated-cmd-heard-command-hook vr-deprecated-message-handler-hooks)
;  (cl-puthash 'mic-state 'vr-deprecated-cmd-mic-state-hook vr-deprecated-message-handler-hooks)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; These messages are defined in vr-deprecated.exe but don't seem useful for VCode.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; In vr-deprecated.exe, this message is sent only to confirm
;;; that there is indeed a window with the ID, title or class that Emacs
;;; sent upon connection. But VCode doesn't expect Emacs to tell it about
;;; its window (it decides that for itself by looking at what is the active
;;; window)
;  (cl-puthash 'connected 'vcode-cmd-connected-hook vr-deprecated-message-handler-hooks)

;;; See comment about 'vcode-cmd-connected-hook
;  (cl-puthash 'initialize 'vr-deprecated-cmd-initialize-hook vr-deprecated-message-handler-hooks)

;;; VCode will tell Emacs that a frame is activated when
;;; it sends a recog_begin message
;  (cl-puthash 'frame-activated 'vr-deprecated-cmd-frame-activated-hook vr-deprecated-message-handler-hooks)


;;; Not needed in VCode. VCode will send different messages to make different
;;; kinds of changes.
;  (cl-puthash 'make-changes 'vr-deprecated-cmd-make-changes vr-deprecated-message-handler-hooks)

)


(defun vcode-serialize-message (mess)
  "Serializes a LISP data structure into a string message that can be sent to 
VoiceCode server."
  (let ((mess-name (elt mess 0)) (mess-cont (elt mess 1)))
    (vcode-trace "vcode-serialize-message" "mess-name=%S, mess-cont=%S" mess-name mess-cont)
    (setq serialized-mess (vcode-pack-mess (vcode-encode-mess mess-name mess-cont)))
    serialized-mess
  )
)

(defun vcode-deserialize-message (mess)
  "Deserializes a string message received from the VoiceCode server 
into a LISP data structure."

  (vcode-trace "vcode-deserialize-message" "mess=%S" mess)

  (let ((unpack-result) (unpacked-mess) (bytes-parsed) 
	(mess-name) (mess-cont))

    ;;;
    ;;; Unpack the message
    ;;;
    (setq unpack-result (vcode-unpack-mess mess))
    (setq unpacked-mess (elt unpack-result 0))
    (setq bytes-parsed (elt unpack-result 1))

    (vcode-trace "vcode-deserialize-message" "unpacked-mess=%S" unpacked-mess)

    ;;;
    ;;; Then decode it
    ;;; 
    (setq mess (vcode-decode-mess unpacked-mess ))
    (setq mess-name (elt mess 0))
    (setq mess-cont (elt mess 1))
    (vcode-trace "vcode-deserialize-message" "mess-name=%S, mess-cont=%S" mess-name mess-cont)
    (list (list mess-name mess-cont) bytes-parsed)
  )
)

(defun vcode-send-cmd-error-message (vcode-req)

  "Sends error message an error message to VCode. The error was
generated while executing VCode request 'vcode-req."

;   (let ((mess-name (concat (elt 0 vcode-req) "_resp"))
;        (mess-cont make-hash-table :test 'string=))
;     (cl-sethash "error" 1 mess-cont)
;     (vr-deprecated-send-reply 
;      (run-hook-with-args 
;       'vr-deprecated-serialize-message-hook (list mess-name mess-cont)))
;     )

)

(defun vcode-get-buff-name-from-message (message)
  "Returns name of \"buff_name\" element in 'message. If absent or nil, then
returns name of current buffer."
  (let ((buff-name (cl-gethash "buff_name" message nil)))
    (if (not buff-name)
	(setq buff-name (buffer-name (current-buffer)))
    )
    buff-name
  )
)

(defun vcode-cmd-send-app-name (vcode-req)
   "Sends the name of the application ('Emacs) to the VoiceCode server."


   (let ((mess-cont (make-hash-table :test 'string=)))
     (cl-puthash "value" "emacs" mess-cont)
     (vr-deprecated-send-cmd (run-hook-with-args 'vr-deprecated-serialize-message-hook (list "app_name" mess-cont)))
  )

)


(defun vcode-cmd-test-client-query (vcode-req)
   "Sends the name of the application ('Emacs) to the VoiceCode server."


   (let ((mess-cont (make-hash-table :test 'string=)))
     (cl-puthash "value" vr-deprecated-vcode-test-client mess-cont)
     (vr-deprecated-send-cmd (run-hook-with-args 'vr-deprecated-serialize-message-hook (list "test_client_query_resp" mess-cont)))
  )

)

(defun vcode-cmd-set-instance-string (vcode-req)
   (let ((resp-cont (make-hash-table :test 'string=)))

   (setq vcode-instance-string (cl-gethash "instance_string" (nth 1 vcode-req)))

; emacs can't set the window title when running from a shell window
   (if (eq window-system nil)
     (cl-puthash "value" 0 resp-cont)
     (cl-puthash "value" 1 resp-cont)	 
   )

   (setq vcode-previous-frame-title-format frame-title-format)
   (setq frame-title-format 
          `("%b -- " (,vcode-instance-string " " invocation-name "@" system-name)))

;   (setq frame-title-format 
;	 `(multiple-frames "%b" (,vcode-instance-string invocation-name "@" system-name)))

     (vr-deprecated-send-reply (run-hook-with-args 
		   'vr-deprecated-serialize-message-hook (list "set_instance_string_resp" resp-cont)))
  )

)

(defun vcode-cmd-get-instance-string (vcode-req)
   (let (resp-cont (make-hash-table :test 'string=))
     (cl-puthash "value"  vcode-instance-string resp-cont)
     (vr-deprecated-send-reply (run-hook-with-args 
		   'vr-deprecated-serialize-message-hook (list "get_instance_string_resp" resp-cont)))
  )

)

(defun vcode-cmd-your-id-is (vcode-req)
   "Stores the unique ID assigned by VoiceCode to this instance of Emacs, so 
we can send it back to VoiceCode when we ask it for a second network connection."

   (let ((mess-name (elt vcode-req 0)) (mess-cont (elt vcode-req 1))
;	 (ok-mess-cont (make-hash-table :key 'string=)))
	 (ok-mess-cont (make-hash-table :test 'string=)))


     (vr-deprecated-send-cmd (run-hook-with-args 'vr-deprecated-serialize-message-hook (list "ok" ok-mess-cont)))

     ;;;
     ;;; Wait til the very end before setting 'vcode-app-id, because that will
     ;;; cause 'vcode-wait-for-handshake to exit, which will in turn
     ;;; start the second network connection (and we don't want the second
     ;;; connection to start before VCode server has received the 'ok message
     ;;; and reacted to it).
     ;;;
     (setq vcode-app-id (cl-gethash "value" mess-cont))
   )
)

(defun vcode-cmd-send-app-id (vcode-req)

   "Sends the unique ID received from VCode server (when opened first
network connection to it) back to the VCode server.

This allows the VCode server to know for sure that the second network
connection originates from the same Emacs instance as the first one."


   (let ((mess-cont (make-hash-table :test 'string=)))

     ;;; Then send that ID back to the server
     (cl-puthash "value" vcode-app-id mess-cont)
     (vr-deprecated-send-reply (run-hook-with-args 'vr-deprecated-serialize-message-hook (list "my_id_is" mess-cont)))


     ;;;
     ;;; Invoke 'vr-deprecated-startup explicitly here because VCode server never sends an
     ;;; 'inialize message.
     ;;;
     (vr-deprecated-startup)

   )
)

(defun vcode-wait-for-handshake ()
  "This function waits until Emacs has shaken hands with VoiceCode server
on the first socket connection. We know the handshake has happened when
Emacs has set 'vcode-app-id to a non nil value."

  (while (not vcode-app-id)
    (progn
      (sleep-for 0.1)
      )
    )
)

(defun vcode-send-kill-buffer ()
   "Sends a message to VCode server to tell it that Emacs has closed a buffer"
   (let ((mess-cont (make-hash-table :test 'string=)))
     (cl-puthash "buff_name" (buffer-name) mess-cont)
     (cl-puthash "action" "close_buff" mess-cont)
; DCF - try this instead
     (vr-deprecated-send-cmd
       (vcode-serialize-updates (list mess-cont)))
       
; DCF - AppStateMessaging is not expecting this format
;     (vr-deprecated-send-cmd (list 'updates mess-cont))
;       (run-hook-with-args 
;	 'vr-deprecated-serialize-message-hook (list "updates" mess-cont)))
   )
)

(defun vcode-send-activate-buffer ()
   "Sends a message to VCode server to tell it that Emacs has voice activated
a buffer"
   (let ((mess-cont (make-hash-table :test 'string=)))
     (cl-puthash "buff_name" (buffer-name) mess-cont)
     (cl-puthash "action" "curr_buffer" mess-cont)
     (vr-deprecated-send-cmd (list 'updates mess-cont))
   )
)

(defun vcode-send-deactivate-buffer ()
   "Sends a message to VCode server to tell it that Emacs has voice deactivated
a buffer"
   (let ((mess-cont (make-hash-table :test 'string=)))
     (cl-puthash "buff_name" (buffer-name) mess-cont)
     (cl-puthash "action" "close_buff" mess-cont)
     (vr-deprecated-send-cmd (list 'updates mess-cont))
   )
)


(defun vcode-cmd-suspendable (vcode-request)
   (let ((resp-cont (make-hash-table :test 'string=)))

     (if (string= "w32" window-system)
	 (cl-puthash "value" 0 resp-cont)
       (cl-puthash "value" 1 resp-cont)	 
     )
    (vr-deprecated-send-reply 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "suspendable_resp" resp-cont)))
   )
)

(defun vcode-cmd-suspend-notification (vcode-request)
   (let ((resp-cont (make-hash-table :test 'string=)))

     (if (string= "w32" window-system)
	 (cl-puthash "value" 0 resp-cont)
       (cl-puthash "value" 1 resp-cont)	 
     )
    (vr-deprecated-send-reply 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "suspend_notification_resp" resp-cont)))
   )
)

(defun vcode-cmd-shared-window (vcode-request)
   (let ((resp-cont (make-hash-table :test 'string=)))

; when running from the shell, Emacs could be suspended or killed and
; the same window could belong to another application
     (if (eq window-system nil)
	 (cl-puthash "value" 1 resp-cont)
       (cl-puthash "value" 0 resp-cont)	 
     )
    (vr-deprecated-send-reply 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "shared_window_resp" resp-cont)))
   )
)

(defun vcode-cmd-multiple-windows (vcode-request)
   (let ((resp-cont (make-hash-table :test 'string=)))

     ; unless we're running from the shell, we can have multiple windows
     ; (frames, in Emacs terminology)
     (if (eq window-system nil)
	 (cl-puthash "value" 0 resp-cont)
       (cl-puthash "value" 1 resp-cont)	 
     )
    (vr-deprecated-send-reply 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "multiple_windows_resp" resp-cont)))
   )
)


(defun vcode-cmd-recognition-start (vcode-request)
  (let ((mess-cont (elt vcode-request 1)) 
	(resp-cont (make-hash-table :test 'string=))
        (window-id nil))

    ;;;
    ;;; Tell Emacs what the active window was when we heard the utterance
    ;;;
    (setq window-id (cl-gethash 'window_id mess-cont))
    (vcode-trace "vcode-cmd-recognition-start" "(type-of window-id)=%S,
    window-id=%S" (type-of window-id) window-id)
    (vcode-trace "vcode-cmd-recognition-start" "minibuffer active? %S"
      (if (equal (active-minibuffer-window) nil) "no" "yes"))
; this is always returning "no"
    (vcode-trace "vcode-cmd-recognition-start" "minibuffer depth = %S"
      (minibuffer-depth))
; this is always returning 0
    (vcode-trace "vcode-cmd-recognition-start" 
      "selected-window is minibuffer? %S"
      (if (window-minibuffer-p (selected-window)) "yes" "no")
    )
    (vr-deprecated-log "-- vcode-cmd-recognition-start: input-pending-p is %S" 
      (input-pending-p))
    (if (numberp window-id)
        (setq window-id (int-to-string window-id)))
    (vr-deprecated-log "--** vcode-cmd-recognition-start: window id is %S\n" window-id)


    (if vcode-awaiting-ignored-key
        (progn
        ; we received the emacs_prepare_for_ignored_key message, but
        ; failed to receive the expected ignored key before the
        ; recog_begin message.  
          (vcode-warning "ignored key expected, but not received")
;; Don't do this, in case the user has mapped f9 to some function.
;; The f9 key will eventually make its way to Emacs and emacs will then
;; restore the key mapping to what it was.
;;          (vcode-restore-special-event-map)
          (setq vcode-frame-activated-necessary t)
        )
    )

    (if vcode-frame-activated-necessary
; tried this just to confirm that keyboard macros don't trigger focus
; events (as per the Info file).  They don't, so we still need the
; mediator to send the ignored key.  -- DCF
;      (vcode-set-special-event-map-for-ignored-key)
;      (execute-kbd-macro [f9])
      (vr-deprecated-cmd-frame-activated window-id)
    )

; trying my alternative form here
;    (vr-deprecated-cmd-alt-frame-activated window-id)
; DCF: I don't understand why list nil was used here.  The comparison
; was with the window-id as a string.
;    (vr-deprecated-cmd-frame-activated (list nil (cl-gethash 'window_id mess-cont)))

    ;;;
    ;;; Check if current buffer is speech enabled
    ;;;
    (if (vr-deprecated-activate-buffer-p (current-buffer))
      (progn
	
        ;;;
        ;;; Reformulate the request received from VCode, into a request 
        ;;; that can be processed by the standard vr-deprecated Mode function
        ;;;
	(vr-deprecated-cmd-recognition (list nil 'begin))
	(cl-puthash "value" 1 resp-cont)
	)
      (cl-puthash "value" 0 resp-cont)
      )
    (vr-deprecated-log "active, current buffer name is %d, %S" (cl-gethash "value"
    resp-cont)(buffer-name (current-buffer)))


    (vr-deprecated-send-reply 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "recog_begin_resp" resp-cont)))
    )    
  )


(defun vcode-cmd-recognition-end (vcode-request)
  (let ((empty-resp (make-hash-table :test 'string=)))
    ;;;
    ;;; Reformulate the request received from VCode, into a request 
    ;;; that can be processed by the standard vr-deprecated Mode function
    ;;;
    (vr-deprecated-cmd-recognition (list nil 'end))
    
    (vr-deprecated-send-reply 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "recog_end_resp" empty-resp)))
    )

  )


(defun vcode-cmd-active-buffer-name (vcode-request)
  (let ((mess-cont (make-hash-table :test 'string=)))
    (cl-puthash "value" (buffer-name) mess-cont)
    (vr-deprecated-send-reply (run-hook-with-args 'vr-deprecated-serialize-message-hook (list "active_buffer_name_resp" mess-cont)))
    )
  )


;;;
;;; Generate a hash table describing an insertion or deletion change
;;;
(defun vcode-generate-insert-change-hash (a-change)
  (let ((buff-name (nth 0 a-change))
	(deleted-start (nth 1 a-change))
	(deleted-end (nth 2 a-change))
	(inserted-text (nth 3 a-change))
	(insert-change-hash (make-hash-table :test 'string=)))

    (cl-puthash "action" "insert" insert-change-hash)
    (cl-puthash "range" 
		(list (vcode-convert-pos deleted-start 'vcode)
		      (vcode-convert-pos deleted-end 'vcode))
		insert-change-hash)
    (cl-puthash "buff_name" buff-name insert-change-hash)
    (cl-puthash "text" inserted-text insert-change-hash)

    insert-change-hash
  )
)

;;;
;;; Generate a hash table describing a buffer contents change
;;;
(defun vcode-generate-contents-change-hash (a-change)
  (let ((buff-name (nth 0 a-change))
	(text (nth 1 a-change))
	(contents-change-hash (make-hash-table :test 'string=)))

    (cl-puthash "action" "buff_contents" contents-change-hash)
    (cl-puthash "buff_name" buff-name contents-change-hash)
    (cl-puthash "text" text contents-change-hash)

    contents-change-hash
  )
)

; obsolete - replaced with vcode-pos-select-change-hash
(defun vcode-generate-select-change-hash (a-change)
  (let ((buff-name (nth 0 a-change))
	(sel-start (nth 1 a-change))
	(sel-end (nth 2 a-change))
	(select-change-hash (make-hash-table :test 'string=))
	(range nil))
    (cl-puthash "action" "select" select-change-hash)
    (setq range (vcode-convert-range sel-start sel-end 'vcode))
    (cl-puthash "range" range select-change-hash)
    (cl-puthash "cursor_at" 1 select-change-hash)
    (cl-puthash "buff_name" buff-name select-change-hash)
    select-change-hash
  )
)

(defun vcode-generate-pos-select-change-hash (a-change)
  (let ((buff-name (nth 0 a-change))
	(sel-start (nth 1 a-change))
	(sel-end (nth 2 a-change))
	(select-change-hash (make-hash-table :test 'string=))
	(range nil))
    (cl-puthash "action" "pos_selection" select-change-hash)
    (setq range (vcode-convert-range 
      (min sel-start sel-end) (max sel-start sel-end) 'vcode))
    (cl-puthash "selection" range select-change-hash)
    (cl-puthash "pos" (vcode-convert-pos sel-end 'vcode) select-change-hash)
    (cl-puthash "buff_name" buff-name select-change-hash)
    select-change-hash
  )
)


(defun vcode-generate-change-hash (a-change)

    (vcode-trace "vcode-generate-change-hash" "a-change=%S" a-change)
    (if (eq (nth 0 a-change) 'change-is-insert)
	(setq a-change-hash (vcode-generate-insert-change-hash (nth 1 a-change)))
        (if (eq (nth 0 a-change) 'change-is-contents)
           (setq a-change-hash (vcode-generate-contents-change-hash (nth
1 a-change)))
      (setq a-change-hash (vcode-generate-pos-select-change-hash (nth 1 a-change)))
        )
    )
    a-change-hash
)

(defun vcode-serialize-updates (update-hash-list)
  "Creates an Emacs-initiated updates message to be sent to VCode 
  speech server.

Argument 'change-list is a list of hashes representing updates to be
sent.

"

  (let ((mess-name "updates") (mess-key "value") 
        (mess-cont (make-hash-table :test 'string=)))
    (cl-puthash mess-key update-hash-list mess-cont)

    (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list mess-name mess-cont)))
)

(defun vcode-serialize-changes (change-list)
  "Creates a change notification message to be sent to VCode speech server.

Argument 'change-list is a list of 5ple:
   (change-type buffer-name inserted-start inserted-end deleted-length).

If 'vr-deprecated-changes-caused-by-sr-cmd is not nil, then the message must be 
formatted as a response message to SR command 'vr-deprecated-changes-caused-by-sr-cmd.

Otherwise, the message must be formated as an Emacs initiated 'updates' 
message.
"

  (let ((mess "") (buff-name) (a-change) (inserted-start) (inserted-end) 
	(deleted-length) (inserted-text)
	(change-list-vcode (list)) (a-change-vcode) (a-change-action)
	(deleted-end)
	(mess-name) (mess-key "updates") (mess-cont (make-hash-table :test 'string=)))

    (vcode-trace "vcode-serialize-changes" "change-list=%S" change-list)

    (while change-list
      (setq a-change (car change-list))
      (setq change-list (cdr change-list))
      (vcode-trace "vcode-serialize-changes" "after popping a change, a-change=%S, change-list=%S" a-change change-list)

      ;;; Generate a hashes describing this change and append it to the 
      ;;; change list destined for VCode
      (setq a-change-vcode (vcode-generate-change-hash a-change))

      (setq change-list-vcode 
	    (append change-list-vcode (list a-change-vcode)))
      (vcode-trace "vcode-serialize-changes" "a-change-vcode=%S, change-list-vcode=%S" a-change-vcode change-list-vcode)

    )

    
    ;;; Name the message that will be sent to VCode.
    ;;; Changes generated in response to VCode request "some_command"
    ;;; -> name = "some_command_resp"
    ;;;
    ;;; Changes not generated in response to a VCode request:
    ;;; -> name = "updates"
    (if vr-deprecated-changes-caused-by-sr-cmd
	(if (string= vr-deprecated-changes-caused-by-sr-cmd "updates")
	    (setq mess-name "updates")
	    (setq mess-name (format "%s_resp" vr-deprecated-changes-caused-by-sr-cmd))
	)
	(setq mess-name "updates")
      )
    (if (string= mess-name "updates")
	(setq mess-key "value")
    )
    (cl-puthash mess-key change-list-vcode mess-cont)

    (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list mess-name mess-cont)))
)

; most changes sent as they occur, but there is no hook for position and
; selection changes, so we need to send them when the updates message
; requests them
(defun vcode-cmd-updates (vcode-request)
  (vcode-report-goto-select-change 
    (buffer-name (current-buffer)) 
    (if mark-active (mark) (point))
    (point)
  )
  (vcode-send-queued-changes) 
)

(defun vcode-matching-test-file-name (new-file-name buffers)
    (let ((a-buff) (found-buff nil))
        (save-excursion
            (dolist (a-buff buffers)
                    (set-buffer a-buff)
                    (if (equal vcode-test-file-name-was new-file-name)
                        (setq found-buff (buffer-name a-buff))
                    )
            )
        )
        found-buff
    )
)


(defun vcode-cmd-open-file (vcode-request)
  (let ((mess-cont (elt vcode-request 1)) 
	(file-name)
	(emacs-file-name)
        (a-buff)
        (old-buff-list)
        (found-buff)
	(response (make-hash-table :test 'string=)))
    (setq file-name (cl-gethash "file_name" mess-cont))
    (vr-deprecated-log "--** vcode-cmd-open-file: python-mode-hook =%S file-name=%S\n" file-name python-mode-hook )
    (setq old-buff-list (buffer-list))
    (find-file file-name)
    (setq emacs-file-name (buffer-file-name))
    (setq found-buff nil)
    (if vcode-is-test-editor 
        (setq found-buff (vcode-matching-test-file-name emacs-file-name
            old-buff-list))
    )
    (if found-buff
        (progn
            (kill-buffer (current-buffer))
            (switch-to-buffer found-buff)
        )
        (setq vcode-test-file-name-was (buffer-file-name (current-buffer)))
        (set-visited-file-name nil)
    )
    (cl-puthash "buff_name" (buffer-name) response)
    (vr-deprecated-send-reply
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "open_file_resp" response))
    )
  )
)


(defun vcode-cmd-confirm-buffer-exists (vcode-request)
  (let ((mess-cont (elt vcode-request 1)) 
	(buffer-name)
	(response (make-hash-table :test 'string=)))

    (setq buffer-name (vcode-get-buff-name-from-message mess-cont))
    (if (get-buffer buffer-name)
	(cl-puthash "value" 1 response)
      (cl-puthash "value" 0 response))
    (vr-deprecated-send-reply
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "confirm_buffer_exists_resp" response))
    )
  )
)


(defun vcode-cmd-list-open-buffers (vcode-request)
  (let ((mess-cont (elt vcode-request 1)) 
	(open-buffers (buffer-list))
	(buffer-names nil)
	(response (make-hash-table :test 'string=)))

    (while open-buffers
      (if (vr-deprecated-activate-buffer-p (car open-buffers))
	  (setq buffer-names (append buffer-names (list (buffer-name (car open-buffers)))))
      )
      (setq open-buffers (cdr open-buffers))
      )

    (cl-puthash "value" buffer-names response)
    (vr-deprecated-send-reply
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "list_open_buffers_resp" response))
    )
  )
)

(defun vcode-cmd-list-all-buffers (vcode-request)
  (let ((mess-cont (elt vcode-request 1)) 
	(buffer-names nil)
	(response (make-hash-table :test 'string=)))

    (setq buffer-names 
        (mapcar (lambda (buff) (buffer-name buff)) (buffer-list)))
    (cl-puthash "value" buffer-names response)
    (vr-deprecated-send-reply
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "list_all_buffers_resp" response))
    )
  )
)

(defun vcode-cmd-file-name (vcode-request)
  (let ((mess-cont (elt vcode-request 1)) 
	(response (make-hash-table :test 'string=))
	(buff-name) (file-name) (buff-name))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (setq file-name (buffer-file-name (get-buffer buff-name)))
    (if (and (eq file-name nil) vcode-is-test-editor)
        (setq file-name vcode-test-file-name-was))
    (cl-puthash "value" file-name response)
    (vr-deprecated-send-reply
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "file_name_resp" response))
    )
  )
)


;;;
;;; Kill a buffer, possibly saving it and/or asking user if she wants
;;; to save.
;;;

(defun vcode-kill-buffer (buff-name save)
  ;;; 
  ;;; kill-buffer will prompt user if buffer needs saving
  ;;;


  (if (eq 0 save) 
      (progn
	(kill-buffer buff-name)
      )
  )
  (if (eq -1 save) 
      (progn
	;;;
        ;;; The easiest way to kill a buffer without prompting is to set
        ;;; the visited file name to nil, so Emacs thinks the buffer
        ;;; isn't associated with a file
	;;;

          (save-excursion
            (set-buffer buff-name)
            (set-visited-file-name nil)
            (kill-buffer buff-name)
;        (kill-buffer buff-name)
          )
	)
  )
  (if (eq 1 save) 
      (progn 
	(save-buffer) 
	(kill-buffer buff-name)
      )
  )
)


(defun vcode-cmd-close-buffer (vcode-request)
  (let ((mess-cont (elt vcode-request 1))
	(response (make-hash-table :test 'string=))
	(buff-name) (buff) (save))

    (cl-puthash "value" 1 response)

    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (setq buff (get-buffer buff-name))
    (setq save (cl-gethash "save" mess-cont))

    (if (not buff)
       ;;; 'buff_name is not the name of a buffer. Maybe the name
       ;;; of a file visited by a buffer?
	(progn
	  (setq buff (find-buffer-visiting buff-name))
	)
      )

    ;;;
    ;;; Ignore VCode requests to close the minibuffer
    ;;;
    (vr-deprecated-log "--** vcode-cmd-close-buffer: before testing for minbufff, buff=%S\n")
    (if buff 
	(if (not (string-match "*Minbuff-" (buffer-name buff)))
	    (progn 
	      (vr-deprecated-log "--** vcode-cmd-close-buffer: closing the buffer\n")
	      (vcode-kill-buffer buff-name save)
	    )
	  (vr-deprecated-log "-- ** vcode-cmd-close-buffer: this is minibuffer... not closing it\n")
	)
      (ding)
      (message (format "vr-deprecated Mode: could not close buffer %S" buff-name))
      (cl-puthash "value" 0 response)
    )

    (vr-deprecated-log "--** vcode-cmd-close-buffer: before send-reply\n")
    (vr-deprecated-send-reply
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "close_buffer_resp" response))
    )
    (vr-deprecated-log "--** vcode-cmd-close-buffer: after send-reply\n")
  )
)


(defun vcode-fix-pos (pos fix-for-who default-pos)

  "Fixes a cursor position received from VCode server. 

If the position is nil, then returns the current position in the current 
buffer.

Also converts from VCode's 0-based positions to Emacs 1-based positions."

  ;;;
  ;;; If nil position was received from vcode, set it to the default
  ;;; position received as argument (current cursor, end of buffer or 
  ;;; beginning of buffer). No need to convert it to 1-based 
  ;;; counting since it is already passed it in 1-based counting
  ;;;
  (if (and (eq 'emacs fix-for-who) (not pos))
      (progn
	(setq pos default-pos)
      )
    
    ;;; Else, convert non-nil position to appropriate counting base. 
    (setq pos (vcode-convert-pos pos fix-for-who)))
  pos
)

(defun vcode-make-sure-range-is-valid-for-buffer (range buff-name)
   "Make sure range is a valid range for buffer named buff-name. 

    If start of range is less than (min-point), set it to (min-point).

    If start of range is greater than (max-point), set it to (max-point).

    We assume that start of range is less or equal to end of range.
   "
   (let ((range-start (elt range 0)) (range-end (elt range 1)))
      
      (save-excursion
	 (switch-to-buffer buff-name)
         (if (< range-start (point-min)) (setq range-start (point-min)))
         (if (> range-end (point-max)) (setq range-end (point-max)))
      )
      (list range-start range-end)
    )
  
)

(defun vcode-fix-range-for-emacs (range default)
  "Fixes a position range received from VCode server. This range
may be nil or contain string values, and the 1st element may be
greater than the 2nd element. If the range is nil, use
the range specified in default"

  (let ((start) (end) (tmp))
    (if (eq range nil)
        (progn 
            (vr-deprecated-log "--** vcode-fix-range-for-emacs: range is nil, using default\n")
	    (setq start (nth 0 default))
	    (setq end (nth 1 default))
; defaults already in Emacs 1-based counting
	)
	(setq start (wddx-coerce-int (nth 0 range)))
	(setq end (wddx-coerce-int (nth 1 range)))
; but range needs to be converted to Emacs 1-based counting
	(setq start (vcode-convert-pos start 'emacs))
	(setq end (vcode-convert-pos end 'emacs))
    )

    (if (> start end)
	(progn
	  (setq tmp end)
	  (setq end start)
	  (setq start tmp)
	  )      
      )
    (list start end)
  )
)

; NO!!!
; range arguments are never (start nil) or (nil start).  They are always
; (start end) or simply nil (i.e. Python None), and in the latter case,
; default to the start and end of the selection for the specified buffer

; When we want to use start is nil to mean beginning of buffer or end is
; nil to mean end of buffer, we use separate start and end arguments
; -- DCF:

(defun bad-vcode-fix-range (range for-who default-range)
  "Fixes a position range received from VCode server. This range
may contain nil or string values, and the 1st element may be
greater than the 2nd element. If one of the values in 'rage is nil, 
then use values in 'default-range."

  (let ((start (nth 0 range)) (end (nth 1 range)) (tmp))
    (setq start (wddx-coerce-int start))
    (setq end (wddx-coerce-int end))

    ;;;
    ;;; If nil start position was received from VCode, set it to the lower
    ;;; bound of the default range
    ;;; No need to convert it to 1-based counting because it is already
    ;;; in 1-based value.
    ;;;
    (if (and (eq 'emacs for-who) (not start))
	(setq start (nth 0 default-range))
      ;;; Else, convert start position to appropriate count base
      (setq start (vcode-convert-pos start for-who)))

    ;;;
    ;;; If nil end position was received from VCode, set it to upper bound
    ;;; of default-range.
    ;;; No need to convert it to 1-based counting because it is already
    ;;; a 1-based value.
    ;;;
    (if (and (eq 'emacs for-who) (not end))
	(setq end (nth 1 default-range))
      ;;; Else, convert end position to appropriate count base
      (setq end (vcode-convert-pos end for-who)))

    (if (> start end)
	(progn
	  (setq tmp end)
	  (setq end start)
	  (setq start end)
	  )      
      )
    (list start end)
  )
)

(defun vcode-convert-pos (pos for-who)
  "Because Emacs and VCode use a different base for counting character 
positions (Emacs starts from 1, VCode from 0), we need to convert from one
to the other"

  ;;;
  ;;; NOTE: It is admissible for 'vcode-conver-pos to be invoked with a
  ;;;       nil position. For example, we use a nil end position for 
  ;;;       insert updates that delete the whole buffer and replaces it
  ;;;       with new content.
  ;;;
  (if pos
      (if (equal for-who 'vcode)
	  (progn 
	    (1- pos)
	    )
	(1+ pos)
	)
  )
)

(defun vcode-convert-range (start end for-who)
  (list (vcode-convert-pos start for-who) (vcode-convert-pos end for-who))
)

(defun is-in-hash (key table)
   (not (eq (cl-gethash key table 'entrywasnotintable) 'entrywasnotintable))
)

(defun vcode-selection-of-buff-in-message (mess)
  "Identifies the start and end of the current selection in the buffer 
  to which VCode message 'mess applies. If no buffer listed in the
  message, use the current buffer"
  (let ((buff-name))
    (save-excursion
      (setq buff-name (cl-gethash "buff_name" mess nil))
      (if buff-name
	  (set-buffer buff-name)
      )	  
      (vr-deprecated-log "--** vcode-selection-of-buff-in-message: mark-active = %S\n" mark-active)
      (vr-deprecated-log "--** vcode-selection-of-buff-in-message: raw range = %S, %S\n"
        (if mark-active (mark) nil) (point))
      (vr-deprecated-log "--** vcode-selection-of-buff-in-message: no-nil range = %S\n"
        (vcode-make-sure-no-nil-in-selection (if mark-active (mark) nil)
        (point)))
      (vcode-make-sure-no-nil-in-selection (if mark-active (mark) nil) (point))
    )
  )
)  


(defun vcode-bounds-of-buff-in-message (mess)
  "Identifies the start and end of the buffer to which VCode message 
'mess applies. If no buffer listed in the message, return bounds of current 
buffer"
  (let ((buff-name))
    (save-excursion
      (setq buff-name (cl-gethash "buff_name" mess nil))
      (if buff-name
	  (set-buffer buff-name)
      )	  
      (list (point-min) (point-max))
    )
  )
)  

;;;
;;; Fix possibly nil positions in messages, and convert them between 0-based 
;;; and 1-based counting
;;;
(defun vcode-fix-positions-in-message (message fix-for-who)
   (let ((pos) (range) (default-pos) (buff-name) (dummy-var-temp))
     (save-excursion
       (if (eq fix-for-who 'emacs)
         (progn
;             (vcode-trace "vcode-fix-positions-in-message" "here")
           (if (is-in-hash "buff_name" message)
             (progn
;                   (vcode-trace "vcode-fix-positions-in-message"
;                       "buff_name in hash")
;                   (setq buff-name (cl-gethash "buff_name" message))
               (setq buff-name (vcode-get-buff-name-from-message message))
               (if (get-buffer buff-name)
                   (set-buffer buff-name))
             )
           )
;             (vcode-trace "vcode-fix-positions-in-message" "message is %S"
;                 message)
;            (setq buff-name (vcode-get-buff-name-from-message message))
;            (setq buff-name nil)
         )
       )
;       (if buff-name (set-buffer buff-name))
       (dolist (position-field '("pos" "position" "start" "end")) 
         (if (is-in-hash position-field message)
             (progn
               (if (or (string= "pos" position-field) 
                       (string= "position" position-field))
                   (setq default-pos (point)))
               (if (string= "start" position-field)
                   (setq default-pos 
                         (nth 0 (vcode-bounds-of-buff-in-message message)))
               )
               (if (string= "end" position-field)
                   (setq default-pos 
                         (nth 1 (vcode-bounds-of-buff-in-message message)))
               )
               (setq pos (cl-gethash position-field message))
               (setq pos (vcode-fix-pos pos fix-for-who default-pos))
               (cl-puthash position-field pos message)
             )
         )
       )

       (if (is-in-hash "range" message)
           (progn
               (setq range (cl-gethash "range" message))
               (vr-deprecated-log "--** vcode-fix-positions-in-message: range is %S\n" range)
               ;;;
               ;;; Fix possibly nil positions received from VCode
               ;;;
               (if (eq fix-for-who 'emacs)
                   (setq range (vcode-fix-range-for-emacs 
                                range 
                                (vcode-selection-of-buff-in-message message))
                   )
               )
               (cl-puthash "range" range  message)
           )
       )
    )
    message
  )
)

(defun vcode-make-sure-no-nil-in-selection (start end)
  "Makes sure that the selection is not 'nil"
  (if (not start) (setq start end))
  (if (not end) (setq end start))
  (list start end)
)

(defun vcode-cmd-cur-pos (vcode-request)
  (let ((mess-cont (make-hash-table :test 'string=)) (buff-name))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (save-excursion
      (set-buffer buff-name)
      (cl-puthash 'value (vcode-convert-pos (point) 'vcode) mess-cont)
    )
    (vr-deprecated-send-reply 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "cur_pos_resp" mess-cont)))
    )
)

(defun vcode-cmd-line-num-of (vcode-request)
  (let ((mess-cont (nth 1 vcode-request))
	(response (make-hash-table :test 'string=))
	(buff-name) (line-num) (opoint))

    (setq opoint (cl-gethash "position" mess-cont))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))

    (save-excursion
      (set-buffer buff-name)
      (goto-char opoint)
      (setq line-num (line-at-pos (point)))
      )

    (cl-puthash "value" line-num response)
    (vr-deprecated-send-reply 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "line_num_of_resp" response)))
    )
  )

(defun vcode-cmd-get-pos-selection (vcode-request)
  (let ((mess-cont (nth 1 vcode-request))
        (response (make-hash-table :test 'string=))
	(value (make-hash-table :test 'string=))
	(selection) (to-convert) (no-nil-selection) (pos) (buff-name))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (save-excursion
	(set-buffer buff-name)
	(setq no-nil-selection 
	    (vcode-make-sure-no-nil-in-selection 
		(point) (if mark-active (mark))))
	(setq to-convert (append no-nil-selection (list 'vcode)))
	(setq selection (apply 'vcode-convert-range to-convert))
	(setq pos (vcode-convert-pos (point) 'vcode))
        (vr-deprecated-log "--** vcode-cmd-get-pos-selection: pos = %S, selection = %S\n"
            pos selection)
    )
    (cl-puthash 'pos pos value)
    (cl-puthash 'selection selection value)
    (cl-puthash 'value 
		value
		response)
    (vr-deprecated-send-reply 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "get_pos_selection_resp" response)))
    )
  )


(defun vcode-cmd-get-selection (vcode-request)
  (let ((mess-cont (nth 1 vcode-request))
        (response (make-hash-table :test 'string=))
	(selection) (buff-name))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (save-excursion
      (set-buffer buff-name)
      (setq selection (vcode-make-sure-no-nil-in-selection (point) 
	  (if mark-active (mark))))
    )
    (cl-puthash 'value 
		(list (vcode-convert-pos (nth 0 selection) 'vcode)
		      (vcode-convert-pos (nth 1 selection) 'vcode))
		response)
    (vr-deprecated-send-reply 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "get_selection_resp" response)))
    )
  )

(defun vcode-cmd-get-text (vcode-request)
  (let ((resp-cont (make-hash-table :test 'string=)) 
	(mess-cont (elt vcode-request 1))
	(buff-name) (start) (end)
	)
    (setq start (cl-gethash "start" mess-cont))
    (setq end (cl-gethash "end" mess-cont))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (vcode-trace "vcode-cmd-get-text" "(current-buffer)=%S, buff-name=%S, start=%S, end=%S" 
		 (current-buffer) buff-name start end)

    (save-excursion
      (set-buffer buff-name)
      (cl-puthash "value" (buffer-substring-no-properties start end) resp-cont)
      (cl-puthash (current-buffer) t vcode-cached-buffers)
    )

    (vr-deprecated-send-reply 
     (run-hook-with-args 
      'vr-deprecated-serialize-message-hook (list "get_text_resp" resp-cont)))
    )
  )


(defun vcode-cmd-get-visible (vcode-request)
  (let ((resp-cont (make-hash-table :test 'string=))
	(mess-cont (nth 1 vcode-request))
	(buff-name))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (save-excursion
      (set-buffer buff-name)
      (cl-puthash 'value (vcode-convert-range (window-start) (window-end) 'vcode) resp-cont)
    )
    (vr-deprecated-send-reply (run-hook-with-args 'vr-deprecated-serialize-message-hook (list "get_visible_resp" resp-cont)))
    )  
  )

(defun vcode-cmd-len (vcode-request)
  (let ((resp-cont (make-hash-table :test 'string=))
	(mess-cont (nth 1 vcode-request))
	(buff-name))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (save-excursion
      (set-buffer buff-name)
      (cl-puthash 'value (buffer-size) resp-cont)
    )
    (vr-deprecated-send-reply (run-hook-with-args 'vr-deprecated-serialize-message-hook (list "get_visible_resp" resp-cont)))
    )  
  )

(defun vcode-cmd-newline-conventions (vcode-request)
  (let ((mess-cont (make-hash-table :test 'string=)))
    (cl-puthash 'value (list "\n") mess-cont)
    (vr-deprecated-send-reply (run-hook-with-args 'vr-deprecated-serialize-message-hook (list "newline_conventions_resp" mess-cont)))
    )  
  )

(defun vcode-cmd-pref-newline-conventions (vcode-request)
  (let ((mess-cont (make-hash-table :test 'string=)))
    (cl-puthash 'value "\n" mess-cont)
    (vr-deprecated-send-reply (run-hook-with-args 'vr-deprecated-serialize-message-hook (list "pref_newline_convention_resp" mess-cont)))
    )  
)

(defun vcode-cmd-file-language-name (vcode-request)
  (let ((resp-cont (make-hash-table :test 'string=))
	(mess-cont (nth 1 vcode-request))
	(file-name) (lang-name))
    (setq file-name (cl-gethash "file_name" mess-cont))
    (setq lang-name (vcode-language-for-file file-name))
    (cl-puthash "value" lang-name resp-cont)
    (vr-deprecated-send-reply 
        (run-hook-with-args 'vr-deprecated-serialize-message-hook 
			    (list "file_language_name_resp" resp-cont)))

  )   
)

(defun vcode-cmd-language-name (vcode-request)
  (let ((resp-cont (make-hash-table :test 'string=))
	(mess-cont (nth 1 vcode-request))
	(buff-name) (lang-name))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (setq lang-name (vcode-language-for-buff buff-name))
    (cl-puthash "value" lang-name resp-cont)
    (vr-deprecated-send-reply 
        (run-hook-with-args 'vr-deprecated-serialize-message-hook 
			    (list "language_name_resp" resp-cont)))

  )   
)


(defun vcode-language-for-buff  (buff-name)
   (let ((file-name))
     (vcode-trace "vcode-language-for-buff" "buff-name=%S" buff-name)
     (setq file-name (buffer-file-name (get-buffer buff-name)))
     ;;; In case this is a new buffer not yet associated with a file
     (if (not file-name) (setq file-name buff-name))
     (vcode-trace "vcode-language-for-buff" "file-name=%S" file-name)
     (vcode-language-for-file file-name)
   )
)

(defun vcode-language-for-file (file-name)
  (let ((extension nil) (lang-name "file_names"))
    (vcode-trace "vcode-language-for-file" "file-name=%S" file-name)
    (if file-name 
	(progn 
	  (save-match-data
	    (string-match "\\.\\([^\\.]*\\)$" file-name)
            (if (match-beginning 1)
		(setq extension (substring file-name 
					   (match-beginning 1) (match-end 1)))
	    )
	  )
	  (if extension 
	      (setq lang-name (cl-gethash extension vcode-language-name-map 
				      "file_names")))
	)
    )
    lang-name
  )
)

(defun vcode-force-cursor-and-selection-change-report (buff-name)
  "The 'after-change-functions hook only reports on insertions and
deletions in buffer. Changes in the selection and cursor position 
are not reported.

In case where we respond to VCode by just moving the cursor and/or selection,
we need to add a dummy change report to the queued changes. This dummy change just inserts a blank string over a null region (i.e., it does nothing).

This will in effect end up reporting the position of cursor and selection 
since `vcode-send-queued-changes appends that information for each and every 
change reports it sends to VCode.
"
  (let ()
    (if (not buff-name) (setq buff-name (buffer-name)))
    (setq dummy-change (list buff-name ()))
  )
)

(defun vcode-cmd-set-selection (vcode-request)
  (let ((mess-cont (nth 1 vcode-request)) 
	(sel-range) (put-cursor-at) (sel-start) (sel-end)
        (buff-name))

    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (setq sel-range (cl-gethash "range" mess-cont))
    (setq put-cursor-at (cl-gethash "cursor_at" mess-cont))
    (if (= put-cursor-at 1) 
	(progn 
	  (setq sel-start (elt sel-range 0))
	  (setq sel-end (elt sel-range 1))
	  )
      (progn 
	(setq sel-start (elt sel-range 1))
	(setq sel-end (elt sel-range 0))
	)
      )
    (condition-case err     
	(progn
;	  (switch-to-buffer buff-name)
	  (set-buffer buff-name)
	  (goto-char sel-start)
	  (set-mark sel-start)
	  (goto-char sel-end)
	)
       ('error (error "vr-deprecated Error: could not select region [%S, %S]" sel-start sel-end))
    )

    ;;;
    ;;; Selection changes do not automatically get queued to the change queue.
    ;;; Need to do so explicitely
    ;;;
    ;;; Compute final selection here as opposed to inside the 'condition-case
    ;;; statement. That way, if there are errors, we can still report
    ;;; where Emacs actually set the selection, as opposed to where we
    ;;; expected it to go.
    (vcode-report-goto-select-change buff-name 
	(if mark-active (mark) (point)) (point))

    (vcode-send-queued-changes)
  )
)

(defun vcode-cmd-move-relative-page (vcode-request)
  (let ((mess-cont (elt vcode-request 1)) 
	(buff-name) (direction) (num) (times-so-far 0))
    (setq direction (cl-gethash "direction" mess-cont))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (setq num (cl-gethash "num" mess-cont))
    (vcode-trace "vcode-cmd-move-relative-page" "direction=%S, buff-name=%S, num=%S" direction buff-name num)
    (set-buffer buff-name)
    (while (< times-so-far num)
      (vcode-trace "vcode-cmd-move-relative-page" "times-so-far=%S, num=%S" times-so-far num)
      (if (>= direction 0)
	  (scroll-up-nomark nil)
	(scroll-down-nomark nil))
      (setq times-so-far (+ 1 times-so-far))
    )

    ;;;
    ;;; Cursor changes do not automatically get queued to the change queue.
    ;;; Need to do so explicitely
    ;;;
    (vcode-report-goto-select-change buff-name 
      (if mark-active (mark) (point))
      (point)
    )
  )



  (vcode-send-queued-changes)
)

(defun vcode-cmd-backspace (vcode-request)
  (let ((mess-name (elt vcode-request 0)) 
	(mess-cont (elt vcode-request 1))
	(buff-name) 
	(n-times) (many-dels))
	(setq buff-name (vcode-get-buff-name-from-message mess-cont))
	(setq n-times (cl-gethash "n_times" mess-cont))


	(set-buffer buff-name)

; executing a command string of n-times dels doesn't have the same effect 
; as actual DEL keys, when the region is non-empty,
; so in that case we patch it up by killing the region and then sending
; n-times - 1 dels
        (if (and mark-active 
                 (not (eq (mark) (point))))
            (progn
              (kill-region (mark) (point))
              (setq n-times (- n-times 1))
            )
        )

        (setq many-dels (make-string n-times 127))
        (vcode-execute-command-string many-dels)
; try this instead (though I don't think set-buffer will be enough here
; -- at least it isn't when I execute this from a function called
; manually by eval-sexp
;        (execute-kbd-macro [127] n-times)
        

	(vcode-send-queued-changes)
    )
)


(defun vcode-cmd-insert (vcode-request)
  (let ((mess-name (elt vcode-request 0)) 
	(mess-cont (elt vcode-request 1))
	(text) (range) (vr-deprecated-request) 
	(delete-start) (delete-end))
	(setq text (wddx-coerce-string (cl-gethash "text" mess-cont)))
	(setq buff-name (vcode-get-buff-name-from-message mess-cont))
	(setq range (cl-gethash "range" mess-cont))
	(setq delete-start (elt range 0))
	(setq delete-end (elt range 1))

;	(vr-deprecated-log "--** vcode-cmd-insert: upon entry, (point)=%S,
;	    (mark)=%S, range=%S, delete-start=%S, delete-end=%S, text=%S\n" 
;	    (point) (mark) range delete-start delete-end text)
	(vcode-trace "vcode-cmd-insert" "upon entry, (point)=%S,
	    (mark)=%S, range=%S, delete-start=%S, delete-end=%S, text=%S\n" 
	    (point) (mark) range delete-start delete-end text)


	(set-buffer buff-name)

	(kill-region delete-start delete-end)
	(set-mark nil)

        (vcode-execute-command-string text)

	(vcode-send-queued-changes)
    )
)

(defun vcode-cmd-insert-indent (vcode-request)
  (let ((mess-name (elt vcode-request 0)) 
	(mess-cont (elt vcode-request 1))
	(code-bef) (code-after) (range) (buff-name) (vr-deprecated-request) 
	(delete-start) (delete-end))
	(setq code-bef (wddx-coerce-string (cl-gethash "code_bef" mess-cont)))
	(setq code-after (wddx-coerce-string (cl-gethash "code_after" mess-cont)))
	(setq buff-name (vcode-get-buff-name-from-message mess-cont))
	(setq range (cl-gethash "range" mess-cont))
	(setq delete-start (elt range 0))
	(setq delete-end (elt range 1))

	(vcode-trace "vcode-cmd-insert-indent" "code before=%S"
		     code-bef)
        ;;;
        ;;; We wrap this in an exception catching block in case we try
        ;;; to dictate text in a read-only buffer like "*Completions*"
        (condition-case err
           (progn
	     (set-buffer buff-name)

	     (kill-region delete-start delete-end)

	     (vcode-trace "vcode-cmd-insert-indent" "after kill-region")

	     (vcode-execute-command-string code-bef)
	     (vcode-trace "vcode-cmd-insert-indent" "after before, buffer=%S"
			  (buffer-substring (point-min) (point-max)))


	     (save-excursion
	       (vcode-execute-command-string code-after)
	     )

	     (set-mark nil)
	   )
           ('error (message "VCode Error: executing insert_indent"))
	)

	(vcode-send-queued-changes)
    )
)


(defun vcode-cmd-set-text (vcode-request)
  (let ((mess-name (elt vcode-request 0)) 
 	(mess-cont (elt vcode-request 1))
 	(buff-name) (text) (start) (end))

 	(setq text (wddx-coerce-string (cl-gethash "text" mess-cont)))
 	(setq buff-name (vcode-get-buff-name-from-message mess-cont))
 	(setq start (cl-gethash "start" mess-cont))
 	(setq end (cl-gethash "end" mess-cont))
        (vcode-trace "vcode-cmd-set-text" "buff-name=%S, text=%S, start=%S, end=%S\n" buff-name text start end)
; Is this causing new problems with scratch that with multiple windows
; open, or did those problems predate the addition of save-excursion?
; The latter -- DCF
        (save-excursion
            (set-buffer buff-name)
            (vcode-trace "vcode-cmd-set-text" "*** before kill-region, in buff-name, (point-min)=%S, (point-max)=%S, after-change-functions=%S, buffer contains:\n%S" (point-min) (point-max) after-change-functions (buffer-substring (point-min) (point-max)))
            (kill-region start end)
            (vcode-trace "vcode-cmd-set-text" "*** after kill-region, buffer contains\n%S" (buffer-substring (point-min) (point-max)))
            ; DCF -- the code below was outside the excursion, so the
            ; text got inserted into the wrong buffer
            ;;;
            ;;; We don't use 'vcode-execute-command-string because set_text message
            ;;; is used to restore a buffer to a previous state (which is
            ;;; assumed to have already been indented properly)

            ; ugh - need this, otherwise it doesn't even insert at the
            ; right place
            (goto-char start)
            (push-mark start)
            (insert text)
            (push-mark nil)
            (vcode-trace "vcode-cmd-set-text" "*** after insert, buffer contains\n%S" (buffer-substring (point-min) (point-max)))
        )


 	(vcode-send-queued-changes)
    )
)



(defun vcode-cmd-indent (vcode-request)
  (let ((mess-name (elt vcode-request 0)) 
	(mess-cont (elt vcode-request 1))
	(range) (vr-deprecated-request) (buff-name)
	(indent-start) (indent-end))

    (setq range (cl-gethash "range" mess-cont))
    (setq indent-start (elt range 0))
    (setq indent-end (elt range 1))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))


    (set-buffer buff-name)
    (vcode-indent-region indent-start indent-end)

    (vcode-send-queued-changes)
  )
)


(defun vcode-cmd-decr-indent-level (vcode-request)
  (let ((mess-name (elt vcode-request 0))
	(mess-cont (elt vcode-request 1))
	(range) (levels) (vr-deprecated-request) (buff-name)
	(indent-start) (indent-end))
    (setq range (cl-gethash "range" mess-cont))
    (setq indent-start (elt range 0))
    (setq indent-end (elt range 1))
    (setq levels (cl-gethash "levels" mess-cont))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))

    (vcode-trace "vcode-cmd-decr-indent-level" "upon entry, buff-name=%S, (point)=%S, (mark)=%S, range=%S, levels=%S\n" buff-name (point) (mark) range levels)

    ;;;
    ;;; Note: don't enclose this in a save-excursion because it causes problems
    ;;;       with Python unindentation of single blank lin
    ;;;
    (set-buffer buff-name)

    (set-mark nil)

    (vcode-unindent-region indent-start indent-end levels) 

    (vcode-send-queued-changes)
  )
)

(defun vcode-cmd-incr-indent-level (vcode-request)
  (let ((mess-name (elt vcode-request 0))
	(mess-cont (elt vcode-request 1))
	(range) (levels) (vr-deprecated-request) (buff-name)
	(indent-start) (indent-end))
    (setq range (cl-gethash "range" mess-cont))
    (setq indent-start (elt range 0))
    (setq indent-end (elt range 1))
    (setq levels (cl-gethash "levels" mess-cont))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))

    (vcode-trace "vcode-cmd-incr-indent-level" "upon entry, (point)=%S, (mark)=%S, range=%S, levels=%S\n" (point) (mark) range levels)



    (save-excursion
      (set-buffer buff-name)

      (set-mark nil)
      (vcode-incr-indent-region indent-start indent-end levels) 

    )

   (vcode-send-queued-changes)

  )
)


(defun vcode-indent-region (start end)
  ;;;
  ;;; For some reason, when I invoke (indent-region start end) it doesn't 
  ;;; work. If I mark the region and then invoke (indent-region) without
  ;;; arguments, it doesn't work either. The only way I found to make it
  ;;; work is to mark the region, and then simulate an interactive call to
  ;;; indent-region
  ;;; 
  (save-excursion
    (set-mark start)
    (goto-char end)
    (call-interactively 'indent-region)
;    (indent-region start end)
  )
)

(defun vcode-unindent-region (start end n-levels)
  "Deindents region from START to END by N-LEVELS levels."
  (let ((end-line))
    (for-lines-in-region-do start end 'vcode-unindent-line (list n-levels))
  )
)

(defun vcode-incr-indent-region (start end n-levels)
  "Indent region from START to END by N-LEVELS levels."
  (let ()
    (for-lines-in-region-do start end 'vcode-indent-line (list n-levels))
  )
)

(defun vcode-unindent-line (n-levels)
  (interactive "nNumber of levels: ")
  (let ((counter 0) (start-of-line))
     (vcode-trace "vcode-unindent-line" "upon entry, n-levels=%S, (point)=%S, (mark)=%S, buffer contains: \n%S\n" n-levels (point) (mark) (buffer-substring (point-min) (point-max)))
      ;;;
      ;;; Move to the first non-blank character on the line, then simulate the
      ;;; backspace key multiple times.
      ;;;
      ;;; Don't enclose this in a save-excursion because it causes problems 
      ;;; Python indentation of a blank line
      ;;;
;      (save-excursion
	(beginning-of-line)
	(setq start-of-line (point))
	(while (and (looking-at " ") (< (point) (point-max)))
	  (forward-char-nomark 1)
	  )

        ;;;
        ;;; Don't backspace if empty line, because that will delete the line 
        ;;; instead of deindenting.
        ;;;
	(if (not (or (eq start-of-line (point)) (eq 1 (point))))
	    (progn
	      (setq counter 0)
	      (while (< counter n-levels)
                  ;;; Execute a command string containing just the 
		  ;;; backspace key
;	          (vcode-execute-command-string "\177")
;	          (vcode-execute-command-string "\127")
		(vcode-execute-command-string "\d")
		(setq counter (1+ counter))
	      )
           )
	  )
;	)

     (vcode-trace "vcode-unindent-line" "upon exit, n-levels=%S, (point)=%S, (mark)=%S, buffer contains: \n%S\n" n-levels (point) (mark) (buffer-substring (point-min) (point-max)))
   )
)

(defun vcode-indent-line (n-levels)
  (interactive "nNumber of levels: ")
  (let ((counter 0) (start-of-line))
     (vcode-trace "vcode-indent-line" "upon entry, n-levels=%S, (point)=%S, (mark)=%S, buffer contains: \n%S\n" n-levels (point) (mark) (buffer-substring (point-min) (point-max)))
     (setq counter 0)
     (while (< counter n-levels)
        ;;; Execute a command string containing just the 
	;;; backspace key
       (vcode-execute-command-string "\t")
       (setq counter (1+ counter))
     )

     (vcode-trace "vcode-indent-line" "upon exit, n-levels=%S, (point)=%S, (mark)=%S, buffer contains: \n%S\n" n-levels (point) (mark) (buffer-substring (point-min) (point-max)))
   )
)

(defun line-at-pos (pos)
   (let ((line-num))
     (setq line-num (count-lines (point-min) pos))
     (save-excursion
       (goto-char pos)
       (if (bolp)
           (setq line-num (1+ line-num))
       )
     )
     line-num
   )
)

(defun for-lines-in-region-do (start end do-this args)
  (let ((start-line) (end-line) (keep-going) (current-line))

     ;;; 
     ;;; Don't enclose inside a save-excursion. It causes problems for
     ;;; Python backindentation of a blank line. Basically, the cursor ends
     ;;; up at the beginning of the line instead of at the place where
     ;;; the cursor was when you deindented the line
     ;;;
     ;;; On the other hand, if you don't enclose it, you get other problems
     ;;; when unindenting a non-blank line. Basically, the cursor is off
     ;;; by some characters.
     ;;;
;    (save-excursion
       (setq end-line (line-at-pos end))
       (goto-char start)
       (setq keep-going t)
       (while keep-going
         (apply do-this args)
	 (setq current-line (line-at-pos (point)))
         (if (<  current-line end-line)
	     (next-line 1)
	   (setq keep-going nil)
	 )
       )
;;;
;    )
  )
)


(defun vcode-cmd-delete (vcode-request)
  (let ((mess-name (elt vcode-request 0)) 
	(mess-cont (elt vcode-request 1))
	(text) (range) (vr-deprecated-request) 
	(delete-start) (delete-end) (buff-name))

	(setq buff-name (vcode-get-buff-name-from-message mess-cont))
	(setq range (cl-gethash "range" mess-cont))
        (setq range
              (vcode-make-sure-range-is-valid-for-buffer range buff-name))

	(setq delete-start (elt range 0))
	(setq delete-end (elt range 1))
	(set-buffer buff-name)
	(kill-region delete-start delete-end)
        (set-mark nil)

	(vcode-send-queued-changes)
    )
)


(defun vcode-cmd-copy-selection (vcode-request)
  (let ((mess-name (elt vcode-request 0)) 
	(mess-cont (elt vcode-request 1))
        (buff nil))

     (save-excursion
       (setq buff (cl-gethash 'buff_name mess-cont))

       (set-buffer buff)

       (message "vcode-cmd-copy-selection" "(mark)=%S, (point)=%S" (mark) (point))
       (vcode-trace "vcode-cmd-copy-selection" "(mark)=%S, (point)=%S" (mark) (point))

       (copy-region-as-kill-nomark (mark) (point))
       (vcode-trace "vcode-cmd-copy-selection" "after copy, (mark)=%S, (point)=%S" (mark) (point))
       (vcode-report-goto-select-change (buffer-name) (mark) (point))
     )

     (vcode-send-queued-changes)

  )
)


(defun vcode-cmd-cut-selection (vcode-request)
  (let ((mess-name (elt vcode-request 0)) 
	(mess-cont (elt vcode-request 1))
	(buff nil))

     (save-excursion
       (setq buff (cl-gethash 'buff_name mess-cont))
       (set-buffer buff)
       (kill-region (mark) (point))
       (vcode-send-queued-changes)
     )
  )
)


(defun vcode-cmd-paste (vcode-request)
  (let ((mess-name (elt vcode-request 0)) 
	(mess-cont (elt vcode-request 1))
	(buff nil))

     (save-excursion
       (setq buff (cl-gethash 'buff_name mess-cont))
       (set-buffer buff)
       (yank)
     )
     (vcode-send-queued-changes)
  )
)


(defun vcode-cmd-goto (vcode-request)
  (let ((mess-cont (nth 1 vcode-request))
	(pos) (buff-name) (final-pos))
    (setq pos (cl-gethash "pos" mess-cont))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (condition-case err     
	(progn 
;	  (switch-to-buffer buff-name)
          (set-buffer buff-name)
	  (goto-char pos)
	  (push-mark (point))
          (vr-deprecated-log "--** vcode-cmd-goto: mark-active = %S\n" mark-active)
	)

      ('error (error "vr-deprecated Error: could not go to position %S" pos))
    )

    ;;;
    ;;; Compute final position here instead of inside the 'condition-case 
    ;;; statement. That way, if there were some errors, we can still
    ;;; report where the cursor actually went (as opposed to where we 
    ;;; expected it to go).
    ;;;
;    (switch-to-buffer buff-name)
    (set-buffer buff-name)
; what if I change this to set-buffer (essentially, to change buffers,
; we should issue an explicit call to AppState.change_buffer)
    (setq final-pos (point))

    ;;;
    ;;; Cursor changes do not automatically get queued to the change queue.
    ;;; Need to do so explicitely
    ;;;
    (vcode-report-goto-select-change buff-name final-pos final-pos)

    (vcode-send-queued-changes)

  )
)

(defun vcode-cmd-goto-line (vcode-request)
  (let ((mess-cont (nth 1 vcode-request))
	(line-num) (go-where) (buff-name) (final-pos))
    (setq line-num (cl-gethash "linenum" mess-cont))
    (setq go-where (cl-gethash "where" mess-cont))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (condition-case err     
	(progn 
;	  (switch-to-buffer buff-name)
          (set-buffer buff-name)
	  (goto-line line-num)
	  (if (= -1 go-where) 
	      (beginning-of-line)
	    (end-of-line)
	  )
	  (push-mark (point))
	)
      ('error (error "vr-deprecated Error: could not go to line %S" line-num))
    )

    ;;;
    ;;; Compute final position here instead of inside the 'condition-case 
    ;;; statement. That way, if there were some errors, we can still
    ;;; report where the cursor actually went (as opposed to where we 
    ;;; expected it to go).
    ;;;
;    (switch-to-buffer buff-name)
    (set-buffer buff-name)
; what if I change this to set-buffer (essentially, to change buffers,
; we should issue an explicit call to AppState.change_buffer)
    (setq final-pos (point))

    ;;;
    ;;; Cursor changes do not automatically get queued to the change queue.
    ;;; Need to do so explicitely
    ;;;
    (vcode-report-goto-select-change buff-name final-pos final-pos)

    (vcode-send-queued-changes)

  )
)

(defun vcode-line-start-end-pos (which-end vcode-request)
  (let ((mess-cont (nth 1 vcode-request))
	(pos) (line-num) (return-pos)
	(which-end-as-text)
	(response (make-hash-table :test 'string=)))
    (setq pos (cl-gethash "pos" mess-cont))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (if (< which-end 0)
	(setq which-end-as-text "beginning")
      (setq which-end-as-text "end")
    )
;    (vcode-trace "vcode-line-start-end-pos" 
;		 "pos=%S, buff-name=%S, line-num=%S, which-end=%S, which-end-as-text=%S"
;		 pos buff-name line-num which-end which-end-as-text)
    (save-excursion
       (condition-case err     
	   (progn 
	     (set-buffer buff-name)
             (if (not (equal pos nil))
                 (goto-char pos))
   	     (if (<  which-end 0) 
	         (beginning-of-line)
	       (end-of-line)
	     )
	     (setq return-pos (vcode-convert-pos (point) 'vcode))
	     (vcode-trace "vcode-line-start-end-pos" "** return-pos=%S" return-pos)
	     (cl-puthash "value" return-pos response)
	     (setq reply-name (concat which-end-as-text "_of_line_resp"))
	     (vr-deprecated-send-reply 
	     (run-hook-with-args 
	       'vr-deprecated-serialize-message-hook 
	       (list reply-name response)))
	   )
         ('error (error (concat "vr-deprecated Error: could not get %S of line at %S" which-end-as-text pos)))
       )
    )
  )
)

(defun vcode-cmd-end-of-line (vcode-request)
  (vcode-line-start-end-pos 1 vcode-request)
)

(defun vcode-cmd-beginning-of-line (vcode-request)
  (vcode-line-start-end-pos -1 vcode-request)
)

(defun vcode-cmd-find-matching (vcode-request)
  (let ((mess-cont (nth 1 vcode-request))
        (return-pos) (pos (point)) (reply-name) (buff-name) (direction)
	(response (make-hash-table :test 'string=)))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (setq direction (cl-gethash "direction" mess-cont nil))
    (save-excursion
      (condition-case err     
        (progn 
          (set-buffer buff-name)
          (setq return-pos 
             (vcode-convert-pos (scan-lists (point) direction 0) 'vcode))
        )
;        ('error (error "vr-deprecated Error: could not find matching paren/brace to that at %d" pos))
      )
      (cl-puthash "value" return-pos response)
      (setq reply-name "find_matching_resp")
      (vr-deprecated-send-reply 
        (run-hook-with-args 
          'vr-deprecated-serialize-message-hook 
          (list reply-name response)
        )
      )
    )
  )
)

(defun vcode-cmd-beginning-of-statement (vcode-request)
  (let ((mess-cont (nth 1 vcode-request))
        (return-pos) (pos (point)) (reply-name) (buff-name)
	(response (make-hash-table :test 'string=)))
    (setq buff-name (vcode-get-buff-name-from-message mess-cont))
    (vcode-trace "vcode-cmd-beginning-of-statement" "** buff-name=%S" buff-name)
    (save-excursion
      (condition-case err     
        (progn 
          (set-buffer buff-name)
          (if (equal major-mode 'c-mode)
            (progn
              (c-beginning-of-statement)
              (setq return-pos (vcode-convert-pos (point) 'vcode))
            )
;            ('error (error "vr-deprecated Error: could not find beginning of statement at %d" pos))
          )
        )
;        ('error (error "vr-deprecated Error: could not find beginning of statement at %d" pos))
      )
      (cl-puthash "value" return-pos response)
      (setq reply-name "beginning_of_statement_resp")
      (vr-deprecated-send-reply 
        (run-hook-with-args 
          'vr-deprecated-serialize-message-hook 
          (list reply-name response)
        )
      )
    )
  )
)



(defun vcode-cmd-change-buff (vcode-request)
  (let ((mess-cont (nth 1 vcode-request))
	(buff-name) (success)
        (response (make-hash-table :test 'string=)))
    (setq buff-name (cl-gethash "buff_name" mess-cont nil))
    (setq success 1)
    (vcode-trace "vcode-cmd-change-buff" "buff-name is %S" buff-name)
    (if (not buff-name) 
	(vcode-switch-buffer-dlg)
      (condition-case
	  (switch-to-buffer buff-name)
	('error (progn (setq success 0) (vcode-trace "vcode-cmd-change-buff"
"error switching to buffer %S" buff-name)))
      )
    )
    (cl-puthash "value" success response)
    (vr-deprecated-send-reply 
        (run-hook-with-args 'vr-deprecated-serialize-message-hook 
			    (list "change_buff_resp" response)))
	 
  )
)

(defun vcode-switch-buffer-dlg ()
   (list-buffers)
   (switch-to-buffer "*Completions*")
)

(defun vcode-cmd-mediator-closing (vcode-request)
   (vcode-mode-activate nil)
; huh? this tries to connect again when we get a mediator closing
; message.  Maybe Alain meant deactivate
;  (vcode-mode-activate 'vcode)
)

;;;; =============== SNIPPET MANAGEMENT =============

;;;; functions to queue buffer changes
(defun vcode-indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max)))

(defun vcode-queue-whole-buffer-change (buff-name)
  (setq vcode-queued-changes 
    (append vcode-queued-changes 
      (vcode-generate-whole-buffer-changes buff-name))))

(defun vcode-queue-current-buffer-change ()
  (interactive)
  (vcode-queue-whole-buffer-change (buffer-name)))

;;;; ===============================================
;;;; Snippet Templates for Java
(defun vcode-snippet-insert-java-import ()
  (interactive)
  (snippet-insert "import $${package};$."))

(defun vcode-snippet-insert-java-package ()
  (interactive)
  (snippet-insert "package $${package};$."))

(defun vcode-snippet-insert-java-if ()
  (interactive)
  (snippet-insert "if ($${true}) {\n$>$.\n}"))

(defun vcode-snippet-insert-java-if-else ()
  (interactive)
  (snippet-insert "if ($${true}) {\n$>$.\n} else  {\n$>\n$>}"))

(defun vcode-snippet-insert-java-else-if ()
  (interactive)
  (snippet-insert "else  {\n$>$${// TODO: add statement}\n} if ($${true}) {\n$>$.\n}"))

(defun vcode-snippet-insert-java-else ()
  (interactive)
  (snippet-insert "else {\n$>$${// TODO: add statement}\n}$."))

(defun vcode-snippet-insert-java-for ()
  (interactive)
  (snippet-insert "for (int $${i} = 0; $${i} < $${range}; $${i}++) {\n$>$.\n}"))

(defun vcode-snippet-insert-java-for-each ()
  (interactive)
  (snippet-insert "for ($${iterable_type} $${iterable_element} : $${iterable}) {\n$>$.\n}"))

(defun vcode-snippet-insert-java-iterate-array ()
  (interactive)
  (snippet-insert "for (int $${index} = 0; $${index} < $${array}.length; $${index}++) {\n$>$.\n$>}"))

(defun vcode-snippet-insert-java-iterate-collection ()
  (interactive)
  (snippet-insert "for (Iterator $${iterator} = $${collection}.iterator(); $${iterator}.hasNext(); {\n$>$${type} $${element} = ($${type}) $${iterator}.next();\n$>$.\n$>}"))

(defun vcode-snippet-insert-java-while ()
  (interactive)
  (snippet-insert "while ($${true}) {\n$>$.\n}"))

(defun vcode-snippet-insert-java-while-enumeration ()
  (interactive)
  (snippet-insert "while ($${enumeration}.hasMoreElements()) {\n$>$${type} $${element} = ($${type}) $${enumeration}.nextElement();\n$>$.\n}"))

(defun vcode-snippet-insert-java-while-iterator ()
  (interactive)
  (snippet-insert "while ($${iterator}.hasNext()) {\n$>$${type} $${element} = ($${type}) $${iterator}.next();\n$>$.\n}"))

(defun vcode-snippet-insert-java-do-while ()
  (interactive)
  (snippet-insert "do {\n$>$.\n$>} while ($${True});"))

(defun vcode-snippet-insert-java-lazy-init ()
  (interactive)
  (snippet-insert "if ($${name} == null) {\n$>$${name} = new $${type}($${arguments});\n$>$.\n$>}\n$>return $${name};"))

(defun vcode-snippet-insert-java-new-instance ()
  (interactive)
  (snippet-insert "$${intf} $${name} = new $${impl}($${args});\$."))

(defun vcode-snippet-insert-java-try-catch ()
  (interactive)
  (snippet-insert "try {\n$>$.\n} catch ($${Exception} e) {\n$>$${e.printStackTrace(); // TODO: handle exception}\n$>}"))

(defun vcode-snippet-insert-java-catch ()
  (interactive)
  (snippet-insert "catch ($${Exception} e) {\n$>$${e.printStackTrace(); // TODO: handle exception}\n$>}\n$."))

(defun vcode-snippet-insert-java-cast ()
  (interactive)
  (snippet-insert "$${type} $${name} = ($${type}) $${instance};\n$."))

(defun vcode-snippet-insert-java-to-array ()
  (interactive)
  (snippet-insert "$${type}[] $${name} = ($${type}[]) $${collection}.toArray(new $${type}[$${collection}.size()]);\n$."))

(defun vcode-snippet-insert-java-define-main ()
  (interactive)
  (snippet-insert "public static void main(String[] args) {\n$>$.\n}"))

(defun vcode-snippet-insert-java-define-private-method ()
  (interactive)
  (snippet-insert "private $${return_type} $${name}($${args}) {\n$>$.\n}"))

(defun vcode-snippet-insert-java-define-public-method ()
  (interactive)
  (snippet-insert "public $${return_type} $${name}($${args}) {\n$>$.\n}"))

(defun vcode-snippet-insert-java-define-protected-method ()
  (interactive)
  (snippet-insert "protected $${return_type} $${name}($${args}) {\n$>$.\n}"))

(defun vcode-snippet-insert-java-define-public-static-method ()
  (interactive)
  (snippet-insert "public static $${return_type} $${name}($${args}) {\n$>$.\n}"))

(defun vcode-snippet-insert-java-define-private-static-method ()
  (interactive)
  (snippet-insert "private static $${return_type} $${name}($${args}) {\n$>$.\n}"))

(defun vcode-snippet-insert-java-define-runnable ()
  (interactive)
  (snippet-insert "new Runnable() {\n$>public void run() {\n$>$.\n}"))

(defun vcode-snippet-insert-java-define-anonymous-class ()
  (interactive)
  (snippet-insert "new $${type}() {\n$>$.\n}"))

(defun vcode-snippet-insert-java-switch ()
  (interactive)
  (snippet-insert "switch ($${key}) {\n$>case $${value}:$.\n$>break;\n$>default:\n$>break;\n}"))

(defun vcode-snippet-insert-java-case ()
  (interactive)
  (snippet-insert "case $${label}: $.\n$>break;"))

(defun vcode-snippet-insert-java-default ()
  (interactive)
  (snippet-insert "default: $."))

(defun vcode-snippet-insert-java-synchronized ()
  (interactive)
  (snippet-insert "synchronized ($${mutex}) {\n$>$.\n}"))

(defun vcode-snippet-insert-java-print-error ()
  (interactive)
  (snippet-insert "System.err.println(\"$${ERROR: }\");$."))

(defun vcode-snippet-insert-java-print ()
  (interactive)
  (snippet-insert "System.out.println(\"$${message}\");$."))

(defun vcode-snippet-insert-java-print-trace ()
  (interactive)
  (snippet-insert "System.out.println(\"$${var}:\" + $${var});$."))

(defun vcode-snippet-insert-java-define-test ()
  (interactive)
  (snippet-insert "public void test$${name}() throws Exception {\n$>$.\n}"))

(defun vcode-snippet-insert-java-define-interface ()
  (interactive)
  (snippet-insert "public interface $${name} {\n$>$.\n}"))

(defun vcode-snippet-insert-java-define-annotation ()
  (interactive)
  (snippet-insert "public @interface $${name} {\n$>$.\n}"))

(defun vcode-snippet-insert-java-annotation ()
  (interactive)
  (snippet-insert "@$${name}($${arg})\n$>$."))

(defun vcode-snippet-insert-java-define-class ()
  (interactive)
  (snippet-insert "public class $${name} {\n$>$.\n}"))

(defun vcode-snippet-insert-java-define-inner-class ()
  (interactive)
  (snippet-insert "public static class $${name} {\n$>$.\n>}"))

(defun vcode-snippet-insert-java-extend-class ()
  (interactive)
  (snippet-insert " extends $${type}$."))

(defun vcode-snippet-insert-java-implement-interface ()
  (interactive)
  (snippet-insert "implements $${intf}$."))

(defun vcode-snippet-insert-java-implement-interfaces ()
  (interactive)
  (snippet-insert "implements $${intf1}, $${intf2}$."))

