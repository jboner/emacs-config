
;;
;; VR Mode - integration of GNU Emacs and Dragon NaturallySpeaking.
;;
;; Copyright 1999 Barry Jaspan, <bjaspan@mit.edu>, 2001-2003, 2007
;; Patrik Jonsson <patrik-voice at familjenjonsson.org>.  All rights
;; reserved.
;;
;; $Id: vr.el,v 1.23 2007/02/17 19:42:18 grifgrif Exp $
;;
;; This file is part of Emacs VR Mode (http://emacs-vr-mode.SourceForge.net).
;;
;; Emacs VR Mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; Emacs VR Mode is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs VR Mode; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-command "vr.exe" "*The \"vr.exe\" program to be
invoked as the VR mode sub-process.  This can be just the name, if the
program is in your PATH, or it can be a full path.")

(defvar vr-host nil "*The name of the computer running the VR Mode
process.  If nil, the process will be started on this computer.  See
also vr-port.")

(defvar vr-port 0 "*The port on which to connect to vr-host.  If
vr-host is nil, this can be zero to tell the VR Mode process to use
any available port.")

(defvar vr-win-class nil
  "*Class name of the Windows window for which VR Mode will accept
voice input.  Whenever a window matching vr-win-class and vr-win-title
(which see) is the foreground window, dictation and commands spoken
into the microphone will be executed by VR Mode.")
(defvar vr-win-title "emacs"
  "*Title of the Windows window for which VR Mode will accept voice
input.  Whenever a window matching vr-win-class (which see) and
vr-win-title is the foreground window, dictation and commands spoken
into the microphone will be executed by VR Mode.")

(defvar vr-activation-list nil
  "*A list of buffer name patterns which VR Mode will voice activate.
Each element of the list is a REGEXP.  Any buffer whose name matches
any element of the list is voice activated.  For example, with

(setq vr-activation-list '(\"^\\*scratch\\*$\" \"\\.txt$\"))

the buffer named \"*scratch*\" and any buffer whose name ends with
\".txt\" will be voice-activated.  Note that voice activation of the
minibuffer is controlled by vr-activate-minibuffer.")

(defvar vr-activate-minibuffer t
  "*Flag controlling whether the minibuffer is voice-activated.")

(defvar vr-voice-command-list '(vr-default-voice-commands)
  "*The list of Emacs interactive commands that can be invoked by
voice.  Each element can be a command, a CONS cell containing
spoken text and a command or key sequence, the name of a list
containing any of these (recursive option), or the special symbol
'vr-default-voice-commands, which implicitly includes the voice
commands in vr-default-voice-command-list (which see).

For example:

(setq test-commands '(test-command-one test-command-two))
(setq vr-voice-command-list
      '(vr-default-voice-commands
	test-commands
	my-command
	(\"other command\" . my-other-command)
	(\"prefix shell command\" . [\?\\C-u \?\\M-\\S-!])))

sets up the voice commands

	Spoken			Invokes
	===============		=============
	test command one       	M-x test-command-one
	test command two      	M-x test-command-two
	my command		M-x my-command
	other command		M-x my-other-command
	prefix shell command	C-u M-S-! (i.e. C-u M-x shell-command)

along with all the commands on vr-default-voice-command-list.")

(setq vr-voice-command-list
  '(vr-default-voice-commands
	
    ;; misc stuff
    ("voice code mode" . vcode-mode)
    ("comp" . dabbrev-expand)
    ("hippie" . hippie-expand)
    insert-current-time
    match-paren
    delete-blank-lines
    ("toggle line numbers" . linum)
    ("line numbers" . linum)
    ("set mark" . set-mark-command)
    ("indent whole buffer" . iwb)
    ("indent buffer" . iwb)
    ("maven compile" . mvn)
	goto-line

    ;; etags - navigation
    complete-tag
    ("tag search" . tags-search)
    ("next tag search" . tags-loop-continue)
    find-tag
    visit-tags-table
    ("replace tag" . tags-query-replace)
    ("find tag regex" . find-tag-regexp)
    ("next tag" . [?\C-u ?\M-.])
    ("back tag" . [?\M-*])

    ;; ido
    ("switch buffer" . ido-switch-buffer)
    ("switch buffer other window" . ido-switch-buffer-other-window)
    ("find file" . ido-find-file)
    ("open directory" . ido-dired) 
    ("kill buffer" . ido-kill-buffer)
    ("insert buffer" . ido-insert-buffer)
    ("i do next" . ido-next-match)
    ("i do previous" . ido-prev-match)
    ("i do match" . ido-up-directory)
    ("i do match" . ido-complete)    

    ;; bookmarks
    ("toggle bookmark" . bm-toggle)
    ("next bookmark" . bm-previous)
    ("previous bookmark" . bm-next)
    ("show bookmarks" . bm-show)
    
    ;; nxml-mode
    ("N. X. M. L. mode" . nxml-mode)
    ("complete element" . nxml-complete)
    ("close element" . nxml-finish-element)

    ;; ECB	
    ("goto folders" . ecb-goto-window-directories)
    ("goto files" . ecb-goto-window-sources)
    ("goto methods" . ecb-goto-window-methods)
    ("goto history" . ecb-goto-window-history)
    ("goto source" . ecb-goto-window-edit1)
    ("goto next" . ecb-nav-goto-next)
    ("goto previous" . ecb-nav-goto-previous)
    ("toggle compile window" . ecb-toggle-compile-window)
    ("toggle E. C. B." . ecb-toggle-ecb-windows)
    ("E. C. B. activate" . ecb-activate)

    ;; senator tag management
    ("jump tag" . senator-jump)
    ("next tag" . senator-next-token)
    ("previous tag" . senator-previous-token)

    ;; flymake
    flymake-mode
    ("quick fix" . my-flymake-show-next-error)
    ("flymake next" . my-flymake-show-next-error)
    ("flymake display" . flymake-display-err-menu-for-current-line)
    ("flymake previous" . flymake-goto-previous-error)
    ("flymake start check" . flymake-start-syntax-check)
    ("flymake stop check" . flymake-stop-all-syntax-checks)

    ;; JDE
    jde-mode
    jde-compile
    jde-build
    ("load project" . jde-load-project-file)
    ("save project" . jde-save-project)
    ("comp it" . jde-complete-minibuf)
    ("find and import class" . jde-import-find-and-import)
    ("import all" . jde-import-all)
    ("organize imports" . jde-import-organize)
    ("kill extra imports" . jde-import-kill-extra-imports)        

    ;; JDE usages
    ("open class" . jde-open-class-source-with-completion)
    ("locate class" . jde-usages-locate-class)
    ("next usage" . jde-usages-next-pos)
    ("show call tree" . jde-usages-display-call-tree)
    ("show call tree for reference" . jde-usages-display-call-tree-for-thing-at-point)
    ("show call tree for this" . jde-usages-display-call-tree-for-thing-at-point)
    ("show subtypes" . jde-usages-display-subs-and-implementers)
    ("show supertypes" . jde-usages-display-superclasses)
    ("show type hierarchy" . jde-usages-display-type-hierarchy)
    ("show implementing methods" . jde-usages-display-subs-implementing-method)
    ))

(defconst vr-default-voice-command-list
  '(

    ;; Lists
    (list "0to20" "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20")
    
    ;; VR Mode commands
    ("activate buffer" . vr-add-to-activation-list)

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
    ("faster" . "vr-repeat-mult-rate 0.5")
    ("slower" . "vr-repeat-mult-rate 2")
    ("stop" . "vr-repeat-stop nil")

    ;; Repeat that.
    ("repeat that <0to20> times" . vr-repeat-that)

    ;; files
;;    find-file
    save-buffer
;;    find-file-other-window
;;    find-file-other-frame
    
if    ;; buffers
;;    switch-to-buffer
;;    kill-buffer
;;    switch-to-buffer-other-window
    switch-to-buffer-other-frame
    ("resynchronize" .  vr-resynchronize)
    
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
    ("page-down" . scroll-up)
    ("page-up" . scroll-down)
    ("beginning of line" . beginning-of-line)
    ("end of line" . end-of-line)
    ("beginning of buffer" . beginning-of-buffer)
    ("end of buffer" . end-of-buffer)

    ("move up" . vr-repeat-move-up-s)
    ("move up slow" . vr-repeat-move-up-s)
    ("move up fast" . vr-repeat-move-up-f)
    ("move down" . vr-repeat-move-down-s)
    ("move down slow" . vr-repeat-move-down-s)
    ("move down fast" . vr-repeat-move-down-f)
    ("move left" . vr-repeat-move-left-s)
    ("move left slow" . vr-repeat-move-left-s)
    ("move left fast" . vr-repeat-move-left-f)
    ("move right" . vr-repeat-move-right-s)
    ("move right slow" . vr-repeat-move-right-s)
    ("move right fast" . vr-repeat-move-right-f)

    ("up <0to20>" . previous-line)
    ("down <0to20>" . next-line)
    ("left <0to20>" . backward-char)
    ("right <0to20>" . forward-char)
    ("left <0to20> words" . backward-word)
    ("right <0to20> words" . forward-word)
    ("left <0to20> sentences" . backward-sentence)
    ("right <0to20> sentences" . forward-sentence)
    ("left <0to20> paragraphs" . backward-paragraph)
    ("right <0to20> paragraphs" . forward-paragraph)

    ("back <0to20>" . backward-char)
    ("forward <0to20>" . forward-char)
    ("back <0to20> words" . backward-word)
    ("forward <0to20> words" . forward-word)
    ("back <0to20> sentences" . backward-sentence)
    ("forward <0to20> sentences" . forward-sentence)
    ("back <0to20> paragraphs" . backward-paragraph)
    ("forward <0to20> paragraphs" . forward-paragraph)

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

    ;; editing text
    ("delete char <0to20>" . delete-char)
    ("kill word <0to20>" . kill-word)
    ("backward kill word <0to20>" . backward-kill-word)
    ("left kill word <0to20>" . backward-kill-word)
    ("kill line <0to20>" . kill-line)
    ("repeat kill line" . "vr-repeat-kill-line 0.5")
    yank
    yank-pop
    ;; assumes yank-pop has key binding, else last-command==self-insert-command
    ("yank again" . yank-pop)
    ;; requires a key binding for yank, repeat yank to work!
    ("repeat yank" . vr-repeat-yank)

    ("set mark" . set-mark-command)
    kill-region
    ("cut region" . kill-region)
    ("copy region" . copy-region-as-kill)
    
    ;; Searching
    ("I search forward" . isearch-forward)
    ("I search backward" . isearch-backward)
    ("repeat I search forward" . vr-repeat-search-forward-s)
    ("repeat I search backward" . vr-repeat-search-backward-s)

    ;; formatting
    fill-paragraph
    
    ;; modes
    auto-fill-mode
    exit-minibuffer
    )
  "*A list of standard Emacs voice commands.  This list is used as-is
whenever vr-voice-command-list (which see) includes the symbol
'vr-default-voice-commands, or it can be appended explicitly in a
custom vr-voice-command-list.")

(defvar vr-log-do nil "*If non-nil, VR mode prints debugging information
in the \" *vr*\" buffer.")

(defvar vr-log-send nil "*If non-nil, VR mode logs all data sent to the VR
subprocess in the \" *vr*\" buffer.")

(defvar vr-log-read nil "*If non-nil, VR mode logs all data received
from the VR subprocess in the \" *vr*\" buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-mode-setup-hook nil
  "Hooks that are run after VR Mode is enabled but before VR.EXE is
successfully started (or connected to).  See also
vr-mode-startup-hook, called later.")

(defvar vr-mode-cleanup-hook nil
  "Hooks that are run after VR Mode is disabled and after VR.EXE has
exited or been told to exit.")

(defvar vr-mode-startup-hook nil
  "Hooks that are run after VR Mode is enabled, VR.EXE is
successfully started (or connected to), and after VR.EXE is initialized
with any per-connection state such as voice commands.  See also
vr-mode-setup-hook, called earlier.")

(defvar vr-mode-modified-hook nil
  "Hooks that are called whenever a voice activated buffer is modifed
for any reason, invoked by the 'modified-hooks property of vr-overlay.
Arguments provided are OVERLAY AFTER BEG END LEN.  If any hook returns
non-nil, VR Mode will *not* perform its normal modification processing
(ie: telling VR.EXE/DNS about the change).

If vr-ignore-changes is not nil, the hook has been invoked inside
vr-cmd-make-changes, which means the current change comes from DNS,
not from the keyboard or elsewhere.

Danger, Will Robinson!")

(defvar vr-cmd-listening-hook '(vr-cmd-listening)
  "Hooks that are called when the VR Mode command \"listening\" is
received.  Each hook function receives a single argument, REQ,
which is the list representing the command and its arguments.  If any
hook function returns non-nil, subsequent hooks on the list will not
be called.")

(defvar vr-cmd-connected-hook '(vr-cmd-connected)
  "Hooks that are called when the VR Mode command \"connected\" is
received.  Each hook function receives a single argument, REQ,
which is the list representing the command and its arguments.  If any
hook function returns non-nil, subsequent hooks on the list will not
be called.")

(defvar vr-cmd-initialize-hook '(vr-cmd-initialize)
  "Hooks that are called when the VR Mode command \"initialize\" is
received.  Each hook function receives a single argument, REQ,
which is the list representing the command and its arguments.  If any
hook function returns non-nil, subsequent hooks on the list will not
be called.")

(defvar vr-cmd-terminating-hook '(vr-cmd-terminating)
  "Hooks that are called when the VR Mode command \"terminating\" is
received.  Each hook function receives a single argument, REQ,
which is the list representing the command and its arguments.  If any
hook function returns non-nil, subsequent hooks on the list will not
be called.")

(defvar vr-cmd-frame-activated-hook '(vr-cmd-frame-activated)
  "Hooks that are called when the VR Mode command \"frame-activated\" is
received.  Each hook function receives a single argument, REQ,
which is the list representing the command and its arguments.  If any
hook function returns non-nil, subsequent hooks on the list will not
be called.")

(defvar vr-cmd-heard-command-hook '(vr-cmd-heard-command)
  "Hooks that are called when the VR Mode command \"heard-command\" is
received.  Each hook function receives a single argument, REQ,
which is the list representing the command and its arguments.  If any
hook function returns non-nil, subsequent hooks on the list will not
be called.")

(defvar vr-cmd-mic-state-hook '(vr-cmd-mic-state)
  "Hooks that are called when the VR Mode command \"mic-state\" is
received.  Each hook function receives a single argument, REQ,
which is the list representing the command and its arguments.  If any
hook function returns non-nil, subsequent hooks on the list will not
be called.")

(defvar vr-cmd-get-buffer-info-hook '(vr-cmd-get-buffer-info)
  "Hooks that are called when the VR Mode command \"get-buffer-info\" is
received.  Each hook function receives a single argument, REQ,
which is the list representing the command and its arguments.  If any
hook function returns non-nil, subsequent hooks on the list will not
be called.")

(defvar vr-cmd-make-changes-hook '(vr-cmd-make-changes)
  "Hooks that are called when the VR Mode command \"make-changes\" is
received.  Each hook function receives a single argument, REQ,
which is the list representing the command and its arguments.  If any
hook function returns non-nil, subsequent hooks on the list will not
be called.")

(defvar vr-cmd-recognition-hook '(vr-cmd-recognition)
  "Hooks that are called when the VR Mode command \"recognition\" is
received.  Each hook function receives a single argument, REQ,
which is the list representing the command and its arguments.  If any
hook function returns non-nil, subsequent hooks on the list will not
be called.")

(defvar vr-invisible-hook nil
  "This hook is called before VR Mode looks for \"vr-invisible\" text,
which makes it easy to plug-in routines that make various things
invisible.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-mode nil
  "Non-nil turns on VR (Voice Recognition) mode.  DO NOT SET THIS
VARIABLE DIRECTLY.  Call M-x vr-mode instead.")

(defvar vr-internal-activation-list nil
  "The working copy of vr-activation-list.  Keeping it separate allows
re-starting VR Mode to undo vr-add-to-activation-list.")

(defvar vr-mode-line " VR"
  "String displayed in the minor mode list when VR mode is enabled.
In the dictation buffer, the format is VR:<micstate>.")
(make-variable-buffer-local 'vr-mode-line)
(if (not (assq 'vr-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(vr-mode vr-mode-line)
				 minor-mode-alist)))

(defvar vr-mic-state "not connected"
  "String storing the microphone state for display in the mode line.")

(defvar vr-overlay nil
  "Overlay used to track changes to voice-activated buffers.")
(make-variable-buffer-local 'vr-overlay)

(defvar vr-select-overlay (make-overlay 1 1)
  "Overlay used to track and visually indicate the NaturallySpeaking
selection.")
(delete-overlay vr-select-overlay)
(overlay-put vr-select-overlay 'face 'region)
(if (eq window-system nil)
    (progn
      (overlay-put vr-select-overlay 'before-string "[")
      (overlay-put vr-select-overlay 'after-string "]")))

(defvar vr-process nil "The VR mode subprocess.")
(defvar vr-emacs-cmds nil)
(defvar vr-dns-cmds nil)

(defvar vr-reading-string nil "Storage for partially-read commands
from the VR subprocess.")

(defvar vr-buffer nil "The current voice-activated buffer, or nil.
See vr-activate-buffer and vr-switch-to-buffer.")

(defvar vr-ignore-changes nil "see comment in vr-overlay-modified")
(defvar vr-queued-changes nil "see comment in vr-overlay-modified")

(defvar vr-cmd-executing nil
  "If non-nil, the command symbol heard by NaturallySpeaking and
currently being executed by VR Mode, for which VR.EXE is expecting a
reply when done.")

(defvar vr-resynchronize-buffer nil)
(make-variable-buffer-local 'vr-resynchronize-buffer)

;; all of these variables have to do with abbreviation expansion functions
(defvar deferred-function nil)
(defvar vr-deferred-deferred-function nil)
(defvar vr-deferred-deferred-deferred-function nil)
;; load the abbreviation function definitions
(load "abbrev-cmds")
;;(if (not (boundp 'fix-else-abbrev-expansion))
  ;;  (defun fix-else-abbrev-expansion () nil))

(defconst vr-nonlocal-exit-commands
  '(exit-minibuffer minibuffer-complete-and-exit)
  "These commands never exit and can't be executed in the make-changes
loop without screwing up the I/O.") 

(defvar vr-floating-invisible nil
  "If non-nil, vr-zap-invisible has marked text \"removed\", and this
needs to be found and restored in vr-restore-invisible.")

(defvar vr-point-shift nil
  "Info for translating buffer positions between emacs and vr.exe if there is invisible text around.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixes for miscellaneous interaction issues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for some reason activating a buffer in VR mode sometimes makes this
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
  "make VR mode resynchronize the buffer after a placeholder has been
expanded, since they often make it go out of sync."

  (message "Resynchronizing VR-buffer")
  (call-interactively 'vr-resynchronize)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-prefix-map nil "Prefix key used to access VR mode commands.")
(defvar vr-map nil)
(if vr-map
    nil
  (setq vr-map (make-sparse-keymap))
  (define-key vr-map "ws" 'vr-show-window)
  (define-key vr-map "wh" 'vr-hide-window)
  (define-key vr-map "B" 'vr-add-to-activation-list)
  (define-key vr-map "b" 'vr-switch-to-buffer)
  (define-key vr-map "m" 'vr-toggle-mic)
  (define-key vr-map "q" 'vr-quit)
  (define-key vr-map "\C-\M-y" 'vr-repeat-yank)
  )

(if vr-prefix-map
    nil
  (setq vr-prefix-map (make-sparse-keymap))
  (define-key vr-prefix-map "\C-cv" vr-map))

(if (not (assq 'vr-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
	  (cons (cons 'vr-mode vr-prefix-map) minor-mode-map-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry points for global hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-enter-minibuffer ()
  (if (and vr-emacs-cmds vr-activate-minibuffer)
      (vr-activate-buffer (current-buffer))))

(defun vr-post-command ()
  (add-hook 'post-command-hook 'vr-post-command)
  (if (overlayp vr-select-overlay)
      (delete-overlay vr-select-overlay))
  (vr-log "post-command: %s %s %s\n" this-command
	  vr-cmd-executing (buffer-name))
  (if vr-emacs-cmds
      (progn
	(vr-maybe-activate-buffer (current-buffer))
	(if (and vr-cmd-executing t) ;  (eq vr-cmd-executing this-command))
; apparently this-command is not always set to the name of the
; command, for example kill-line is executed with "kill-region" in
; this-command, so this check doesn't really work
	    (progn
	      (vr-send-cmd (format "command-done %s" vr-cmd-executing))
	      (setq vr-cmd-executing nil)))
	)))

(defun vr-kill-buffer ()
  (if vr-emacs-cmds
      (progn
	(vr-log "kill-buffer: %s\n" (current-buffer))
	(vr-send-cmd (concat "kill-buffer " (buffer-name (current-buffer)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer activation control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-filter (pred in)
  (let (out el)
    (while in
      (setq el (car in))
      (setq in (cdr in))
      (if (funcall pred el)
	  (setq out (cons el out)))
      )
out))
  
(defun vr-add-to-activation-list (buffer)
  "Adds BUFFER, which can be a buffer name or buffer, to the list of
buffers that are voice activated.  Called interactively, adds the
current buffer.

The only way to undo the effect of this function is to re-start VR
Mode."
  ;; If called interactively, vr-post-command will activate the
  ;; current buffer (so this function doesn't have to).
  (interactive (list (current-buffer)))
  (if (bufferp buffer)
      (setq buffer (buffer-name buffer)))
  (if (vr-activate-buffer-p buffer)
      nil
    (setq vr-internal-activation-list
	  (cons (concat "^" (regexp-quote buffer) "$")
		vr-internal-activation-list))))

(defun vr-resynchronize (buffer)
  "asks VR mode to resynchronize this buffer, if it has gotten out of
sync.  (That shouldn't happen, in an ideal world, but..."
  (interactive (list (current-buffer)))
  (set-buffer buffer)
  (setq vr-resynchronize-buffer t))

(defun vr-activate-buffer-p (buffer)
  "Predicate indicating whether BUFFER matches any REGEXP element and
does not match any '(not REGEXP) element of
vr-internal-activation-list.  BUFFER can be a buffer or a buffer name."
  (if (bufferp buffer)
      (setq buffer (buffer-name buffer)))
  (if (string-match "^ \\*Minibuf-[0-9]+\\*$" buffer)
      vr-activate-minibuffer
    (and (vr-filter (lambda (r) (and (stringp r) (string-match r buffer)))
		    vr-internal-activation-list)
	 (not (vr-filter (lambda (r)
			   (and (consp r) (eq (car r) 'not)
				(string-match (car (cdr r)) buffer)))
			 vr-internal-activation-list)))))

(defun vr-maybe-activate-buffer (buffer)
  ;; Deactivate whenever isearch mode is active.  This is a
  ;; "temporary" solution until isearch mode can be supported.
  (if (and (not isearch-mode) (vr-activate-buffer-p (buffer-name buffer)))
      (if (eq buffer vr-buffer)
	  nil
	(vr-activate-buffer buffer))
    (if vr-buffer 
	(vr-activate-buffer nil))))

(defun vr-switch-to-buffer ()
  "Select the current VR mode target buffer in the current window."
  (interactive)
  (if (buffer-live-p vr-buffer)
      (switch-to-buffer vr-buffer)
    (error "VR target buffer no longer exists; use vr-activate-buffer")))

(defun vr-activate-buffer (buffer)
  "Sets the target BUFFER that will receive voice-recognized text.  Called
interactively, sets the current buffer as the target buffer."
  (interactive (list (current-buffer)))
  (if (buffer-live-p vr-buffer)
      (save-excursion
	(set-buffer vr-buffer)
	;; somehow vr-buffer can be set to the minibuffer while
	;; vr-overlay is nil.
	(if (overlayp vr-overlay)
	    (delete-overlay vr-overlay))
	(setq vr-overlay nil)
	(kill-local-variable 'vr-mode-line)))
  (set-default 'vr-mode-line (concat " VR-" vr-mic-state))
  (setq vr-buffer buffer)
  (if buffer
      (save-excursion
	(set-buffer buffer)
	(setq vr-mode-line (concat " VR:" vr-mic-state))
	(if (and enable-multibyte-characters  
		 (multibyte-string-p (buffer-string)))
	    ;; buffer contains multibyte characters -- log a
	    ;; warning (though I think it's fixed now)
	    (vr-log "Warning: Buffer contains multibyte characters, VR Mode may malfuction\n"))
	(vr-send-cmd (concat "activate-buffer " (buffer-name vr-buffer)))
	(if vr-overlay
	    nil
	  (setq vr-modification-stack ())
	  (setq vr-overlay-before-count 0)
	  (setq vr-overlay (make-overlay (point-min) (point-max) nil nil t))
	  (overlay-put vr-overlay 'modification-hooks '(vr-overlay-modified))
	  (overlay-put vr-overlay 'insert-in-front-hooks '(vr-grow-overlay))
	  (overlay-put vr-overlay 'insert-behind-hooks '(vr-grow-overlay)))
	)
    (vr-send-cmd "deactivate-buffer")
    )
  (force-mode-line-update)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tracking changes to voice-activated buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-overlay-before-count 0 "see comment in vr-grow-overlay")

; In emacs 22 and up we use buffer-chars-modified-tick to track
; changes so we don't get confused by properties changes
(defun vr-buffer-tick nil
  (if (functionp 'buffer-chars-modified-tick)
      (buffer-chars-modified-tick)
    (buffer-modified-tick)))

(defun vr-grow-overlay (overlay after beg end &optional len)
  ;; Make OVERLAY grow to contain range START to END.  If called "before"
  ;; twice before called "after", only call vr-overlay-modified once.
  ;; This happens when we type the first char in the buffer, because I
  ;; guess it is inserted both before and after the empty overlay.

  (vr-log "Grow: %s %d %d %s %d\n" (if after "After: " "Before: ") beg end
  	  (if after (int-to-string len) "") vr-overlay-before-count)
  (if after
      (progn
	(move-overlay overlay
		      (min beg (overlay-start overlay))
		      (max end (overlay-end overlay)))
	(setq vr-overlay-before-count (1- vr-overlay-before-count))
	(if (> vr-overlay-before-count 0)
	    (progn;; (vr-log "   ignored duplicate grow\n")
	      nil)
	  (vr-report-change overlay after beg end len))
	;;(setq vr-modification-stack (cdr vr-modification-stack))
	)

    (setq vr-overlay-before-count (1+ vr-overlay-before-count))
    ;;(setq vr-modification-stack (cons (buffer-substring beg end)
    ;;vr-modification-stack))
    ))

(defvar vr-modification-stack () )

(defun vr-overlay-modified (overlay after beg end &optional len)
  (vr-log " overlay modified: a:%s vro:%s %d %d %d: \"%s\"\n"
	  after (eq vr-overlay overlay) beg end (if after len 0)
	  (buffer-substring beg end))
  (vr-log "  modification stack: %d\n" (length vr-modification-stack))
  (if after
      (progn
	(vr-log "   %s %s \n" (car vr-modification-stack)
		(buffer-substring beg end))
	(if (equal (car vr-modification-stack) (buffer-substring beg end))
	    ;; the before and after text are the same, so so it's one of these
	    ;; funky changes we can ignore.
	    (vr-log "ignoring bogus change\n" );;nil
	  ;; they're not equal, so we call the modification routine like before.
	  (vr-report-change overlay after beg end len))
	(setq vr-modification-stack (cdr vr-modification-stack))
	(if (< 0 vr-overlay-before-count)
	    (setq vr-overlay-before-count (1- vr-overlay-before-count))))

    ;; for the before call, we just save the prechange string in the stack
    (setq vr-modification-stack (cons (buffer-substring beg end)
				      vr-modification-stack))))

(defun vr-report-change (overlay after beg end &optional len)
  (if (and (not (run-hook-with-args-until-success 'vr-mode-modified-hook
						  overlay after beg end len))
	   after)
      ;; If vr-ignore-changes is not nil, we are inside the make-changes
      ;; loop.  Don't tell DNS about changes it told us to make.  And,
      ;; for changes we do need to tell DNS about (e.g. auto-fill),
      ;; queue them up instead of sending them immediately to avoid
      ;; synchronization problems.  make-changes will send them when
      ;; it is done. 
      ;;
      ;; This is not a foolproof heuristic.
      (progn
;;	(vr-log " overlay modified: a:%s vro:%s %d %d %d: \"%s\"\n"
;;		after (eq vr-overlay overlay) beg end len
;;		(buffer-substring beg end))
	(if (or (and (eq vr-ignore-changes 'self-insert)
		     (eq len 0)
		     (eq (- end beg) 1)
		     (eq (char-after beg) last-command-char))
		(and (eq vr-ignore-changes 'delete)
		     (> len 0)
		     (eq beg end))
		 (eq vr-ignore-changes 'unconditionally))
	    (progn (vr-log "ignore: %d %d %d: \"%s\" %s\n" beg end len
			   (buffer-substring beg end) vr-ignore-changes)
		   nil)
;	  (vr-log " After: %s %d %d %d: \"%s\"\n" overlay beg end len
;		  (buffer-substring beg end))
	  (if vr-floating-invisible
	      ;; there is invisible text floating around, we have to
	      ;; make sure we don't send a delete command for it,
	      ;; since NaturallySpeaking already thinks it's been
	      ;; deleted.  This is a pain because there could be small
	      ;; invisible chunks, not necessarily the entire change.
	      (vr-only-visible-chunks overlay beg end len)
	    (let ((cmd (format "change-text \"%s\" %d %d %d %d %s"
			       (buffer-name (overlay-buffer overlay))
			       (vr-pfe beg) (vr-pfe end) len
			       (vr-buffer-tick)
;;			       (encode-coding-string 
			       (string-make-unibyte
				(vr-string-replace 
				 (buffer-substring beg end) "\n" "\\n"))
;;				'iso-2022-7bit-ss2-dos)
			       )))
	      (if vr-ignore-changes
		  (setq vr-queued-changes (cons cmd vr-queued-changes))
		(vr-send-cmd cmd))))))
    (vr-log " overlay modified: a:%s vro:%s %d %d : \"%s\"\n"
	    after (eq vr-overlay overlay) beg end 
	    (buffer-substring beg end))
    )
  (if deferred-function
      (progn
	(setq vr-deferred-deferred-function deferred-function)
	(setq deferred-function nil)
	;(debug)
	(delete-backward-char 2)
	;(debug)
	(fix-else-abbrev-expansion)
	;(debug)
	(if (not (eq vr-deferred-deferred-function
		     'else-expand-placeholder))
	    (progn
	      ;;(call-interactively deferred-deferred-function)
	      (vr-log "report-change executing deferred function %s\n" vr-deferred-deferred-function)
	      (setq vr-deferred-deferred-deferred-function
		    vr-deferred-deferred-function )
	      (setq vr-tricky-deferred-deferred tricky-deferred-function)
	      (setq vr-deferred-deferred-function nil)
	      (vr-execute-command
	       vr-deferred-deferred-deferred-function))
	  (vr-log "report-change deferring command %s\n"
		  vr-deferred-deferred-function))
    ))
  t  )

(defun vr-only-visible-chunks (overlay beg end len)
  ;; we're fucked here because we don't know the text that was deleted
  (let ((vr-floating-invisible nil)
	(text (car vr-modification-stack))
	(new-length ))
    (if (and (> len 0) (eq beg end))
	;; it's a deletion
	;; see if we can find the deleted string on the modification
	;; stack
	(progn
	  (vr-log "visible chunks: looking for deleted string\n")
	  (if (eq len (length text))
	      (progn
		(vr-log "  think I found it: %s\n" text)
		;; now our task is to only send the visible portion of
		;; this deletion, which amounts to finding out the
		;; number of invisible characters in the deleted
		;; string.
		(setq new-length (- len (vr-count-invisible text))) 
		(vr-log "  %d visible characters in string\n" new-length) 
		(vr-report-change overlay t beg end
				  new-length)))))))


(defun vr-count-invisible (text)
  (let ((position 0) (count 0))
    (while (setq position (text-property-any position (length text)
					     'vr-invisible
					     'invisible text))
      (let ((end (next-single-property-change position 'vr-invisible
					      text (length text))))
	(setq count (+ count (- end position)))))
    count))
    
    
(defun vr-string-replace (src regexp repl)
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

(defvar vr-recognizing nil)

(defun vr-sleep-while-recognizing ()
  (interactive)
  (let* ((first t) (count 0))
    (while (and (< count 200) vr-recognizing (string= vr-mic-state "on"))
      ;; (if first (message "Waiting for voice recognition..."))
      (setq first nil)
      (setq count (1+ count))
      (sleep-for 0 50))
    (if (eq count 200) 
	(message "Time out in vr-sleep-while-recognizing!")
      ;; (message nil)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subprocess communication.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-log (&rest s)
  (if vr-log-do
      (let* ((buf (get-buffer-create " *vr*"))
	     (win (get-buffer-window buf 't)))
	(save-excursion
					;( message "logging")
	  (set-buffer buf)
	  (goto-char (point-max))
	  (insert (apply 'format s))
	  (if win
	      (set-window-point win (point-max)))
	  ))))

(defun vr-sentinel (p s)
  (if (equal s "disconnect\n")
      (progn
	(if (processp vr-process)
	    (delete-process vr-process))
	(setq vr-process nil)))
  (if (or (equal s "disconnect\n")(equal s "finished\n"))
      (progn
	(if (processp vr-emacs-cmds)
	    (delete-process vr-emacs-cmds))
	(if (processp vr-dns-cmds)
	    (delete-process vr-dns-cmds))
	(setq vr-emacs-cmds nil
	      vr-dns-cmds nil))
    (error "VR process exited with status \"%s\"" s)))

(defun vr-cmd-listening (vr-request)
  (vr-connect "127.0.0.1" (nth 1 vr-request) t)
  t)

(defun vr-cmd-connected (vr-request)
  (vr-send-cmd (format "initialize %s|%s|%s"
		       (if (equal vr-win-class "")
			   nil
			 vr-win-class)
		       (if (equal vr-win-title "")
			   nil
			 vr-win-title)
		       (cdr (assoc 'window-id
				   (frame-parameters (car
						      (visible-frame-list)))))
		       )
	       )
  t)

(defun vr-cmd-initialize (vr-request)
  (cond ((eq (nth 1 vr-request) 'succeeded)
	 (vr-startup))
	((eq (nth 1 vr-request) 'no-window)
	 (vr-mode 0)
	 (message "VR process: no window matching %s %s"
		  vr-win-class vr-win-title))
	(t
	 (vr-mode 0)
	 (message "VR process initialization: %s"
		  (nth 1 vr-request))))
  t)

(defun vr-cmd-terminating (vr-request)
  (let (vr-emacs-cmds)
    (vr-mode 0))
  (if vr-host
      (vr-sentinel nil "finished\n"))
  (message "VR process terminated; VR Mode turned off")
  t)

(defun vr-cmd-frame-activated (vr-request)
  ;; This is ridiculous, but Emacs does not automatically change its
  ;; concept of "selected frame" until you type into it.  So, we have
  ;; the subprocess send us the HWND value and explcitly activate the
  ;; frame that owns it.  The HWND may not belong to any frame, for
  ;; example if vr-win-class/title match a Windows window not
  ;; belonging to Emacs.  In that case, just ignore it.
  ;;
  (let* ((wnd (int-to-string (car (cdr vr-request))))
	 (frame (car (vr-filter
		      (lambda (f) (equal (cdr (assoc 'window-id
						     (frame-parameters f)))
					 wnd))
		      (visible-frame-list)))))
    (if frame
	(select-frame frame)
      (message "VR Mode: %s is not an Emacs frame window handle; ignored."
	       wnd)))
  (vr-maybe-activate-buffer (current-buffer))
  t)

(defun vr-cmd-heard-command (vr-request)
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
  ;; Set vr-cmd-executing so vr-post-command (hook) will inform VR.EXE
  ;; when the command is finished.  If cmd is an undefined key
  ;; sequence, no command will be executed, so complete immediately.
  ;;
  (let* ((cmd (nth 1 vr-request))
	 (kseq (or (and (vectorp cmd) cmd)
		   (where-is-internal cmd nil 'non-ascii)
		   (concat "\M-x" (symbol-name cmd) "\n"))))
    (setq vr-cmd-executing (if (vectorp cmd) (key-binding cmd) cmd))
    (if (not vr-cmd-executing)
	(vr-send-cmd "command-done undefined"))
    
    (if (not (vectorp cmd))
	(vr-execute-command (cdr vr-request))
      (vr-log "running %s as key sequence:\n" cmd )
      (setq unread-command-events
	    (append unread-command-events
		    (listify-key-sequence kseq)))
      ) 
	)
  t)

;; executes a command, and runs the appropriate hooks.  It's used by
;; heard-command and by the deferred-function executions.  VR-command
;; can either be a symbol or a list.
(defun vr-execute-command (vr-command)
  (let ((cmd (or (and (listp vr-command ) (car vr-command))
		 vr-command)))
	  (run-hooks 'pre-command-hook)
	  (condition-case err
	      (if (and (listp vr-command) 
		       (> (length vr-command) 1))
		  (apply cmd (cdr vr-command))
		(call-interactively cmd))
	    ('wrong-number-of-arguments
	     (ding)
	     (message
	      "VR Mode: Wrong number of arguments calling %s"
	      vr-command))
	    ('wrong-type-argument 'error
				  (ding)
				  (message "VR Mode: %s calling %s"
					   (error-message-string err)
					   vr-command )))
	  (vr-log "running post-command-hook for %s\n" cmd)
 	  (let ((this-command cmd))
	    (run-hooks 'post-command-hook)))
  t)


(defun vr-cmd-mic-state (vr-request)
  (let ((state (car (cdr vr-request))))
    (cond ((eq state 'off)
	   (setq vr-mic-state "off"))
	  ((eq state 'on)
	   (setq vr-mic-state "on"))
	  ((eq state 'sleep)
	   (setq vr-mic-state "sleep")))
    (vr-activate-buffer vr-buffer))
  t)

(defun vr-cmd-get-buffer-info (vr-request)
  (let ((buffer (nth 1 vr-request))
	(dns-tick (nth 2 vr-request))
	(our-tick (vr-buffer-tick))
	vr-queued-changes
	(vr-ignore-changes 'buffer-info) 
	vr-text)
    (vr-log "get-buffer-info: current buffer: %s vr-buffer:%s\n"
	    (buffer-name) vr-buffer)
    (setq vr-tricky-deferred-deferred nil)
    (if	(not (equal vr-buffer (get-buffer buffer)))
	(progn
	  (ding)
	  (message "VR Mode: get-buffer-info: %s is not %s"
		   buffer (buffer-name vr-buffer))    

	  ;; make sure that we always give information for the buffer that
	  ;; was asked for, even if we ding and the wrong buffer is
	  ;; selected.  This almost certainly means that the subprocess
	  ;; has not had time to update its idea of current buffer
	  (save-excursion
	    (set-buffer (get-buffer buffer))
	    (vr-log "buffer synchronization problem: using %s\n" 
		    (buffer-name (current-buffer)))
	    (if (and (not vr-resynchronize-buffer) (eq our-tick dns-tick))
		(vr-send-reply "0 not modified")
	      (vr-send-reply "1 modified")
	      (setq vr-text (string-make-unibyte (buffer-string)))
	      (vr-send-reply (length vr-text))
	      (vr-send-reply vr-text))
	    
	    ;; now we send possible queued-up changes resulting from
	    ;; activity further up in this function
	    ;; don't like this code duplication 
	    ;; (vr-zap-invisible) 
	    (vr-send-reply (length vr-queued-changes))
	    (mapcar 'vr-send-reply vr-queued-changes)

	    ;;  these need to be sent after the changes, because
	    ;;  invisible text will change the point values
	    (vr-send-reply (vr-pfe (point)))
	    (vr-send-reply (vr-pfe (point)))
	    (vr-send-reply (vr-pfe (window-start)))
	    (vr-send-reply (vr-pfe (window-end)))
	    (vr-send-reply (format "%d" (vr-buffer-tick)))
	    ))

      ;;
      ;; If mouse-drag-overlay exists in our buffer, it
      ;; overrides vr-select-overlay.
      ;;
      (let* ((mdo mouse-drag-overlay)
	     (sel-buffer (overlay-buffer mdo)))
	(vr-log " %s %s \n" vr-select-overlay sel-buffer)
	(if (eq sel-buffer vr-buffer)
	    (move-overlay vr-select-overlay
			  (overlay-start mdo)
			  (overlay-end mdo)
			  sel-buffer)))

      ;;
      ;; Then, send buffer contents, if modified.
      ;;

      (if (and (not vr-resynchronize-buffer) (eq our-tick dns-tick))
	    (vr-send-reply "0 not modified")
	;; we need to dump the entire buffer contents
	(if vr-resynchronize-buffer
	    (vr-log "buffer resynchronization requested \n"))
	(vr-send-reply "1 modified")
	(setq vr-text (string-make-unibyte (buffer-string)))
	(set-text-properties 0 (length vr-text) nil vr-text)
	(vr-send-reply (length vr-text))
	(vr-send-reply vr-text)
	(setq vr-resynchronize-buffer nil))

      ;; deal with potential "vr-invisible" text
      (vr-zap-invisible)
      ;; now we send possible queued-up changes resulting from
      ;; the above function
      (vr-send-reply (length vr-queued-changes))
      (mapcar 'vr-send-reply vr-queued-changes)))

      ;; Send selection (or point) and viewable window.
      ;;
      (let ((sel-buffer (overlay-buffer vr-select-overlay)))
	(if (eq sel-buffer vr-buffer)
	    (progn
	      (vr-send-reply (vr-pfe (overlay-start vr-select-overlay)))
	      (vr-send-reply (vr-pfe (overlay-end vr-select-overlay)))
	      )
	  (vr-send-reply (vr-pfe (point)))
	  (vr-send-reply (vr-pfe (point)))
	  ))
      (vr-send-reply (vr-pfe (window-start)))
      (vr-send-reply (vr-pfe (window-end)))
      ;; and the current buffer tick
      (vr-send-reply (format "%d" (vr-buffer-tick)))
      
;(sleep-for 0 100)
;  (yank)
  t)

;; perform changes requested by NaturallySpeaking
(defun vr-cmd-make-changes (vr-request)
  (if (eq (current-buffer) vr-buffer)
      (let ((start (vr-pte (nth 1 vr-request)))
	    (num-chars (nth 2 vr-request))
	    (text (nth 3 vr-request))
	    (sel-start (vr-pte (nth 4 vr-request)))
	    (sel-chars (nth 5 vr-request))
	    vr-queued-changes)
	(if (and buffer-read-only (or (< 0 num-chars) (< 0 (length text))))
	    ;; if the buffer is read-only we don't make any changes
	    ;; to the buffer, and instead we send the 
	    ;; the inverse command back
	    (progn
	      (vr-log "make changes:Buffer is read-only %d %d\n"
		      num-chars (length text))
	      (let ((cmd (format "change-text \"%s\" %d %d %d %d %s"
				 (buffer-name) (vr-pfe start)
				 (+ (vr-pfe start) num-chars) (length text)
				 (vr-buffer-tick)
				 (string-make-unibyte
				  (vr-string-replace
				   (buffer-substring start (+ start num-chars))
				   "\n" "\\n")))))
		(setq vr-queued-changes (cons cmd
					      vr-queued-changes))))
	    
	  ;; if it's not read-only we perform the changes as before
	  (let ((vr-ignore-changes 'delete))
	    (delete-region start (+ start num-chars)))
	  (goto-char start)
	  (let ((vr-ignore-changes 'self-insert))
	    ;;we make the changes by inserting the appropriate
	    ;;keystrokes and evaluating them
	    (setq unread-command-events
		  (append unread-command-events
			  (listify-key-sequence text)))
	    (while unread-command-events
	      (let* ((event(read-key-sequence-vector nil))
		     (command (key-binding event))
		     (this-command command)
		     (last-command-char (elt event 0))
		     (last-command-event (elt event 0))
		     (last-command-keys event)
		     )
		(vr-log "key-sequence %s %s %s\n" event
			command last-command-char)
		(run-hooks 'pre-command-hook)
		;; See if we should resynchronize because of deferred
		;; movement commands
		(if vr-tricky-deferred-deferred
		  (call-interactively 'vr-resynchronize))
		(if (eq command 'self-insert-command)
		    (command-execute command nil)
		  (vr-log "command is not a self insert: %s\n"
			  command )
		  ;; send back a "delete command", since when
		  ;; command is executed it will send the insertion.
		  (let ((cmd (format "change-text \"%s\" %d %d %d %d %s"
				     (buffer-name) (vr-pfe (point))
				     (1+ (vr-pfe (point)))
				     1 (vr-buffer-tick) ""))
			(vr-ignore-changes 'command-insert ))
		    (setq vr-queued-changes (cons cmd
						  vr-queued-changes))
		    ;; exit-minibuffer is a command that does not
		    ;; return properly , so to avoid timeouts waiting
		    ;; for the replies, we put it in the deferred
		    ;; function
		    (if (memq command vr-nonlocal-exit-commands )
			(setq vr-deferred-deferred-function command )
		      (command-execute command nil))
		    (vr-log "executed command: %s\n" command)
		    ))
		(run-hooks 'post-command-hook)
		))); ends self-insert region

	  ;; whether or not we should put point where
	  ;; NaturallySpeaking wants is not so easy to decide.  If
	  ;; point is not there, dictation won't work correctly if
	  ;; there are characters in front of point.  On the other
	  ;; hand, keys can be bound to multiple characters, and
	  ;; deferred functions can move point in which case
	  ;; NaturallySpeaking has no idea where it should be.  This
	  ;; is some kind of heuristic.
	  (if (equal (length text) 0)
	      ;; this is a pure selection or cursor repositioning,
	      ;; just put it there
	      (progn
		(vr-log "make changes: putting point at %s\n" sel-start)
		(goto-char sel-start))
	    ;; Text is being inserted, so we move point to where it
	    ;; should be relative to the end of the string we got from
	    ;; NaturallySpeaking.  This should work even if keys are
	    ;; bound to multiple characters, and surprisingly enough
	    ;; even if deferred functions have moved point completely!
	    (vr-log "make changes: positioning point relative\n")

	    ;; tricky with invisible text: if we're inserting
	    ;; characters, sel-start is really *relative*to point, and
	    ;; should be shifted as point. Actually, we don't need to
	    ;; shifted at all, since it's just start - sel-start
	    ;; that matters.
	    (goto-char (+ (point)
			  ;(- sel-start (+ start (length text)))))
			  (- (nth 4 vr-request) 
			     (nth 1 vr-request) (length text))))
	    )
	  (delete-overlay mouse-drag-overlay)
	  (if (equal sel-chars 0)
	      (delete-overlay vr-select-overlay)
	    (move-overlay vr-select-overlay
			  sel-start (+ sel-start sel-chars)
			  (current-buffer))))

	;; in any case, we send the replies and the queued changes.
	(vr-log "sending tick\n")
	(vr-send-reply (vr-buffer-tick))
	(vr-send-reply (length vr-queued-changes))
	(mapcar 'vr-send-reply (nreverse vr-queued-changes))
	(if vr-deferred-deferred-function
	    (progn
	      (vr-log "executing deferred function in make-changes: %s\n"
		      vr-deferred-deferred-function)
	      (setq vr-deferred-deferred-deferred-function
		    vr-deferred-deferred-function )
	      (setq vr-deferred-deferred-function nil)
	      (fix-else-abbrev-expansion)
	      (vr-execute-command vr-deferred-deferred-deferred-function)))
	)
    ;; if the current buffer is not VR-buffer
    (vr-send-reply "-1"))
  t)


;; this function looks for vr-invisible text around point.  If it
;; exists, it returns a cons cell of the start and end positions of
;; the invisible text, otherwise nil.
(defun vr-find-invisible ()
  (let ((before-invisible
	 (and (> (point) (point-min))
	      (get-text-property (- (point) 1) 'vr-invisible)))
	(after-invisible
	 (and (< (point) (point-max))
	      (get-text-property (point) 'vr-invisible))))
    (vr-log "entering zap-invisible: point is %d\n" (point))
 
    (if (not (or before-invisible after-invisible)) 
	 ;; point is not touching invisible text, so we don't have to
	 ;; worry
	nil
      ;; it IS touching invisible text.  Investigate further
      (let ((start-invisible (point)) ( end-invisible (point)))
	(if before-invisible
	    (setq start-invisible
		  (or
		   (previous-single-property-change (- (point) 1)
						    'vr-invisible)
		   (point-min))))
	(if after-invisible
	    (setq end-invisible
		  (or
		   (next-single-property-change (point)
						'vr-invisible)
		   (point-max))))

	(cons start-invisible end-invisible)))))

;; This function deals with tricking NaturallySpeaking into thinking
;; that text with the "vr-invisible" property disappears when we start
;; speaking, if it is next to point.
(defun vr-zap-invisible ()
  ;; First we have to find the invisible text, if any.
  (progn
    ;; This hook gives an opportunity to set text as invisible.  The
    ;; hook functions can absolutely not modify actual text!
    (vr-log "running vr-invisible-hook\n")
    (let ((vr-ignore-changes 'unconditionally))
      (run-hooks 'vr-invisible-hook))
    (let*((invisible-region (vr-find-invisible))
	  (start-invisible (and invisible-region
				(car invisible-region)))
	  (end-invisible (and invisible-region
			      (cdr invisible-region)))
	  invisible-string (current-position (point)) )

      (if invisible-region
	  (progn
	    ;; the sub string between start-invisible and end-invisible is
	    ;; what should be hidden from NaturallySpeaking, and have its
	    ;; invisible property marked "removed"
	     (setq invisible-string (buffer-substring start-invisible
						      end-invisible))
	    (vr-log "zap-invisible: start %d end %d \"%s\"\n" start-invisible
		    end-invisible invisible-string)

	    ;; mark this text string as removed
	    (let ((vr-ignore-changes 'unconditionally))
	      (put-text-property start-invisible end-invisible 'vr-invisible
				 'removed))
	    
	    ;; tell NaturallySpeaking it has been removed (we can't
	    ;; actually remove it and reinsert it because that
	    ;; confuses else mode (or any other code watching for
	    ;; changes)
	    (let ((cmd (format "change-text \"%s\" %d %d %d %d %s"
			       (buffer-name)
			       (vr-pfe start-invisible)
			       (vr-pfe end-invisible)
			       0 ;; it's a deletion 
			       (vr-buffer-tick)
			       ;; tick is meaningless -- no it's not
			       "")))
	      (if vr-ignore-changes
		  (setq vr-queued-changes (cons cmd vr-queued-changes))
		(vr-send-cmd cmd)))
	    
	    ; (save-excursion
	     ; (delete-region start-invisible end-invisible)
	      ;(goto-char start-invisible)
	      ;; hide the reinsertion from NaturallySpeaking
	    ;(let ((vr-ignore-changes 'unconditionally))
		;(insert invisible-string)
		;) )
	    ;; set this so restore-invisible knows to look for the removed
	    ;; text
	    (setq vr-floating-invisible t)
	    ;; Emacs and NaturallySpeaking will now disagree on point
	    ;; values, save the displacement info.
	    (setq vr-point-shift (list start-invisible current-position
				       end-invisible)) 
	    ; (goto-char current-position)
	    (vr-log "leaving zap-invisible: point is %d\n" (point))
	    )
	(setq vr-floating-invisible nil))
      )))


;;  calculates the appropriate Emacs buffer position from a vr.exe
;;  position, if we have invisible text flying around.
(defun vr-pte (position)
  (setq position (1+ position))
  (if vr-point-shift
      (let ((start (car vr-point-shift)))
	(setq position 
	      (cond ((eq position start)
		     (nth 1 vr-point-shift))
		    ((> position start ) 
		     (+ position (- (nth 2 vr-point-shift)
				    start)))
		    (t
		     position)))))
  position
)

;;  calculates the appropriate vr.exe buffer position from an Emacs
;;  position, if we have invisible text flying around.
(defun vr-pfe (position)
  (progn
    (if vr-point-shift
	(let ((start (car vr-point-shift)) 
	      (end (nth 2 vr-point-shift))) 
	      (setq position 
		    (cond ((> position end) 
			   (- position (- end start)))
			  ((> position start)
			   start)
			  (t
			   position)))))
    (1- position)))


;; This function restores (whatever is left, nothing if it was an else
;; placeholder) the text that was hidden from NaturallySpeaking when
;; the utterance started.
(defun vr-restore-invisible ()
  (if vr-floating-invisible
      (progn
	(vr-log "entering restore-invisible: point is %d\n" (point))
	;; there is text out there, and we have to find it...
	;; reset the point-shift because it won't be necessary for the
	;; reinsertion of the invisible parts... I hope... ;-|
	(setq vr-point-shift nil)
	(let ((start (point-min)) (current-position (point)))
	  (while (setq start
		       (text-property-any start (point-max)
					  'vr-invisible 'removed))
	    (let ((end (next-single-property-change start
						    'vr-invisible))
		  invisible-string)
	      ;; The string between start and end is marked "removed",
	      ;; and should have that property changed and be
	      ;; unveiled to NaturallySpeaking.
	      (let ((vr-ignore-changes 'unconditionally))
		(put-text-property start end 'vr-invisible nil))

	      (setq invisible-string (buffer-substring start end))

	      (let ((cmd (format "change-text \"%s\" %d %d %d %d %s"
				 (buffer-name)
				 (vr-pfe start)
				 (vr-pfe start)
				 (length invisible-string) 
				 (vr-buffer-tick)
				 (vr-string-replace invisible-string
						    "\n" "\\n"))))
		(if vr-ignore-changes
		    (setq vr-queued-changes (cons cmd vr-queued-changes))
		  (vr-send-cmd cmd)))

	      (vr-log "restore-invisible: start %d end %d \"%s\"\n"
		      start end invisible-string)
	      
	      ;; hide the deletion from NaturallySpeaking
	      ; (save-excursion
		;(let ((vr-ignore-changes 'unconditionally))
		;  (delete-region start end))
	      ;(goto-char start)
		;(insert invisible-string))
	      ;; and update the position to look for more chunks
	      (setq start end)))
	  ;(goto-char current-position)
	  )
	;; reset these 
	(setq vr-floating-invisible nil)
	(vr-log "leaving restore-invisible: point is %d\n" (point))
)))

		       
	      
	

;; This function is called by Dragon when it begins/ends mulling over an
;; utterance; delay key and mouse events until it is done.  This
;; ensures that key and mouse events are not handled out of order
;; with respect to speech recognition events
(defun vr-cmd-recognition (vr-request)
  (let ((state (nth 1 vr-request)))
    (progn
      (vr-log "recognition %s: current buffer: %s vr-buffer:%s\n"
	      state (buffer-name) vr-buffer)
      (cond ((eq state 'begin)
					; if recognition starts and VR
					; buffer is not the current
					; buffer, we might have a
					; potential problem with
					; synchronization.  In that
					; case, let's try calling
					; maybe-activate-buffer and
					; see if it's not already too
					; late.
	     (vr-maybe-activate-buffer (current-buffer))
	     (run-at-time 0 nil 'vr-sleep-while-recognizing)
	     (setq vr-recognizing t))
	    ((eq state 'end)
	     (progn
	       (setq vr-recognizing nil)
	       ;; see if we have a visible text that might need to be
	       ;; fixed
	       (vr-restore-invisible)
	       )) 
	    (t
	     (error "Unknown recognition state: %s" state)))))
  t)
		
(defun vr-output-filter (p s)
  (setq vr-reading-string (concat vr-reading-string s))
  (while (> (length vr-reading-string) 0)
    (let* ((parsed (condition-case err
		       (read-from-string vr-reading-string)
		     ('end-of-file (error "Invalid VR command received: %s"
					  vr-reading-string))))
	   (vr-request (car parsed))
	   (idx (cdr parsed))
	   (vr-cmd (car vr-request)))
      (if vr-log-read
	  (vr-log "-> %s\n" (substring vr-reading-string 0 idx)))
      (setq vr-reading-string (substring vr-reading-string (1+ idx)))

      (cond ((eq vr-cmd 'listening)
	     (run-hook-with-args-until-success 'vr-cmd-listening-hook
					       vr-request))
	    ((eq vr-cmd 'connected)
	     (run-hook-with-args-until-success 'vr-cmd-connected-hook
					       vr-request))
	    ((eq vr-cmd 'initialize)
	     (run-hook-with-args-until-success 'vr-cmd-initialize-hook
					       vr-request))
	    ((eq vr-cmd 'terminating)
	     (run-hook-with-args-until-success 'vr-cmd-terminating-hook
					       vr-request))
	    ((eq vr-cmd 'frame-activated)
	     (run-hook-with-args-until-success 'vr-cmd-frame-activated-hook
					       vr-request))
	    ((eq vr-cmd 'heard-command)
	     (run-hook-with-args-until-success 'vr-cmd-heard-command-hook
					       vr-request))
	    ((eq vr-cmd 'mic-state)
	     (run-hook-with-args-until-success 'vr-cmd-mic-state-hook
					       vr-request))
	    ((eq vr-cmd 'get-buffer-info)
	     (run-hook-with-args-until-success 'vr-cmd-get-buffer-info-hook
					       vr-request))
	    ((eq vr-cmd 'make-changes)
	     (run-hook-with-args-until-success 'vr-cmd-make-changes-hook
					       vr-request))
	    ((eq vr-cmd 'recognition)
	     (run-hook-with-args-until-success 'vr-cmd-recognition-hook
					       vr-request))
	    (t
	     ;; The VR process should fail gracefully if an expected
	     ;; reply does not arrive...
	     (error "Unknown VR request: %s" vr-request))
	    ))))

; use bindat, if available, to pack the messages and get byte order
; correct, since the vr-etonl function doesn't work past v21
(require 'bindat nil t)

(setq vr-length-pack-spec '((length u32) (msg str (length))))


(defun vr-pack-msg (msg)
  (if (featurep 'bindat)
      (bindat-pack vr-length-pack-spec 
		   (list (cons 'length (length msg))
			 (cons 'msg msg)))
    ; fall back to old method
    (let ((i (length msg)))
      (format "%c%c%c%c%s"
	    (lsh (logand i 4278190080) -24)
	    (lsh (logand i 16711680) -16)
	    (lsh (logand i 65280) -8)
	    (logand i 255)
	    msg))))
  
(defun vr-send-reply (msg)
  (if (and vr-dns-cmds (eq (process-status vr-dns-cmds) 'open))
      (progn
	(if (integerp msg)
	    (setq msg (int-to-string msg)))
	(let ((pmsg (vr-pack-msg msg)))
	  (if vr-log-send
	      (progn
		(vr-log "<- r %d '%s'\n" (length msg) msg)
		(vr-log "   packed '%s'\n" pmsg)))
	  (process-send-string vr-dns-cmds pmsg)))
    (message "VR Mode DNS reply channel is not open!")))

(defun vr-send-cmd (msg)
  (if (and vr-emacs-cmds (eq (process-status vr-emacs-cmds) 'open))
      (let ((pmsg (vr-pack-msg msg)))
	(if vr-log-send
	    (progn
	      (vr-log "<- c %d '%s'\n" (length msg) msg)
	      (vr-log "   packed '%s'\n" pmsg)))
  	(process-send-string vr-emacs-cmds pmsg))
    (message "VR Mode command channel is not open: %s" msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subprocess commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-quit ()
  "Turn off VR mode, and cause the VR mode subprocess to exit cleanly."
  (interactive)
  (vr-mode 0))

(defun vr-toggle-mic ()
  "Toggles the state of the Dragon NaturallySpeaking microphone:
off -> on, {on,sleeping} -> off."
  (interactive)
  (vr-send-cmd "toggle-mic"))

(defun vr-show-window ()
  (interactive)
  (vr-send-cmd "show-window"))

(defun vr-hide-window ()
  (interactive)
  (vr-send-cmd "hide-window"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subprocess initialization, including voice commands.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-connect (host port fatal)
  (condition-case e
      (progn
	(setq vr-emacs-cmds (open-network-stream "vr-emacs" nil
						 host port))
	(vr-log "connecting to VR.exe %s" vr-emacs-cmds)
	(setq vr-dns-cmds (open-network-stream "vr-dns" nil host port))
	(set-process-coding-system vr-emacs-cmds 'no-conversion 'no-conversion)
	(set-process-coding-system vr-dns-cmds 'no-conversion 'no-conversion)
	(process-kill-without-query vr-emacs-cmds)
	(process-kill-without-query vr-dns-cmds)
	(set-process-filter vr-dns-cmds 'vr-output-filter)
	(if vr-process
	    (set-process-filter vr-process nil))
	t)
    ('error
      (progn
	(if fatal
	    ;; if fatal is nil, we're just trying to connect to a
	    ;; local server that might be hanging around.  in that
	    ;; case, we will spawn a server, so don't turn off VR
	    ;; mode.
	    (progn
	      (vr-mode 0)
	      (message "VR Mode: cannot connect to %s:%d" host port)))
	nil))))

;; functionp isn't defined in Win 95 Emacs 19.34.6 (!??!?)
(defun vr-functionp (object)
  "Non-nil if OBJECT is a type of object that can be called as a function."
  (or (subrp object) (byte-code-function-p object)
      (eq (car-safe object) 'lambda)
      (and (symbolp object) (fboundp object))))

(defun vr-strip-dash (symbol)
  "Substitute dashes for spaces in the command names."
  (concat (mapcar (lambda (x) (if (eq x ?\-) ?\  x)) (symbol-name symbol))))

(defun vr-startup ()
  "Initialize any per-execution state of the VR Mode subprocess."
  (let ((l (lambda (x)
             (cond ((eq x 'vr-default-voice-commands)
                    (mapcar l vr-default-voice-command-list))
		   ;; evaluate a list of commands if the name of the
		   ;; list is specified
                   ((condition-case e (listp (symbol-value x)) ('error nil))
                    (mapcar l (symbol-value x)))
                   ((symbolp x)
                    (vr-send-cmd
                     (concat "define-command "
                             (vr-strip-dash x) "|" (symbol-name x))))
                   ((and (listp x) (eq (car x) 'list))
                    (vr-send-cmd
                     (format "define-list %s|%s" (nth 1 x) (nth 2 x))))
                   ((and (consp x) (vectorp (cdr x)))
                    (vr-send-cmd
                     (format "define-command %s|%s" (car x) (cdr x))))
                   ((and (consp x) (symbolp (cdr x)))
                    (vr-send-cmd
                     (format "define-command %s|%s" (car x) (cdr x))))
                   ((and (consp x) (stringp (cdr x)))
                    (vr-send-cmd
                     (format "define-command %s|%s" (car x) (cdr x))))
                   (t
                    (error "Unknown vr-voice-command-list element %s"
                           x))
                   )
             )))
    (mapcar l (if (eq vr-voice-command-list t)
                  vr-default-voice-command-list
                vr-voice-command-list)))
  ;; don't set up these hooks until after initialization has succeeded
  (add-hook 'post-command-hook 'vr-post-command)
  (add-hook 'minibuffer-setup-hook 'vr-enter-minibuffer)
  (add-hook 'kill-buffer-hook 'vr-kill-buffer)
  (vr-maybe-activate-buffer (current-buffer))
  (run-hooks 'vr-mode-startup-hook)
  )

(defun vr-kill-emacs ()
  (vr-mode 0)
  (sleep-for 1)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VR Mode entry/exit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-mode (arg)
  "Toggle VR mode.  With argument ARG, turn VR mode on iff ARG is
positive.

VR mode supports Dragon NaturallySpeaking dictation, Select 'N
Say(tm), and voice commands in Emacs buffers.  See README.txt for
instructions.

\\{vr-map}"
  (interactive "P")
  (setq vr-mode
	(if (null arg) (not vr-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if vr-mode
      ;; Entering VR mode
      (progn
	(vr-log "starting VR mode %s" vr-host)
	(setq vr-reading-string nil)
	(setq vr-mic-state "not connected")
	(set-default 'vr-mode-line (concat " VR-" vr-mic-state))
	(setq vr-internal-activation-list vr-activation-list)
	(setq vr-cmd-executing nil)
	(add-hook 'kill-emacs-hook 'vr-kill-emacs)
	(run-hooks 'vr-mode-setup-hook)

	(if vr-host
	    (vr-connect vr-host vr-port t)
	  (if (and vr-port (vr-connect "localhost" vr-port nil))
	      (message "connecting to already running server")
	    (progn
		(setq vr-process (start-process "vr" " *vr*" vr-command
					  "-child"
					  "-port" (int-to-string vr-port)))
		(process-kill-without-query vr-process)
		(set-process-filter vr-process 'vr-output-filter)
		(set-process-sentinel vr-process 'vr-sentinel))))
	)
    
    ;; Leaving VR mode
    (remove-hook 'post-command-hook 'vr-post-command)
    (remove-hook 'minibuffer-setup-hook 'vr-enter-minibuffer)
    (remove-hook 'kill-buffer-hook 'vr-kill-buffer)
    (vr-activate-buffer nil)
    (if vr-host
	 (vr-sentinel nil "disconnect\n")
      (vr-send-cmd "exit")
      (vr-sentinel nil "finished\n")) 
    (run-hooks 'vr-mode-cleanup-hook)
    )
  (force-mode-line-update)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Repeat that N times"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-last-heard-command-request nil
  "If non-nil, the complete, most-recently-received heard-command
message from VR.EXE")

(defun vr-repeat-that-hook (vr-request)
  (let ((cmd (nth 1 vr-request)))
    (if (not (eq cmd 'vr-repeat-that))
	(setq vr-last-heard-command-request vr-request)))
  nil)
(add-hook 'vr-cmd-heard-command-hook 'vr-repeat-that-hook)

(defun vr-repeat-that (num)
  (interactive '(1))
  (if vr-last-heard-command-request
      (progn
	(while (> num 0)
	  (run-hook-with-args-until-success 'vr-cmd-heard-command-hook
					    vr-last-heard-command-request)
	  (setq num (1- num))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repeating commands (based on code by Steve Freund).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-repeat-rate nil
  "The rate at which to repeat commands, in seconds.  If nil, any
currently repeating command will terminate.")

(defun vr-repeat-cmd (freq cmd &rest args)
  "Every FREQ seconds, execute (CMD ARG ...), until the user
generates an input event such as a key press or mouse click (or
executes a voice command that does so).

If the event is RET (the return key), it terminates the repeat but is
then discarded.  Any other event terminates the repeat and is then
acted on as it normally would be."
  (let (ev)
    (discard-input)
    (setq vr-repeat-rate freq)
    (while vr-repeat-rate
      (apply cmd args)
      (sit-for vr-repeat-rate)
      (if (input-pending-p)
	  (progn
	    (setq ev (read-event))
	    (setq vr-repeat-rate nil))))
    (if (and ev (not (eq ev 'return)))
	(setq unread-command-events
	      (cons ev unread-command-events)))
    ))

(defun vr-repeat-mult-rate (f)
  "Multiply the number of seconds between each execution of the current
repeating command by FACTOR."
  (setq vr-repeat-rate (* vr-repeat-rate f)))

(defun vr-repeat-stop (d)
  "Terminate the current repeating command."
  (setq vr-repeat-rate nil))

(defmacro vr-make-repeat-cmd (name freq cmd &rest args)
  "Define an interactive repeating command called NAME that takes no
arguments and, every FREQ seconds, invokes the function CMD.  Uses
vr-repeat-cmd."
  (let ((vrc 'vr-repeat-cmd))
    (list 'defun name '()
	  (format "Invoke %s every %s seconds,\nusing vr-repeat-cmd (which see)."
		  cmd freq)
	  '(interactive)
	  (list 'apply (list 'quote vrc) freq (list 'quote cmd)
		(list 'quote args)))))

(vr-make-repeat-cmd vr-repeat-move-up-s 0.25 previous-line 1)
(vr-make-repeat-cmd vr-repeat-move-up-f 0.05 previous-line 1)
(vr-make-repeat-cmd vr-repeat-move-down-s 0.25 next-line 1)
(vr-make-repeat-cmd vr-repeat-move-down-f 0.05 next-line 1)

(vr-make-repeat-cmd vr-repeat-move-left-s 0.25 backward-char 1)
(vr-make-repeat-cmd vr-repeat-move-left-f 0.05 backward-char 1)
(vr-make-repeat-cmd vr-repeat-move-right-s 0.25 forward-char 1)
(vr-make-repeat-cmd vr-repeat-move-right-f 0.05 forward-char 1)

(vr-make-repeat-cmd vr-repeat-move-word-left-s 0.25 backward-word 1)
(vr-make-repeat-cmd vr-repeat-move-word-left-f 0.05 backward-word 1)
(vr-make-repeat-cmd vr-repeat-move-word-right-s 0.5 forward-word 1)
(vr-make-repeat-cmd vr-repeat-move-word-right-f 0.05 forward-word 1)

(vr-make-repeat-cmd vr-repeat-search-forward-s 0.75 isearch-repeat-forward)
(vr-make-repeat-cmd vr-repeat-search-forward-f 0.25 isearch-repeat-forward)
(vr-make-repeat-cmd vr-repeat-search-backward-s 0.75 isearch-repeat-backward)
(vr-make-repeat-cmd vr-repeat-search-backward-f 0.25 isearch-repeat-backward)

(defun vr-repeat-kill-line (freq)
  "Invoke kill-line every FREQ seconds, using vr-repeat-cmd (which see).
The lines killed with this command form a single block in the yank buffer."
  (kill-new "") 
  (vr-repeat-cmd freq (function (lambda () (kill-line) (append-next-kill)))))

(defun vr-repeat-yank (freq arg)
  "Perform a yank from the kill ring every FREQ seconds, using
vr-repeat-cmd (which see).  This function cycles through the yank
buffer, doing the right thing regardless of whether the previous
command was a yank or not."
  (interactive (list 0.5 (prefix-numeric-value prefix-arg)))
  (vr-repeat-cmd
   freq (function (lambda ()
		    (if (or (eq last-command 'yank) (eq this-command 'yank))
			(yank-pop arg)
		      (yank arg)
		      (setq last-command 'yank))
		    (undo-boundary)
		    ))))

