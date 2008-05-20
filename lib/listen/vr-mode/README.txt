VR Mode - integration of GNU Emacs and Dragon NaturallySpeaking.
	Available at http://emacs-vr-mode.sourceforge.net.

Copyright 1999 Barry Jaspan, <bjaspan@mit.edu>.  All rights reserved.
See the file COPYING.txt for terms of use. (The file abbrev-cmds is
Copyright 2000 Hans van Dam <hans_van_dam@hetnet.nl>.)

TABLE OF CONTENTS

	Introduction
	Technical support
	Installation
	Using VR Mode
		Getting started
		Voice-activating buffers
		Voice commands
			Standard voice commands
			Custom voice commands
			Lists as command arguments
			Repeating commands
		Using VR Mode over the network
		VR Mode key bindings
	Extending VR Mode

INTRODUCTION

VR Mode integrates the features of Dragon NaturallySpeaking with GNU
Emacs.  You must already have and be familiar with NaturallySpeaking
3.52 and GNU Emacs 19.34 (or newer versions) in order to use VR mode.
However, there are problems with Emacs 21.x.

VR Mode is implemented as an Emacs "minor mode" that can be enabled in
any major mode.  With VR Mode, you can:

- dictate continuously into the microphone and have your recognized
text appear in Emacs;

- use Select 'N Say(tm) with Emacs, including the "Scratch
That/<text>" and "Correct That/<text>" voice commands just as with the
Dragon NaturallySpeaking "speakpad";

- use VR Mode in multiple Emacs buffers and the minibuffer
simultaneously while preserving the text/voice mapping that enables
the Correction Dialog to work properly;

- execute Emacs commands such as "find file" and "switch to buffer" by
voice;

- use continuous dictation commands to execute Emacs commands in the
  middle of dictation (this feature is from the file pbvlisp.el by
  Hans van Dam, now included in VR mode as abbrev-cmds.el, and is
  copyright him.)

- and, intersperse any of the above features with using keyboard to
make arbitrary changes to the Emacs buffers.

VR Mode works with Emacs running on the same computer as
NaturallySpeaking, or with Emacs running on a different computer that
communicates with your NaturallySpeaking over the network.  This means
(e.g.) that you can use NaturallySpeaking in an Emacs running on Unix.

Please read RELEASE-NOTES.txt before attemping to use this package.

TECHNICAL SUPPORT

VR Mode is free software, and is provided as is, so there is no real
tech support.  That said, we are of course interested in making VR
mode as useful as possible, and would like to hear about any
problems/bugs people encounter.  Please use the e-mail list
emacs-vr-mode-users, or contact Patrik directly
(grifgrif@users.SourceForge.net).

INSTALLATION

To install VR Mode, you must already have NaturallySpeaking 3.52 and
GNU Emacs 19.34 installed on your Windows 95/98/NT/2000 system (see
below for using VR Mode with Emacs running on non-Windows platforms).
Then:

1.  If you do not have the DNS SDK installed, run DNSRDK.EXE to
install the necessary Dragon run-time components for applications
developed with the DNS SDK (e.g. VR Mode).  DNSRDK.EXE is available
on the VR Mode homepage.

2.  Extract the Zip file to the location of your choice
(e.g. c:\Program Files\VRMode).

3.  Instruct Emacs how and when to load the file "vr.el" and instruct
VR mode where to find the subprocess executable VR.EXE.  To do this,
add the following text to your .emacs file:

(add-to-list 'load-path (substitute-in-file-name "<vr-path>"))
(autoload 'vr-mode "<vr-path>/vr" "" t nil)
(setq vr-command "<vr-path>/vr.exe")

using "/" as the directory separator.

You also need to tell VR Mode the Windows class name and/or the title
of the window inside which Emacs is running (for example, the Telnet
or SSH window).  To do so,

	(setq vr-win-class "Windows class name")
	(setq vr-win-title "Window title")

You can use one or both of vr-win-class or vr-win-title.  VR.EXE will
choose the first window it finds for which vr-win-class and
vr-win-title (or only one of them, if only one is specified) is a
substring of the actual Windows class name or title. The Windows class
name is normally identical to the name of the executable,
i.e. "Emacs", unless you are running a remote Emacs over X Windows or
in a terminal window.  More on running remote Emacs's below.

USING VR MODE


* Getting Started

To enable VR mode, run the command M-x vr-mode, which runs the VR Mode
subprocess, VR.EXE.  If NaturallySpeaking is not already running,
VR.EXE will start it and load the most recently used speaker.  If you
wish to use a different speaker, run NaturallySpeaking by hand first.

VR Mode can voice activate any emacs buffer (see below).  VR mode
indicates that it is running via the Emacs mode line, which will
contain the text "VR<micstate>".  <micstate> is defined as follows:

- The first character is a colon (":") if the current buffer is
currently voice activated, and it is a hyphen ("-") otherwise.

- The remainder of <micstate> is either "on", "off", or "sleep",
according to the current state of the global Dragon NaturallySpeaking
microphone object in the Windows system tray.


* Voice-activating buffers

VR Mode defines a "voice activated" buffer as one into which it will
insert dictated text.  Zero or one buffers are voice activated at a
time.  Each time Emacs' current buffer changes, VR Mode deactivates
voice in the previous buffer and, if appropriate, activates voice in
the new buffer.

By default, no regular buffers are automatically voice activated.
When visiting a buffer, type C-c v B (or M-x
vr-add-to-activation-list) to voice activate the current buffer for
the duration of the current session only.

If the variable vr-activate-minibuffer is non-nil (the default), the
minibuffer is always automatically voice activated when it is in use.
If vr-activate-minibuffer is nil, it is not voice activated.

VR Mode uses vr-activation-list to decide which regular buffers to
voice activate automatically.  vr-activation-list is a list of buffer
name patterns which VR Mode will voice activate.  If a list element is
a string, it is interpreted as a REGEXP and any buffer whose name
matches it is voice activated.  If a list element is a list whose
first element is the symbol 'not, the second element is interpreted as
a REGEXP and any buffer whose name matches it is not voice activated.
'not elements take priority over non-'not elements.

For example, if your .emacs file contains

	(setq vr-activation-list '("^\*scratch\*$" 
				   "\.txt$")
				   (not "^no-voice\.txt$"))


the buffer named "*scratch*" and any buffer whose name ends with
".txt" will be voice-activated, except for the buffer named
"no-voice.txt".  If your .emacs file contains

	(setq vr-activation-list '((not "^\*info\*$") ".*"))

then all buffers except "*info*" (Info mode) will be voice activated.


* Voice commands

Any Emacs command (i.e. a keystroke or something that can be executed
with M-x) can be voice-activated, which means that VR Mode will
execute the command when you speak its voice-name.  Voice activated
commands work even when the current buffer is not voice-activated for
dictation; this means, for example, that you can always say "find
file" to visit a new file even if the current buffer is not
voice-activated.  And, if the minibuffer is voice activated (see
above), you can dictate command arguments (such as the file name to
load) into it.

** Standard voice commands

The Emacs variable vr-voice-command-list defines which Emacs commands
can be executed by voice.  The variable vr-default-voice-command-list
contains a standard list of voice commands provided by VR Mode.  To
use the default commands, add the following line to your .emacs file:

(setq vr-voice-command-list '(vr-default-voice-command-list))

The default commands are XXX a long list.  Just look in vr.el for now.

** Custom voice commands

You can create additional voice commands by adding new elements to
vr-voice-command-list.  Each element of the list can be a symbol or a
CONS cell.  Here's an example:

	(setq vr-voice-command-list
		'(vr-default-voice-command-list
		  find-file
		  ("forward expression" . forward-sexp)
		  ("describe key" . [?\C-h ?k])))

An element that is a symbol, such as vr-default-voice-command-list,
can be a list of elements that will themselves be treated as if they
were on vr-voice-command-list.  vr-default-voice-command-list is a
collection of common file, buffer, window, frame, editing, and
movement commands that most people should use (see Standard voice
commands, above).

An element that is a symbol with a command definition, such as
find-file, is activated by speaking its name without hyphens
(e.g. "find file"), and executes the symbol's command.

For an element that is a CONS cell whose first part (the CAR) a
string, that string is the "voice name" of the command in the second
part (the CDR).  The CDR can be a command, like forward-sexp (whose
name might otherwise be hard to pronounce), or a vector of keystrokes
to be typed (in this example, the keystrokes C-h k).

** Lists as command arguments

vr-voice-command-list can contain list definitions that can then be
used to specify arguments to voice commands.  A list is defined by an
element of vr-voice-command-list that is a list, whose first element
is the symbol 'list, whose second element is the name of the list, and
whose third element is the space-separated values of the list.  The
list can then be used in a voice command entry by enclosing the list
name in angle brackets (< and >).

List elements that look like numbers are passed to the invoked Emacs
command as numbers, and list elements that do not look like numbers
are passed as strings.

For example,

	(setq vr-voice-command-list
		'((list "1to3" "1 2 3")
		  ("move down <1to3>" . next-line)))

createse the voice commands "move down 1" through "move down 3" which
moves the cursor down 1, 2, or 3 lines, by passing a numeric argument
to the Emacs command next-line.

** Repeating commands

Commands can be repeated.  Look at the command list in vr.el, and you
should be able to figure out how it works.


* Abbreviation commands (continuous dictation commands)

For some commonly used commands its desirable to not have to pause
between utterances to have NaturallySpeaking recognize it is a
command.  The continuous commands use Emacs's abbrev feature to run
commands when a certain string is typed into the buffer.  Look in the
file abbrev-cmds.el, and you will see definitions looking like this:

    ("forwardchar" "" (lambda ()  (deferred-execution 'forward-char t))
     0)

This defines an Emacs abbrev "forwardchar", so that when this is typed
the command forward-char will be executed. The next step is to start
the NaturallySpeaking Vocabulary Editor and add a word with written
form "*forwardchar*" and spoken form "forward char", setting its
properties to "no preceding or following space".  The idea is that
when you say "forward char", NaturallySpeaking will type
"*forwardchar*" into the buffer, and Emacs will recognize this as an
abbreviation for running the command forward-char.  (The asterisks are
necessary because you might be saying this in the middle of a word and
the abbrev has to be surrounded by non-alphanumeric characters.  The
asterisks are deleted.)

The end result of all this is that you can say things like "tab-key
next line tab-key forward word kill word hello next line beginning of
line" without stopping.  There are drawbacks -- you can't "scratch
that", and if any part of the utterance is not recognized correctly,
the results are unpredictable.  But overall, it's a very useful
feature.

To enable this, but the following in your .emacs file:

(setq-default abbrev-mode t)
(load "abbrev-cmds")

(VR mode will do the last step, so you only need to manually load the
file if you want to use the abbreviations before starting VR mode.)
 

* Using VR Mode over the network

In the default configuration, VR Mode expects to run on the same
computer as NaturallySpeaking.  It automatically starts a new VR.EXE
process on the local computer and uses it.

You can, instead, tell VR Mode to use a VR.EXE on a different computer
accessible over the network.  To do so, first start
NaturallySpeaking.  Then, start VR.EXE on the same computer as
NaturallySpeaking and specify a TCP port for it to listen on; for
example,

	VR.EXE -port 1234

Then, on the remote computer on which you want to run VR Mode inside
Emacs, put the following lines in your .emacs file:

	(setq vr-host "<host running DNS/VR.EXE>")
	(setq vr-port 1234)

When you start VR Mode, it will connect to the remote VR.EXE on your
Windows computer.  You still need your microphone plugged in to the
soundcard on your Windows computer running NaturallySpeaking.  

A single VR.EXE can handle multiple client connections at a time.

Note that if you are running both local and remote Emacses, you can
have the local Emacs start VR.EXE by not setting vr-host, and then
having the remote instances connect to that same process.  Just make
sure you have the same setting for vr-port in all instances of Emacs,
and that they can be uniquely identified using the Windows class and
title settings. 


* Key bindings

Whenever VR Mode is enabled, the following commands are available in
any buffer.  All the keystrokes in the table below must follow the
prefix "C-c v":

Key	M-x command		Description
=======	=======================	=====================================
q	vr-quit			Disable VR mode and terminate the VR
				subprocess cleanly. 
m	vr-toggle-mic		Toggle the state of the Dragon 
				NaturallySpeaking microphone.
B	vr-add-to-activation-list  Voice activates the current buffer
				for the current session only.
w s	vr-show-window		Show the VR Mode Status window.
w h	vr-hide-window		Hide the VR Mode Status window.

To get an idea how VR mode works, type the command "C-c v w s" to show
the VR Mode Status window (note that it may appear behind the Emacs
window).  The trace window only shows protocol events that occur while
it is displayed.  Use the command "C-c v w h" to hide the window.

EXTENDING VR MODE

XXX Use the hooks! 
