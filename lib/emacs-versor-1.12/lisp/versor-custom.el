;;; versor-custom.el -- versatile cursor
;;; Time-stamp: <2007-12-11 15:16:28 jcgs>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2006, 2007  John C. G. Sturdy
;;
;; This file is part of emacs-versor.
;; 
;; emacs-versor is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; emacs-versor is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with emacs-versor; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(defgroup versor nil
  "Switch cursor keys between different sorts of movement, and do some high-level editing."
  :group 'convenience
  :prefix "versor-")

;;;;;;;;;;;;;;;;;;;;;;;
;;;; versor motion ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgroup versor-motion nil
  "Control details for Versor movements."
  :group 'versor
  :prefix "versor-")

(defcustom versor-allow-move-to-end-of-last 'dwim
  "*Whether to allow moving to the end of the last sexp in a list.
  Otherwise, versor-next stops at the start of it, and refuses to do
  another forward move.
  Setting this non-nil does what you probably want in practice, although
  setting it nil is probably cleaner in some abstract sort of way.
  Setting it non-nil and not t will make the last move within a list
  go to just before the closing syntax of the list, which is where you
  typically want to be to type the next sexp in."
  :type 'boolean
  :group 'versor-motion)

(defcustom versor-move-out-when-at-end t
  "If non-nil, trying to move further on when already at the end of
  the last thing in a container (see versor-allow-move-to-end-of-last)
  will move to just after the end of the container. Can be convenient in
  practice, although it breaks the symmetry of the next<-->previous
  operations."
  :type 'boolean
  :group 'versor-motion)

(defcustom versor-statement-up-to-next nil
  "Whether to make a statement extend all the way to the start of the next one.
This highlights whitespace, and the author does not like it; but the code can
do it easily enough."
  :group 'versor
  :type 'boolean)

(defcustom versor-level-wrap t
  "*Whether to wrap the level changes.
If this is non-nil, going back from the first level takes you to the last one,
and going forward from the last level takes you to the first one."
  :group 'versor-motion
  :type 'boolean)

(defcustom versor-meta-level-wrap t
  "*Whether to wrap the meta-level changes.
If this is non-nil, going back from the first meta-level takes you to the last one,
and going forward from the last meta-level takes you to the first one."
  :group 'versor-motion
  :type 'boolean)

(defcustom versor-dwim-other-end-count 3
  "*How many times versor-dwim should do versor-other-end-of-item before trying other strategies."
  :group 'versor-motion
  :type 'integer)

(defcustom phrase-end "[,;:] *"
  "*Regular expression to recognize the end of a phrase."
  :group 'versor-motion
  :type 'regexp)

(defcustom versor-show-depth t
  "Whether to put up messages show the parenthesis nesting level of point."
  :group 'versor-motion
  :type 'boolean)

;;;;;;;;;;;;;;;;;
;;;; editing ;;;;
;;;;;;;;;;;;;;;;;

(defgroup versor-editing nil
  "Control of Versor editing behaviour."
  :group 'versor)

(defcustom versor-adjust-whitespace t
  "Whether Versor should adjust whitespace on insertions and deletions."
  :group 'versor-editing
  :type 'boolean)

(defcustom versor-defer-deletions t
  "Whether versor should delay deletions of whitespace until point moves away.
This lets you start typing things in the whitespace, cancelling the deletion."
  :group 'versor-editing
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;
;;;; versor status ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgroup versor-status nil
  "How Versor displays its status information."
  :group 'versor)

(defcustom versor-mode-line-show-both-dimensions t
  "*Whether to show the major dimension as well as the minor dimension, in the mode line.
You might want to turn this off if your mode line is already has a lot in it."
  :type 'boolean
  :group 'versor-status)

(defcustom versor-color-dimension-indicators t
  "Whether to color the names of dimensions.
The colors are used for this are the same as those used for the cursor and selection.
This only works on some versions of Emacs."
  :type 'boolean
  :group 'versor-status)

(defcustom versor-multi-line-level-display (and (boundp 'emacs-major-version)
					     (>= emacs-major-version 21))
  "*Whether to use multi-line indication of the current meta-level and level."
  :type 'boolean
  :group 'versor-status)

(defcustom versor-use-face-attributes
  (and (boundp 'emacs-major-version)
	   (>= emacs-major-version 21))
  "*Whether to use face attributes, as provided from Emacs 21 onwards.
If these are available, it'll normally make sense to use them.
See also versor-highlight-with-brackets."
  :type 'boolean
  :group 'versor-status)

(defcustom versor-highlight-with-brackets (not versor-use-face-attributes)
  "*Whether to use brackets around highlighted items in status feedback.
This is useful if you can't use face attributes (see versor-use-face-attributes)."
  :type 'boolean
  :group 'versor-status)

(defcustom versor-display-highlighted-choice-time 0
  "*How long to show highlighted choices in the echo area."
  :type 'integer
  :group 'versor-status)

(defcustom versor-change-cursor-color t
  "*Whether to use the cursor color to indicate the level.

This refers to the normal GNUemacs cursor rather than the Versor
selection (highlight) cursor.

See versor-item-attribute for the attribute used to change the colour (or other
aspect) of the block cursor."
  :group 'versor-status
  :type 'boolean)

(defcustom versor-item-attribute
  (if (< emacs-major-version 21)
      nil
    '(:background))
  "*An attribute to use to indicate the current item.
This is looked up in the current dimension, to get the value to set it to.
For example, if versor-item-attribute is :background, :background is looked
up in the current dimension, to find the colour to set the background to.
You can only use this from Emacs 21 onwards."
  :type '(set (const :background) (const :foreground) (const :underline))
  :group 'versor-status)

(defcustom versor-describe-selection t
  "Whether Versor should comment much on what it is doing.
If non-nil, Versor will tell you when you have selected a piece of
code that it may have a specific way of handling."
  :type 'boolean
  :group 'versor-status)

(defcustom versor-describe-selection-with-messages nil
  "Whether Versor should use the echo area to describe the selection.
If this is nil, but versor-describe-selection is non-nil, some other
means must be used, such as the header line (which is enabled by
default, using the variable languide-header-line-format.)"
  :type 'boolean
  :group 'versor-status)

(defcustom region-type-description-always t
  "Whether to show the region type even if it is banal."
  :type 'boolean
  :group 'versor-status)

(defcustom versor-describe-selection-hook nil
  "Functions to try to output a selection description.
If any of them returns non-nil, the description is taken
to have been output."
  :type 'hook
  :group 'versor-status)

(defcustom languide-text-word-counters
  '((tex-mode . tex-count-words)
    (latex-mode . tex-count-words))
  "*How versor should announce the number of words in the selection.
This is only meant for text-based modes."
  :type 'alist
  :group 'versor-status)

(defcustom versor-speaking (featurep 'emacspeak)
  "*Whether Versor should use speech output.
This should be set when using it from within emacspeak."
  :type 'boolean
  :group 'versor-status)

(defcustom versor-try-to-display-whole-item t
  "*Whether to try to display the whole item after each movement.
This recenters the text, if possible, so both the start and the end
of it are visible."
  :group 'versor
  :type 'boolean)

(defcustom versor-show-both-ends-of-item
  t

  "*Whether to do special things if needed to show both ends of item after move.
If nil, this is not done.
If 'window, split the window and put one end in each.
If any other value, hide the middle and display
`versor-hiding-middle-banner' before the hidden part."
  :group 'versor
  :type 'boolean)

(defcustom versor-trim-item-starts-to-non-space t
  "*Whether to move the start of the selection so that it is not on the whitespace between items.
To maintain consistency, it generally makes sense to adjust point so
that it's at the start of some non-blank text; most commands will
probably do this anyway, but here we just make sure, which makes it
easier to borrow underlying commands not written specially for versor.
However, it's easy to include a flag to switch this off just in case
anyone prefers it that way."
  :group 'versor
  :type 'boolean)

(defcustom versor-reversible nil
  "*Whether we allow reversing.
This could be useful if you have a very limited number of buttons
available (e.g. a mouthswitch) . It is also useful if you are using
pedals, and cannot use \"shift-next\" for \"previous\". These seem to
work OK on X but not on Windows; not sure about other
platforms/terminals yet."
  :group 'versor
  :type 'boolean)

(defcustom versor-mode-line-begin-string " <"
  "String corresponding to bracket for beginning of Versor markers according to versor-reversed."
  :type 'string
  :group 'versor-status)

(defcustom versor-mode-line-end-string (if versor-reversible "==>" ">")
  "String corresponding to bracket for end of Versor markers according to versor-reversed."
  :type 'string
  :group 'versor-status)

(defface versor-item-face
  ;; (make-face 'versor-item)
  '((t (:inherit 'region)))
  "Face to use for Versor items."
  :group 'versor-status)

(when versor-use-face-attributes
  (set-face-attribute 'versor-item-face nil :inherit 'region))

(unless window-system
  (set-face-attribute 'versor-item-face nil :underline t))

(defcustom versor-display-underlying-commands t
  "*Whether to display the underlying commands that Versor executes.
Useful while you are getting used to Versor, but after that you might
want to turn it off."
  :group 'versor-status
  :type 'boolean)

;;;;;;;;;;;;;;;;;
;;;; general ;;;;
;;;;;;;;;;;;;;;;;

 (defcustom versor-statement-insertion-with-dummy-value nil
   "*Whether Versor statement insertion puts a placeholder value in when adding something.
This tries to avoid changing the semantics, for example, it uses
\"true\" when adding \"and\" or \"if\".
You can then change the value, using the Versor alterations system."
   :type 'boolean
   :group 'versor)

(defcustom versor-reindent-after-insert t
  "*Whether Versor \"around\" insertion re-indents afterwards."
  :type 'boolean
  :group 'versor)

(defcustom versor-per-buffer nil
  "*Whether to remember the dimensions separately for each buffer."
  :type 'boolean
  :group 'versor)

(defcustom versor-auto-change-for-modes t
  "*Whether to change the dimension on changing modes."
  :type 'boolean
  :group 'versor)

(defcustom versor-text-in-code t
  "*Whether Versor should switch dimensions for string literals and comments.
This requires font-lock-mode to be used.

The current dimensions are then remembered separately, and switch to
the last one used in that kind of place, as point moves between code, comments
and strings.
See versor-text-faces for the faces used to recognize this, building on
font-lock-mode."
  :type 'boolean
  :group 'versor)

(defcustom versor-announce-text-in-code t
  "*Whether versor-text-in-code should put up messages telling you when changes the dimension."
  :group 'versor
  :type 'boolean)

(defcustom versor-text-faces '(font-lock-string-face font-lock-comment-face)
  "Faces which Versor regards as being text rather than code.
See versor-text-in-code-function for how this is used."
  :group 'versor)

(defcustom versor-reformat-automatically t
  "If non-nil, some versor movements call reformatting commands."
  :type 'boolean
  :group 'versor)

(defcustom versor-research-live-commentary nil
  "*Whether to make a live commentary on Versor and other activities."
  :type 'boolean
  :group 'versor)

(defcustom versor-indicate-missed-opportunities t
  "*Whether to indicate when you used a non-versor command when a versor command was available.
For this to work, you must have given 'research among the arguments
to versor-setup.
If set to 'all, shows versor commands that are not currently reachable by direct keystrokes.
If set to a number, waits that number of seconds after telling you."
  :type 'boolean
  :group 'versor)

(defcustom versor-research-log-file "~/.versor-log"
  "*File to save Versor's self-recording in."
  :group 'versor
  :type 'file)

(defcustom choices-display-full t
  "*Whether to display in full the range of choices at each level of a treewise chooser."
  :type 'boolean
  :group 'versor)

(defcustom flexi-choose-upstring "[Up]"
  "The label to indicate going back up the tree."
  :type 'string
  :group 'versor)

(defcustom flexi-choose-topstring "[Top]"
  "The label to indicate going straight back up to the top of the tree."
  :type 'string
  :group 'versor)

(provide 'versor-custom)

;;;; end of versor-custom.el
