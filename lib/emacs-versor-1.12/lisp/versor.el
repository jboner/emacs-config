;;; versor.el -- versatile cursor
;;; Time-stamp: <2007-10-07 20:48:59 jcgs>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2005, 2006, 2007  John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2004
;; Keywords: editing

;; This file is NOT part of GNU Emacs.

;; This file is part of emacs-versor.
;;
;; emacs-versor is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
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



;;; Commentary:
;;

(require 'tempo)
(require 'cl)
(require 'versor-custom)
(require 'versor-dimensions)
(require 'versor-status)
(require 'versor-selection)
(require 'versor-base-moves)
(require 'versor-commands)
(require 'versor-help)
(require 'versor-alter-item)		; should probably autoload
(require 'versor-containers)		; should probably autoload
(require 'versor-chop-chop)		; should probably autoload
(require 'versor-dwim)			; should probably autoload

;; todo: command to move to end of container (possibly improved semantics for versor-end)
;; todo: command to toggle between code and string literals
;; todo: fix change of dimensions that happens after type-break uses the minibuffer (probably more general than this)
;; todo: make mouse clicks and drags select and extend using the current versor dimensions

;;; Code:
(defvar version-version "1.09"
  "The version number for this release of versor.")

;;;;;;;;;;;;;;;
;;;; hooks ;;;;
;;;;;;;;;;;;;;;

(defvar versor-start-hooks nil
  "*Hooks for `versor-start'.
If one of these returns non-nil, it is taken as having done the action.")

(defvar versor-prev-hooks nil
  "*Hooks for `versor-prev'.
If one of these returns non-nil, it is taken as having done the action.")

(defvar versor-next-hooks nil
  "*Hooks for `versor-next'.
If one of these returns non-nil, it is taken as having done the action.")

(defvar versor-end-hooks nil
  "*Hooks for `versor-end'.
If one of these returns non-nil, it is taken as having done the action.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; autoloads for structured text ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'nested-blocks-backward "nested-blocks"
  "Move back over a nested block. Like backward-sexp but more general."
  t)

(autoload 'nested-blocks-forward "nested-blocks"
  "Move forward over a nested block. Like forward-sexp but more general."
  t)

(autoload 'nested-blocks-enter "nested-blocks"
  "Into the next nested block forwards."
  t)

(autoload 'nested-blocks-leave "nested-blocks"
  "Move forwards out of current nested block."
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; autoloads for language-guided navigation ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'navigate-this-head "languide"
  "Navigate to the head of the relevant statement."
  t)

(autoload 'statement-navigate-parts-previous "languide"
  "Navigate to the previous part of the statement."
  t)

(autoload 'statement-navigate-parts-next "languide"
  "Navigate to the next part of the statement."
  t)

(autoload 'navigate-this-body "languide"
  "Navigate to the body of the relevant statement."
  t)

(autoload 'latest-statement-navigation-end "languide"
  "Return the end of the latest statement navigation result.
Optional argument for compatibility with other things that get the versor
package to the end of an item."
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; autoload for demo ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'versor-demo "versor-demo"
  "Demonstrate versor." t)

;;;;;;;;;;;;;;;
;;;; Setup ;;;;
;;;;;;;;;;;;;;;

;; To use versor-
;;   add the directory containing this elisp file, and its companions, to your load-path
;;   load this file (or autoload versor-setup)
;;   call versor-setup
;; If you want to use navigation by statements, you will need languide too.

(defvar versor-insertion-placement-keymap (make-sparse-keymap "Versor insert place")
  "Keymap for reading what place to do insertion at.
This is for choosing before, after, around or inside.")

(fset 'versor-insertion-placement-keymap versor-insertion-placement-keymap)

(defvar versor-altering-mode-map (make-sparse-keymap "Versor alter item")
  "Keymap for altering the selected item.")

(fset 'versor-altering-mode-map versor-altering-mode-map)

(defvar versor-original-bindings-map
  (make-sparse-keymap "Versor non-versor bindings")
  "A keymap holding the key bindings that Versor displaced.")

(defun versor-global-set-key (key command)
  "Like `global-set-key', for KEY and COMMAND.
If the old binding was not a versor one, save the old binding."
  (let ((old-value (lookup-key (current-global-map) key t)))
    (when (and (symbolp old-value)
	       (not (string-match "^versor-" (symbol-name old-value))))
      (message "Versor changing binding of %s from %s to %s"
	       (key-description key)
	       (symbol-name old-value)
	       (symbol-name command))
      (define-key versor-original-bindings-map key old-value)))
  (global-set-key key command))

(defvar versor-mode t
  "*Whether Versor is enabled (as a minor mode).")

(defun versor-mode (arg)
  "Toggle Versor mode.
With arg, turn Versor mode on if and only if arg is positive."
  (interactive "P")
  (setq versor-mode (if (null arg)
			(not versor-mode)
		      (> (prefix-numeric-value arg) 0)))
  (when (and versor-mode
	     (not versor-setup-done))
    (versor-setup))
  (if versor-mode
      (versor-enable-mode-line-display)
    (versor-disable-mode-line-display))
  (force-mode-line-update))

(defun versor-enable-mode-line-display ()
  "Put the Versor status indicators into the mode line format."
  (unless (memq 'versor-current-level-name global-mode-string)
    (setq global-mode-string
	  (append global-mode-string
		  '(""		; needed to stop fancy control actions
					; in the rest of this list
		    versor-mode-line-begin-string
		    versor-current-meta-level-name ":")
		  (if versor-mode-line-show-both-dimensions
		      '(versor-current-over-level-name "/")
		    nil)
		  '(versor-current-level-name
		    versor-mode-line-end-string)))))

(defun versor-disable-mode-line-display ()
  "Remove the Versor status indicators from the mode line format."
  (when (memq 'versor-current-level-name global-mode-string)
    (let ((rest global-mode-string))
      (while rest
	(if (eq (cadr (cdr rest)) 'versor-mode-line-begin-string)
	    (progn
	      (rplacd rest (nthcdr
			    (if (memq 'versor-current-over-level-name rest)
				9
			      7)
			    rest))
	      (setq rest nil))
	  (setq rest (cdr rest)))))))

(defvar versor-tracking nil
  "Whether Versor is tracking.
Use `versor-toggle-tracking' to set this.")

(defun versor-toggle-tracking (arg)
  "Toggle versor tracking.
With no argument, just toggle it. With an argument, switch off if the
argument is negative, otherwise switch on."
  (interactive "p")
  (setq versor-tracking
	(if arg
	    (if (< (prefix-numeric-value arg) 0)
		nil
	      t)
	  (not versor-tracking)))
  (if versor-tracking
      (progn
	(require 'versor-tracking)
	(add-hook 'post-command-hook 'versor-tracking-hook))
    (remove-hook 'post-command-hook 'versor-tracking-hook)))

(defun keypad-separate ()
  "Remove mappings of keypad keys."
  (interactive)
  (let* ((holder (cons nil (cdr function-key-map)))
	 (pairs holder)
	 (nextpairs (cdr pairs)))
    (while pairs
      (let* ((pair (car nextpairs))
	     (name (car pair)))
	(if (and (symbolp name)
		 (string-match "kp-" (symbol-name name)))
	    (progn
	      (message "Unmapping %S from function-key-map" name)
	      (rplacd pairs (cdr nextpairs))
	      (setq nextpairs (cdr pairs)))
	  (progn
	    (setq pairs (cdr pairs)
		  nextpairs (cdr pairs))))))
    (rplacd function-key-map (cdr holder))))

(defvar versor-setup-done nil
  "Whether Versor has initialized itself yet.")

(defun versor-setup (&rest keysets)
  "Set up the versor (versatile cursor) key system, as given by KEYSETS.

This rebinds the cursor (arrow) keys in such a way that they can move
the cursor in other ways than just cartesian (up/down,
left/right).  Instead, they take a pair of adjacent dimensions in a
series of dimensions (such as characters/lines or lines/pages).

Several such series (meta-dimensions) of dimensions are available.  The
vertical arrow keys are always for the more significant of the two
dimensions, just as they are in the normal key bindings.

Meta-arrows change the dimensions.  M-left and M-right shift the pair
of dimensions within the current series, and M-up and M-down change
which series you are in.  The cursor colour indicates which dimension
is selected, and the current meta-dimension and dimension are shown
in the mode line.

The current item is highlighted after each versor move.  The
highlighting is removed at the next command, so versor can co-operate
unobtrusively with traditional character-based editing.

DEL deletes the current item.

The meta-dimensions and dimensions defined by default are:
  cartesian (char, line, page)
  structural (char, expression, expression depth, defun)
  natural language oriented (char, word, phrase, sentence, paragraph)
  tabular text (HTML table rows and cells)
  programming language oriented (expression, part of statement, statement)

The arguments can be any (combination) of
  'arrows        -- for the main cursor keys
  'arrows-misc   -- for insert, delete etc
  'keypad        -- for the keypad cursor keys
  'keypad-misc   -- for keypad insert, delete etc
to select which keys are set up to do versor commands.

You can turn on further facilities by including the following symbols
amongst the arguments:
  'modal         -- remember a different dimension for each mode
  'local         -- remember a different dimension for each buffer
  'text-in-code  -- switch dimensions for string literals and comments,
                    allowing code-oriented movement in actual code, and
                    text-oriented movement in embedded natural language text
  'joystick      -- define joystick actions (needs joystick.el)
  'menu          -- define a menu of versor commands
  'mouse         -- define the mouse button to make a versor selection of
                    the appropriate dimension
  'verbose       -- blather about what it is doing

The following arguments suppress some of the default behaviours:
  'use-region-face  -- suppress the selection colour changes
  'quiet-underlying -- do not indicate the underlying commands being used
See the info pages for more details of versor."
  (interactive)

  (when (null keysets) (setq keysets '(arrows arrows-misc)))
  (when (and (not (memq 'meta keysets))
	     (not (memq 'ctrl-x keysets)))
    (setq keysets (cons 'ctrl-x keysets)))
  ;; todo: add setting of versor-meta-dimensions-valid-for-modes, and add to info file, and document this

  (let ((modal (memq 'modal keysets)))
    (when modal
      (require 'versor-local)
      (message "Setting up versor to use auto-change for modes")
      (setq versor-auto-change-for-modes t))

    (when (memq 'local keysets)
      (when modal (error "Versor-setup options 'modal 'local conflict"))
      (require 'versor-local)
      (message "Setting up versor to use per-buffer settings")
      (setq versor-per-buffer t)))

  (when (memq 'text-in-code keysets)
    (require 'versor-text-in-code)
    (message "Setting up versor to use separate settings for text in code")
    (setq versor-text-in-code t))

  (when (memq 'menu keysets)
    (message "Setting up versor to use menu")
    (require 'versor-menu))

  (when (memq 'verbose keysets)
    (message "Setting up versor to be verbose")
    (setq versor-verbose t))

  (when (memq 'research keysets)
    (require 'versor-research)
    (message "Setting up versor to log research data")
    (versor-research-start))

  (when (memq 'joystick keysets)
    (require 'versor-joystick)
    (message "Setting up versor to use joystick")
    (versor-joystick-setup))

  (when (memq 'mouse keysets)
    (require 'versor-tracking)
    (message "Setting up versor to use mouse tracking")
    (versor-mouse-setup))

  (unless (memq 'use-region-face keysets)
    (message "Setting up versor to use the region face")
    (setq versor-item-attribute :background))

  (when (memq 'quiet-underlying keysets)
    (message "Setting up versor not to display underlying commands")
    (setq versor-display-underlying-commands nil))

  (when (memq 'tlc keysets)
    (message "Setting up versor to use tlc")
    (require 'versor-tlc))

  (let ((tracking (memq 'tracking keysets)))
    (when tracking
      (message "Setting up versor to use keyboard tracking")
      (versor-toggle-tracking 1))

    ;; This is the separate set of arrows, not those overlaid on the
    ;; numeric keypad
    (when (memq 'arrows keysets)
      (message "Setting up versor to use main arrow keys")
      (unless tracking
	(versor-global-set-key [ left ]    'versor-prev)
	(versor-global-set-key [ right ]   'versor-next))
      (when (member 'meta keysets)
	(versor-global-set-key [ M-left ]  'versor-out)
	(versor-global-set-key [ M-right ] 'versor-in))
      (when (member 'ctrl-x keysets)
	(versor-global-set-key [ ?\C-x left ]  'versor-out)
	(versor-global-set-key [ ?\C-x right ] 'versor-in))
      (versor-global-set-key [ C-left ]  'versor-extend-item-backwards)
      (versor-global-set-key [ C-right ] 'versor-extend-item-forwards)

      (unless tracking
	(versor-global-set-key [ up ]      'versor-over-prev)
	(versor-global-set-key [ down ]    'versor-over-next))
      (when (member 'meta keysets)
	(versor-global-set-key [ M-up ]    'versor-prev-meta-level)
	(versor-global-set-key [ M-down ]  'versor-next-meta-level)
	(versor-global-set-key "\C-x?" 'versor-describe-selection))
      (when (member 'ctrl-x keysets)
	(versor-global-set-key [ ?\C-x up ]    'versor-prev-meta-level)
	(versor-global-set-key [ ?\C-x down ]  'versor-next-meta-level)
	(versor-global-set-key "\C-x?" 'versor-describe-selection))
      (versor-global-set-key [ C-up ]    'versor-over-start)
      (versor-global-set-key [ C-down ]  'versor-over-end)

      (define-key versor-altering-mode-map [ left ] 'versor-alter-item-prev)
      (define-key versor-altering-mode-map [ right ] 'versor-alter-item-next-value)
      (define-key versor-altering-mode-map [ up ] 'versor-alter-item-over-prev)
      (define-key versor-altering-mode-map [ down ] 'versor-alter-item-over-next)
      )

    (when (memq 'arrows-misc keysets)
      ;; todo: I think I can make better use of these -- perhaps put versor-begin-altering-item and versor-dwim on them?
      (require 'languide-keymap) ; I don't think you can autoload keymaps, but perhaps I should try?
      (message "Setting up versor to use main auxiliary keys")
      (versor-global-set-key [ prior ]   'versor-over-over-prev)
      (versor-global-set-key [ next ]    'versor-over-over-next)
      (versor-global-set-key [ C-prior ] 'versor-over-over-start)
      (versor-global-set-key [ C-next ]  'versor-over-over-end)

      (versor-global-set-key [ home ]    'versor-start)
      (versor-global-set-key [ end ]     'versor-end)
      (versor-global-set-key [ C-home ]  'versor-start-of-item)
      (versor-global-set-key [ C-end ]   'versor-end-of-item)

      (versor-global-set-key [ M-home ] 'versor-dwim)

      ;; todo: why isn't this a normal global-set-key?
      (define-key (current-global-map) [ insert ]   'versor-insertion-placement-keymap)
      (versor-global-set-key [ M-insert ] 'versor-begin-altering-item)
      (define-key global-map [ C-insert ] 'languide-map)
      (define-key versor-altering-mode-map [ insert ] 'versor-end-altering-item)
      (define-key versor-altering-mode-map [ return ] 'versor-end-altering-item)

      (versor-global-set-key [ delete ]   'versor-kill)
      (versor-global-set-key [ M-delete ] 'versor-copy)
      (versor-global-set-key [ C-delete ] 'versor-select-surrounding)
      (define-key versor-altering-mode-map [ delete ] 'versor-abandon-altering-item)

      (define-key versor-insertion-placement-keymap [ left ]  'versor-insert-before)
      (define-key versor-insertion-placement-keymap [ right ] 'versor-insert-after)
      (define-key versor-insertion-placement-keymap [ up ]    'versor-insert-around)
      (define-key versor-insertion-placement-keymap [ down ]  'versor-replace)
      )

    (when (memq 'keypad keysets)
      (message "Setting up versor to use keypad")
      (keypad-separate)
      (unless tracking
	(versor-global-set-key [ kp-left ]    'versor-prev)
	(versor-global-set-key [ kp-right ]   'versor-next))
      (when (memq 'meta keysets)
	(versor-global-set-key [ M-kp-left ]  'versor-out)
	(versor-global-set-key [ M-kp-right ] 'versor-in))
      (when (memq 'ctrl-x keysets)
	(versor-global-set-key [ ?\C-x kp-left ]  'versor-out)
	(versor-global-set-key [ ?\C-x kp-right ] 'versor-in))
      (versor-global-set-key [ C-kp-left ]  'versor-start)
      (versor-global-set-key [ C-kp-right ] 'versor-end)
      (versor-global-set-key [ kp-home ]    'versor-start)
      (versor-global-set-key [ kp-end ]     'versor-end)
      (versor-global-set-key [ C-kp-home ]  'versor-start-of-item)
      (versor-global-set-key [ C-kp-end ]   'versor-end-of-item)
      (versor-global-set-key [ M-kp-home ] 'versor-dwim)

      (unless tracking
	(versor-global-set-key [ kp-up ]      'versor-over-prev)
	(versor-global-set-key [ kp-down ]    'versor-over-next))
      (when (member 'meta keysets)
	(versor-global-set-key [ M-kp-up ]    'versor-prev-meta-level)
	(versor-global-set-key [ M-kp-down ]  'versor-next-meta-level))
      (when (member 'ctrl-x keysets)
	(versor-global-set-key [ ?\C-x kp-up ]    'versor-prev-meta-level)
	(versor-global-set-key [ ?\C-x kp-down ]  'versor-next-meta-level))
      (versor-global-set-key [ C-kp-up ]    'versor-over-start)
      (versor-global-set-key [ C-kp-down ]  'versor-over-end)

      (define-key versor-altering-mode-map [ kp-up ] 'versor-alter-item-over-prev)
      (define-key versor-altering-mode-map [ kp-down ] 'versor-alter-item-next-type)
      (define-key versor-altering-mode-map [ kp-left ] 'versor-alter-item-prev)
      (define-key versor-altering-mode-map [ kp-right ] 'versor-alter-item-next-value)

      (versor-global-set-key [ kp-prior ]   'versor-over-over-prev)
      (versor-global-set-key [ kp-next ]    'versor-over-over-next)
      (versor-global-set-key [ C-kp-prior ] 'versor-over-over-start)
      (versor-global-set-key [ C-kp-next ]  'versor-over-over-end))

    (when (memq 'keypad-misc keysets)
      (message "Setting up versor to use keypad auxiliary keys")
      (keypad-separate)
      (versor-global-set-key [ kp-enter ]  'versor-copy)
      (define-key (current-global-map)
	[ kp-insert ] 'versor-insertion-placement-keymap)
      (versor-global-set-key [ kp-delete ] 'versor-kill)
      (versor-global-set-key [ kp-add ]    'other-window)

      (define-key (current-global-map) [ C-kp-delete ]   'versor-begin-altering-item)
      (define-key global-map [ C-kp-insert ] 'languide-map)

      (define-key versor-insertion-placement-keymap [ kp-left ]  'versor-insert-before)
      (define-key versor-insertion-placement-keymap [ kp-right ] 'versor-insert-after)
      (define-key versor-insertion-placement-keymap [ kp-up ]    'versor-insert-around)
      (define-key versor-insertion-placement-keymap [ kp-down ]  'versor-replace)
      ))

  (setq versor-mode t)

  (eval-after-load "info"
    '(add-to-list 'Info-file-list-for-emacs "versor"))

  (versor-enable-mode-line-display)

  (versor-set-status-display)

  (setq versor-setup-done t))

(defvar versor-latest-spoken-message ""
  "The most recent message to be spoken from Versor.")

(defun versor-speak (format-string &rest args)
  "If `versor-speaking' is non-nil, say FORMAT-STRING, using it to format ARGS."
  (let* ((msg (apply 'format format-string args)))
    (setq versor-latest-spoken-message msg)
    (when versor-speaking
      (dtk-speak msg))))

;;  a little convenience for when editing the source code of versor
(put 'versor-as-motion-command 'lisp-indent-function 1)

(put 'versor-as-versor-command 'lisp-indent-function 0)

(provide 'versor)

;;; versor.el ends here
