;;;; versor-tlc.el -- Two-(or Three-)Letter Commands for versor (and TLC for your hands, perhaps)
;;; Time-stamp: <2007-07-04 18:09:08 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;;; This file defines a two-letter modal command system.

;;; In the command mode, printing characters generally do commands
;;; rather than self-insert. It's possible to switch quickly between
;;; command mode and normal emacs mode. Yes, a bit like you-know-what,
;;; in one sense, but the commands here include some high-level ones,
;;; and the author hopes that with such commands, it will be possible
;;; to move towards a style of editing in which typing individual
;;; characters is an occasional oddity rather than the normal way of
;;; working. This should drastically reduce the amount of chording
;;; required, which will probably be better for the hands.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Keyboard layout switching ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar keyboard-layout-qwerty
  [(["`" "1" "2" "3" "4" "5"]  .  ["6" "7" "8" "9" "0" "-" "="])
   (["\t" "q" "w" "e" "r" "t"] . ["y" "u" "i" "o" "p" "[" "]" "\\"])
   (["a" "s" "d" "f" "g"] . ["h" "j" "k" "l" ";" "'" "\"" "\r"])
   (["z" "x" "c" "v" "b"] . ["n" "m" "," "." "/"])]
  "The keyboard layout for QWERTY keyboards.
The split between left and right hands corresponds to that of
the \"Comfort Keyboard\", a brand of split keyboard.")

(defvar keyboard-layout-dvorak
  [(["`" "1" "2" "3" "4" "5"]  .  ["6" "7" "8" "9" "0" "-" "="])
   (["\t" "'" "<" ">" "p" "y"] . ["f" "g" "c" "r" "l" "/" "=" "\\"])
   (["a" "o" "e" "u" "i"] . ["d" "h" "t" "n" "s" "-" "\r"])
   ([";" "q" "j" "k" "x"] . ["b" "m" "w" "v" "z"])]
  "The keyboard layout for Dvorak keyboards.
The split between left and right hands corresponds to that of
the \"Comfort Keyboard\", a brand of split keyboard.")

(defvar versor-tlc-keyboard-layout keyboard-layout-qwerty
  "The keyboard layout for versor-tlc to use.
Each element of the array is a row of the keyboard, top row first.
Each of these elements is a pair of vectors of strings.
Each vector in each element contains the keys for one hand, the
car of the element being the left hand, and the cdr being the right.
Each string in each element contains one key.")

(defconst row-top 0)
(defconst row-upper 1)
(defconst row-home 2)
(defconst row-lower 3)

(defconst finger-index-stretch 0)
(defconst finger-index 1)
(defconst finger-middle 2)
(defconst finger-ring 3)
(defconst finger-pinky 4)
(defconst finger-pinky-stretch 5)
(defconst finger-pinky-stretch-stretch 6)
(defconst finger-pinky-stretch-stretch-ouch 7)

(defun tlc-key (row hand finger)
  "Return the key for ROW HAND FINGER.
ROW can be row-top, row-upper, row-home or row-lower (actually the
numbers 0 to 3).
HAND can be 'left or 'right.
FINGER can be finger-index-stretch, finger-index, finger-middle,
finger-ring, finger-pinky, finger-pinky-stretch,
finger-pinky-stretch-stretch, finger-pinky-stretch-stretch-ouch, which
are actually 1-4 for the home positions of respective fingers (1 for
the index finger), or 0 for the index finger moved towards the centre
of the keyboard, or >4 for the ring or little fingers reaching towards
the edge of the keyboard."
  (let* ((row-pair (aref versor-tlc-keyboard-layout row))
	 (hand-array (if (eq hand 'left)
			 (car row-pair)
		       (cdr row-pair))))
    (aref hand-array
	  (if (eq hand 'left)
	      (- (length hand-array)
		 finger
		 1)
	    finger))))

(defun define-tlc-key (keymap row hand finger command)
  "In KEYMAP, define the key in ROW under HAND FINGER to do COMMAND."
  (define-key keymap (tlc-key row hand finger) command))

;;;;;;;;;;;;;;;;;;;;
;;;; Dimensions ;;;;
;;;;;;;;;;;;;;;;;;;;

(defvar versor-tlc-dimensions-map (make-sparse-keymap)
  "Dimensions keymap for TLC.")

(fset 'versor-tlc-dimensions-map versor-tlc-dimensions-map)

;; The commands for jumping straight to a particular dimension were
;; originally made as voice commands, so load the voice commands for
;; versor:
(require 'versor-voice)
(define-key versor-tlc-dimensions-map "x" 'versor-select-exprs)
(define-key versor-tlc-dimensions-map "d" 'versor-select-defuns)
(define-key versor-tlc-dimensions-map "w" 'versor-select-words)
(define-key versor-tlc-dimensions-map "p" 'versor-select-paragraphs)
(define-key versor-tlc-dimensions-map "s" 'versor-select-sentences)
(define-key versor-tlc-dimensions-map "c" 'versor-select-chars)
(define-key versor-tlc-dimensions-map ";" 'versor-select-statement-parts)
(define-key versor-tlc-dimensions-map "'" 'versor-select-statements)
(define-key versor-tlc-dimensions-map "t" 'versor-select-cells)
(define-key versor-tlc-dimensions-map "r" 'versor-select-rows)

;;;;;;;;;;;;;;;;
;;;; Search ;;;;
;;;;;;;;;;;;;;;;

(defvar versor-tlc-search-map (make-sparse-keymap)
  "Search keymap for TLC.")
(fset 'versor-tlc-search-map versor-tlc-search-map)

(define-tlc-key versor-tlc-search-map row-home 'left finger-ring 'isearch-forward)
(define-tlc-key versor-tlc-search-map row-home 'left finger-pinky 'isearch-backward)
(define-tlc-key versor-tlc-search-map row-home 'right finger-index 'isearch-backward-regexp)
(define-tlc-key versor-tlc-search-map row-home 'right finger-middle 'isearch-forward-regexp)
(define-tlc-key versor-tlc-search-map row-home 'left finger-index-stretch 'query-replace)

(define-tlc-key versor-tlc-search-map row-home 'right finger-index-stretch 'query-replace-regexp)
(define-tlc-key versor-tlc-search-map row-home 'left finger-index 'find-function)
(define-tlc-key versor-tlc-search-map row-lower 'left finger-index 'find-variable)
(define-tlc-key versor-tlc-search-map row-home 'right finger-ring 'find-library)
(define-tlc-key versor-tlc-search-map row-upper 'left finger-index-stretch 'find-tag)

(define-tlc-key versor-tlc-search-map row-lower 'right finger-index-stretch 'goto-line)
(define-tlc-key versor-tlc-search-map row-lower 'left finger-middle 'goto-char)

(define-tlc-key versor-tlc-search-map row-lower 'left finger-index-stretch 'switch-to-buffer)
(define-tlc-key versor-tlc-search-map row-upper 'right finger-ring 'switch-to-buffer-other-window)

;;;;;;;;;;;;;;;;;;;;;;;
;;;; Delete / Copy ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defvar versor-tlc-delete/copy-map (make-sparse-keymap)
  "Delete/Copy keymap for TLC.")
(fset 'versor-tlc-delete/copy-map versor-tlc-delete/copy-map)
(define-tlc-key versor-tlc-delete/copy-map row-lower 'left finger-middle 'versor-copy)
(define-tlc-key versor-tlc-delete/copy-map row-home 'right finger-middle 'versor-kill)
(define-tlc-key versor-tlc-delete/copy-map row-home 'left finger-middle 'kill-region)
(define-tlc-key versor-tlc-delete/copy-map row-upper 'left finger-index 'kill-ring-save)
(define-tlc-key versor-tlc-delete/copy-map row-upper 'left finger-ring 'backward-kill-word)
(define-tlc-key versor-tlc-delete/copy-map row-lower 'left finger-ring 'kill-sexp)
(define-tlc-key versor-tlc-delete/copy-map row-lower 'right finger-ring 'kill-sentence)
(define-tlc-key versor-tlc-delete/copy-map row-upper 'right finger-index 'versor-select-surrounding)

;;;;;;;;;;;;;;;;
;;;; Insert ;;;;
;;;;;;;;;;;;;;;;

(defun versor-tlc-insert-string (string)
  "Insert STRING.
This is a way of getting round not being able to enter characters,
without having to toggles modes as such."
  (interactive "sInsert string: ")
  (insert string))

(defun versor-tlc-insert-eval (expr)
  "Insert something in the current buffer. If this is called
interactively, the expression is read using the minibuffer, and
evaluated to give the text to insert."
  (interactive "XInsert result of evaluating: ")
  (insert
   (cond
    ((stringp expr)                     ; treat this case specially, to
     expr)                              ; avoid \"quotes\" which are probably
    ('else                              ; unwanted ...
     (prin1-to-string expr)))))

(defun versor-tlc-insert-key-command (key-sequence)
  "Insert at point the emacs function name bound to KEY-SEQUENCE."
  (interactive "*kInsert emacs function for key sequence: ")
  (let ((function (key-binding key-sequence)))
    (insert (symbol-name function) " ")
    (describe-function function)))

(defvar versor-tlc-insert-map (make-sparse-keymap)
  "Insert keymap for TLC.")
(fset 'versor-tlc-insert-map versor-tlc-insert-map)

(define-tlc-key versor-tlc-insert-map row-upper 'right finger-index-stretch 'yank) ; "y" on qwerty
(define-tlc-key versor-tlc-insert-map row-upper 'left finger-ring 'downcase-word)
(define-tlc-key versor-tlc-insert-map row-upper 'left finger-middle 'capitalize-word)
(define-tlc-key versor-tlc-insert-map row-upper 'left finger-index 'upcase-word)
(define-tlc-key versor-tlc-insert-map row-upper 'left finger-pinky 'quoted-insert)
(define-tlc-key versor-tlc-insert-map row-upper 'right finger-middle 'smart-insert-parentheses)
(define-tlc-key versor-tlc-insert-map row-upper 'right finger-ring 'smart-insert-quotes)
(define-tlc-key versor-tlc-insert-map row-home 'left finger-middle 'dabbrev-expand)
(define-tlc-key versor-tlc-insert-map row-lower 'right finger-index 'yank-menu)

;; mimic the tlc movement keys
(define-tlc-key versor-tlc-insert-map row-home 'left finger-index 'versor-insert-before)
(define-tlc-key versor-tlc-insert-map row-home 'left finger-index-stretch 'versor-insert-around)
(define-tlc-key versor-tlc-insert-map row-home 'right finger-index 'versor-insert-after)
(define-tlc-key versor-tlc-insert-map row-home 'right finger-index-stretch 'versor-replace)

(define-tlc-key versor-tlc-insert-map row-home 'left finger-ring 'versor-tlc-insert-string)
(define-tlc-key versor-tlc-insert-map row-home 'right finger-middle 'versor-tlc-insert-key-command)
(define-tlc-key versor-tlc-insert-map row-home 'right finger-ring 'dabbrev-expand)

(define-tlc-key versor-tlc-insert-map row-lower 'left finger-index 'versor-tlc-insert-eval)

;;;;;;;;;;;;;;;;;;;
;;;; Transpose ;;;;
;;;;;;;;;;;;;;;;;;;

(defvar versor-tlc-transpose-map (make-sparse-keymap)
  "Transpose keymap for TLC.")
(fset 'versor-tlc-transpose-map versor-tlc-transpose-map)

(define-key versor-tlc-transpose-map "x" 'transpose-sexps)
(define-key versor-tlc-transpose-map "w" 'transpose-words)
(define-key versor-tlc-transpose-map "c" 'transpose-chars)
(define-key versor-tlc-transpose-map "l" 'transpose-lines)
(define-key versor-tlc-transpose-map "s" 'transpose-sentences)
(define-key versor-tlc-transpose-map "p" 'transpose-paragraphs)
(define-key versor-tlc-transpose-map "v" 'versor-transpose)

;;;;;;;;;;;;;;
;;;; Help ;;;;
;;;;;;;;;;;;;;

(require 'versor-keymap)
(require 'languide-keymap)

(defun versor-tlc-help ()
  "Provide help for TLC top-level."
  (interactive)
  ;; this is for QWERTY -- how should I make it handle dvorak? should use the same keys, but will be less mnemonic
  (message (format "versor-tlc-mode is %S
a -- append (normal emacs)  q --               z -- ctl-4
s -- search                 w -- ctl-x         x -- M-x
d -- dimensions             e --               v -- vc
f -- backward               r --               b -- bookmarks
g -- over-backward          t -- transpose     n --
h -- over-forward           y -- yank          m --
j -- forward                u -- undo          , --
k -- delete / copy          i -- insert        . --
l -- languide               o --               ? -- tlc help
; -- versor                 p -- please help
' -- repeat" versor-tlc-mode)))

(defvar tlc-key-descr-length (- (/ (frame-width) 12) 3)
  "How long to make key descriptions.
If you change this, you should re-calculate tlc-key-descr-format.")

(defvar tlc-key-descr-format (format "%%s:%%-%ds" tlc-key-descr-length)
  "String for formatting key descriptions.")

(defun tlc-describe-key (key)
  "Describe KEY, according to ambient value of the variable tlc-current-keymap."
  (let* ((key-string (key-description key))
	 (value (lookup-key tlc-current-keymap key))
	 (cell-string (format tlc-key-descr-format
			      key-string
			      (if value
				  (or (get value 'vsd) ; very short description
				      (symbol-name value))
				"--"))))
    (if (> (length cell-string) tlc-key-descr-length)
	;; could try something like (replace-regexp-in-string "[aeiou]r?" "" cell-string)
	(substring cell-string 0 tlc-key-descr-length)
      cell-string)))

(defvar versor-tlc-help-padding
  [""
   "    "
   "            "
   "              "]
  "Padding for rows of the help diagram.")

(defun versor-tlc-help-padding (row)
  "Return the padding string for ROW."
  (let* ((i 0)
	 (row-number (catch 'found
		       (mapc
			(function
			 (lambda (try-row)
			   (if (eq row try-row)
			       (throw 'found i)
			     (setq i (1+ i)))))
			versor-tlc-keyboard-layout)
		       0)))
    (aref versor-tlc-help-padding row-number)))

(defun versor-tlc-keymap-diagram (keymap &optional label force-redraw width)
  "Return a diagram for the keymap named by KEYMAP, as a multi-line string.
With optional LABEL, label it LABEL.
With optional FORCE-REDRAW, don't use cached result.
With optional WIDTH, use that instead of the frame width."
  (or (and (not force-redraw)
	   (get keymap 'diagram))
      (progn
	(setq tlc-key-descr-length (- (/ (or width
					     (frame-width)) 12) 3)
	      tlc-key-descr-format (format "%%s:%%-%ds" tlc-key-descr-length))
	(let* ((tlc-current-keymap (symbol-value keymap))
	       (diagram
		(concat
		 (if label (concat label ":\n") "")
		 (mapconcat (function
			     (lambda (key-row)
			       (concat (versor-tlc-help-padding key-row)
				       "|"
				       (mapconcat 'tlc-describe-key
						  (car key-row) "|")
				       "||"
				       (mapconcat 'tlc-describe-key
						  (cdr key-row) "|")
				       "|")))
			    versor-tlc-keyboard-layout
			    "\n"))))
	  (put keymap 'diagram diagram)
	  diagram))))

(defvar versor-tlc-keymaps
  '(("main" . versor-tlc-map)
    ("dimensions" . versor-tlc-dimensions-map)
    ("search" . versor-tlc-search-map)
    ("delete/copy" . versor-tlc-delete/copy-map)
    ("insert" . versor-tlc-insert-map)
    ("transpose" . versor-tlc-transpose-map))
  "List of symbols bound to versor-tlc keymaps.")

(defun versor-tlc-keymap-diagram-buffer (keymap &optional label force-redraw width)
  "Return a buffer containing a diagram for KEYMAP.
With optional LABEL, label it LABEL.
With optional FORCE-REDRAW, don't use cached result.
With optional WIDTH, use that instead of the frame width."
  (or (and (not force-redraw)
	   (get keymap 'diagram-buffer))
      (let ((buffer (or (get-buffer-create label)
			(generate-new-buffer "*keyboard map*"))))
	;; (message "buffer name is %S" (buffer-name buffer))
	(set-buffer buffer)
	(erase-buffer)
	(if (eq keymap t)
	    (dolist (map versor-tlc-keymaps)
	      (insert "\n"
		      (versor-tlc-keymap-diagram
		       (cdr map)
		      (car map) force-redraw width)))
	  (insert (versor-tlc-keymap-diagram keymap label force-redraw width)))
	(goto-char (point-min))
	(put keymap 'diagram-buffer buffer)
	buffer)))

(defvar versor-tlc-diagram-frame-parameters
  '((name . "Versor TLC help")
    (width . 132)
    (height . 7)
    (minibuffer . nil)))

(defvar versor-tlc-diagram-frame nil
  "Frame for displaying the diagram.")

(defun versor-tlc-display-diagram (keymap &optional label force-redraw width)
  "Display a buffer containing a diagram for KEYMAP.
With optional LABEL, label it LABEL.
With optional FORCE-REDRAW, don't use cached result.
With optional WIDTH, use that instead of the frame width."
  (unless (frame-live-p versor-tlc-diagram-frame)
    (setq versor-tlc-diagram-frame (make-frame
				    versor-tlc-diagram-frame-parameters)))
  (let* ( ;; (pop-up-frames t)
	 ;; (display-buffer-reuse-frames t)
	 (editing-frame (selected-frame)))
    (condition-case evar
	(progn
	  (message "editing-frame %S, this-command %S" editing-frame this-command)
	  ;; (backtrace)
	  (unless (eq (cdr (assq 'visibility
				 (frame-parameters versor-tlc-diagram-frame)))
		      t)
	    (make-frame-visible versor-tlc-diagram-frame))
	  (display-buffer
	   (versor-tlc-keymap-diagram-buffer keymap
					     label
					     force-redraw
					     width)
	   'not-this-window
	   versor-tlc-diagram-frame)
	  (message "frame after displaying buffer: %S" (selected-frame))
	  (unless (or (eq this-command 'handle-switch-frame)
		      (eq editing-frame versor-tlc-diagram-frame))
	    (select-frame-set-input-focus editing-frame))
	  (message "frame after trying to go back: %S" (selected-frame)))
      (error (display-buffer
	      (versor-tlc-keymap-diagram-buffer keymap
						label
						force-redraw
						width)
	      'not-this-window
	      nil)))))

(defun versor-tlc-display-all-diagram ()
  "Display a diagram of all the versor-tlc keymaps."
  (interactive)
  (versor-tlc-display-diagram t "*All TLC keymaps*" t))

(defun versor-tlc-help-off ()
  (interactive)
  "Switch off display of keyboard diagrams for versor-tlc-mode."
  (fset 'versor-tlc-dimensions-map versor-tlc-dimensions-map)
  (fset 'versor-tlc-search-map versor-tlc-search-map)
  (fset 'versor-tlc-delete/copy-map versor-tlc-delete/copy-map)
  (fset 'versor-tlc-insert-map versor-tlc-insert-map)
  (fset 'versor-tlc-transpose-map versor-tlc-transpose-map))

(defun versor-tlc-help-on ()
  (interactive)
  "Switch on display of keyboard diagrams for versor-tlc-mode."
  (fset 'versor-tlc-dimensions-map 'versor-tlc-do-dimensions-map)
  (fset 'versor-tlc-search-map 'versor-tlc-do-search-map)
  (fset 'versor-tlc-delete/copy-map 'versor-tlc-do-delete/copy-map)
  (fset 'versor-tlc-insert-map 'versor-tlc-do-insert-map)
  (fset 'versor-tlc-transpose-map 'versor-tlc-do-transpose-map))

(defun versor-tlc-do-map (map-name label)
  (versor-tlc-display-diagram map-name label)
  (let* ((event (read-event))
	 (command (lookup-key (symbol-value map-name) (vector event))))
    (call-interactively command)))

(defun versor-tlc-do-dimensions-map ()
  (interactive)
  (versor-tlc-do-map 'versor-tlc-dimensions-map "dimensions"))

(defun versor-tlc-do-search-map ()
  (interactive)
  (versor-tlc-do-map 'versor-tlc-search-map "search"))

(defun versor-tlc-do-delete/copy-map ()
  (interactive)
  (versor-tlc-do-map 'versor-tlc-delete/copy-map "delete/copy"))

(defun versor-tlc-do-insert-map ()
  (interactive)
  (versor-tlc-do-map 'versor-tlc-insert-map "insert"))

(defun versor-tlc-do-transpose-map ()
  (interactive)
  (versor-tlc-do-map 'versor-tlc-transpose-map "transpose"))

(defun versor-tlc-display-main-map (&optional force)
  "Display the main keyboard map for versor-tlc."
  (interactive "P")
  (when (or force versor-tlc-mode)
    (versor-tlc-display-diagram
     'versor-tlc-map
     "Main map"
     t
     (cdr (assoc 'width
		 versor-tlc-diagram-frame-parameters)))))

;;;;;;;;;;;;;;;;;;;
;;;; Top-level ;;;;
;;;;;;;;;;;;;;;;;;;

(defvar versor-tlc-map (make-keymap)
  "Top-level keymap for TLC.")

;;;; The movements
(define-key versor-tlc-map " " 'versor-next)
;; (define-key versor-tlc-map "DEL" 'versor-prev)
(define-key versor-tlc-map [ right ] 'versor-next)
(define-key versor-tlc-map [ left ] 'versor-prev)
(define-key versor-tlc-map [ down ] 'versor-over-next)
(define-key versor-tlc-map [ up ] 'versor-over-prev)
(define-key versor-tlc-map "<" 'beginning-of-buffer)
(define-key versor-tlc-map ">" 'end-of-buffer)

;;;; The home keys, for QWERTY -- redefine for other layouts
(define-tlc-key versor-tlc-map row-home 'left finger-pinky 'versor-tlc-mode-off)
(global-set-key "\C-x~" 'versor-tlc-mode)
(define-tlc-key versor-tlc-map row-home 'left finger-ring 'versor-tlc-search-map)
(define-tlc-key versor-tlc-map row-home 'left finger-middle 'versor-tlc-dimensions-map)
(define-tlc-key versor-tlc-map row-home 'left finger-index 'versor-prev)

(define-tlc-key versor-tlc-map row-home 'right finger-index 'versor-next)
(define-key versor-tlc-map (tlc-key row-home 'right 2) 'versor-tlc-delete/copy-map)
(define-tlc-key versor-tlc-map row-home 'right finger-ring 'languide-map)
(define-tlc-key versor-tlc-map row-home 'right finger-pinky 'versor-general-keymap)

;;;; The rest of the home row
(define-tlc-key versor-tlc-map row-home 'left finger-index-stretch 'versor-over-prev)
(define-tlc-key versor-tlc-map row-home 'right finger-index-stretch 'versor-over-next)
(define-tlc-key versor-tlc-map row-home 'right finger-pinky-stretch 'repeat-complex-command)

;;;; The top row

(define-tlc-key versor-tlc-map row-upper 'left finger-ring 'ctl-x-map)
(define-tlc-key versor-tlc-map row-upper 'left finger-middle 'versor-dwim)
(define-tlc-key versor-tlc-map row-upper 'left finger-index 'versor-reverse)
(define-tlc-key versor-tlc-map row-upper 'left finger-index-stretch 'versor-tlc-transpose-map)
(define-tlc-key versor-tlc-map row-upper 'right finger-index-stretch 'yank)
(define-tlc-key versor-tlc-map row-upper 'right finger-index 'undo)
(define-tlc-key versor-tlc-map row-upper 'right finger-middle 'versor-tlc-insert-map)
(define-tlc-key versor-tlc-map row-upper 'right finger-ring 'versor-other-end-of-item)
(define-tlc-key versor-tlc-map row-upper 'right finger-pinky 'help-map)

;;;; The bottom row
(define-tlc-key versor-tlc-map row-lower 'left finger-pinky 'ctl-x-4-map)
(define-tlc-key versor-tlc-map row-lower 'left finger-ring 'execute-extended-command)
(define-tlc-key versor-tlc-map row-lower 'left finger-index 'vc-prefix-map)
(define-tlc-key versor-tlc-map row-lower 'left finger-index-stretch 'bookmark-map)
(define-tlc-key versor-tlc-map row-lower 'right finger-pinky 'dabbrev-expand)
(define-key versor-tlc-map "?" 'versor-tlc-help)

;;;; The numbers
(define-key versor-tlc-map "1" 'digit-argument)
(define-key versor-tlc-map "2" 'digit-argument)
(define-key versor-tlc-map "3" 'digit-argument)
(define-key versor-tlc-map "4" 'digit-argument)
(define-key versor-tlc-map "5" 'digit-argument)
(define-key versor-tlc-map "6" 'digit-argument)
(define-key versor-tlc-map "7" 'digit-argument)
(define-key versor-tlc-map "8" 'digit-argument)
(define-key versor-tlc-map "9" 'digit-argument)
(define-key versor-tlc-map "0" 'digit-argument)
(define-key versor-tlc-map "-" 'digit-argument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Very Short Descriptions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'backward-kill-word 'vsd "delwd")
(put 'repeat-complex-command 'vsd "chistory")
(put 'beginning-of-buffer 'vsd "[<--")
(put 'bookmark-map 'vsd "bookmark")
(put 'ctl-x-4-map 'vsd "C-x 4")
(put 'ctl-x-map 'vsd "C-x")
(put 'dabbrev-expand 'vsd "dynabbrev")
(put 'digit-argument 'vsd "digit")
(put 'end-of-buffer 'vsd "-->]")
(put 'execute-extended-command 'vsd "M-x")
(put 'find-function 'vsd "function")
(put 'find-library 'vsd "library")
(put 'find-tag 'vsd "tag")
(put 'find-variable 'vsd "variable")
(put 'goto-char 'vsd "char")
(put 'goto-line 'vsd "line")
(put 'help-map 'vsd "help")
(put 'isearch-backward 'vsd "backward")
(put 'isearch-backward-regexp 'vsd "regback")
(put 'isearch-forward 'vsd "forward")
(put 'isearch-forward-regexp 'vsd "regforward")
(put 'kill-region 'vsd "region")
(put 'kill-ring-save 'vsd "copy")
(put 'kill-sentence 'vsd "sentence")
(put 'kill-sexp 'vsd "sexp")
(put 'languide-map 'vsd "syntax")
(put 'query-replace 'vsd "?")
(put 'query-replace-regexp 'vsd ".+?")
(put 'repeat-complex-command 'vsd "repeat")
(put 'smart-insert-parentheses 'vsd "parens")
(put 'smart-insert-quotes 'vsd "quotes")
(put 'switch-to-buffer 'vsd "buffer")
(put 'switch-to-buffer-other-window 'vsd "window")
(put 'transpose-chars 'vsd "chars")
(put 'transpose-lines 'vsd "lines")
(put 'transpose-paragraphs 'vsd "paragraphs")
(put 'transpose-sentences 'vsd "sentences")
(put 'transpose-sexps 'vsd "exprs")
(put 'undo 'vsd "undo")
(put 'vc-prefix-map 'vsd "version")
(put 'versor-copy 'vsd "copy")
(put 'versor-dwim 'vsd "dwim")
(put 'versor-general-keymap 'vsd "versor")
(put 'versor-insert-after 'vsd "after")
(put 'versor-insert-around 'vsd "around")
(put 'versor-insert-before 'vsd "before")
(put 'versor-kill 'vsd "kill")
(put 'versor-next 'vsd "next")
(put 'versor-other-end-of-item 'vsd "other end")
(put 'versor-over-next 'vsd "NEXT")
(put 'versor-over-prev 'vsd "PREV")
(put 'versor-prev 'vsd "prev")
(put 'versor-replace 'vsd "replace")
(put 'versor-reverse 'vsd "reverse")
(put 'versor-select-cells 'vsd "cells")
(put 'versor-select-chars 'vsd "chars")
(put 'versor-select-defuns 'vsd "defuns")
(put 'versor-select-exprs 'vsd "exprs")
(put 'versor-select-paragraphs 'vsd "paragraphs")
(put 'versor-select-rows 'vsd "rows")
(put 'versor-select-sentences 'vsd "sentences")
(put 'versor-select-statement-parts 'vsd "parts")
(put 'versor-select-statements 'vsd "statements")
(put 'versor-select-surrounding 'vsd "surrounding")
(put 'versor-select-words 'vsd "words")
(put 'versor-tlc-delete/copy-map 'vsd "del/copy")
(put 'versor-tlc-dimensions-map 'vsd "dimension")
(put 'versor-tlc-help 'vsd "help")
(put 'versor-tlc-insert-eval 'vsd "eval")
(put 'versor-tlc-insert-key-command 'vsd "cmd")
(put 'versor-tlc-insert-map 'vsd "insert")
(put 'versor-tlc-insert-string 'vsd "string")
(put 'versor-tlc-mode-off 'vsd "no tlc")
(put 'versor-tlc-search-map 'vsd "search")
(put 'versor-tlc-transpose-map 'vsd "transpose")
(put 'versor-tlc-transpose-map 'vsd "transpose")
(put 'yank-menu 'vsd "yankmenu")

;;;;;;;;;;;;;;;;;
;;;; Control ;;;;
;;;;;;;;;;;;;;;;;

(defvar versor-tlc-initialized nil
  "Whether TLC mode has been initialized.")

(defvar versor-tlc-mode nil
  "Whether TLC mode is turned on.")

(defun versor-tlc-not-in-this-buffer ()
  "Suppress versor-tlc-mode in the current buffer.
This should be done for all minibuffers."
  (interactive)
  (make-local-variable 'versor-tlc-mode)
  (setq versor-tlc-mode nil))

(defun versor-tlc-initialize ()
  "Set up the versor TLC system."
  (push (cons 'versor-tlc-mode versor-tlc-map)
	minor-mode-map-alist)
  (setq versor-non-tlc-map (current-global-map)
	versor-tlc-diagram-frame (make-frame versor-tlc-diagram-frame-parameters)
	)
  (mapcar (lambda (hook)
	    (add-hook hook 'versor-tlc-not-in-this-buffer))
	    '(rmail-mode-hook
	      vm-mode-hook
	      vm-summary-mode-hook
	      electric-buffer-menu-mode-hook))
  (versor-tlc-help-on))

(defun versor-tlc-mode-on ()
  "Helper function for versor-tlc-mode."
  (interactive)
  (if (not versor-tlc-initialized)
      (versor-tlc-initialize))
  (setq versor-tlc-mode t)
  (add-hook 'post-command-hook 'versor-tlc-display-main-map)
  (add-hook 'minibuffer-setup-hook 'versor-tlc-not-in-this-buffer)
  (message "versor-tlc-mode on"))

(defun versor-tlc-mode-off ()
  "Helper function for versor-tlc-mode."
  (interactive)
  (setq versor-tlc-mode nil)
  (remove-hook 'post-command-hook 'versor-tlc-display-main-map)
  (remove-hook 'minibuffer-setup-hook 'versor-tlc-not-in-this-buffer)
  (when (frame-live-p versor-tlc-diagram-frame)
    (iconify-frame versor-tlc-diagram-frame))
  (message "versor-tlc-mode off"))

;;;###autoload
(defun versor-tlc-mode (&optional arg)
  "Toggle versor TLC mode.
With no argument, just toggle it. With an argument, switch off if the
argument is negative, otherwise switch on.

TLC (two-letter command, three-letter command, tender loving care)
mode provides vi-like command access for versor, concentrating the
commonest commands onto the home keys, and avoiding chording and
stretching the hands."
  (interactive "P")
  (setq versor-tlc-mode
	(if arg
	    (if (< (prefix-numeric-value arg) 0)
		nil
	      t)
	  (not versor-tlc-mode)))
  (if versor-tlc-mode
      (versor-tlc-mode-on)
    (versor-tlc-mode-off)))

(provide 'versor-tlc)

;;; end of versor-tlc.el
