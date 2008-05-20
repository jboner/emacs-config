;;;; versor-tracking.el -- tracking ordinary cursor, or using mouse, for versor
;;; Time-stamp: <2007-09-25 18:30:57 jcgs>

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

(defun versor-select-around-point (where)
  "Move the selection to surround WHERE (point, interactively)."
  (interactive "d")
  (save-excursion
    ;; save-excursion is to avoid a snap-to-grid-like effect
    (versor-as-motion-command old-item
      (goto-char where)
      (let* ((item (versor-invent-item))
	     (start (car item))
	     (end (cdr item)))
	(versor-set-current-item start end)
	(if (< (point) start)
	    (goto-char start)
	  (if (> (point) end)
	      (goto-char end)))))))

(defun versor-is-selected (position)
  "Return whether POSITION is within the current versor selection."
  (catch 'found
    (let ((pieces (versor-get-current-items)))
      (while pieces
	(when (and (>= position (versor-overlay-start (car pieces)))
		   (<= position (versor-overlay-end (car pieces))))
	  (throw 'found (car pieces)))
	(setq pieces (cdr pieces)))
      nil)))

(defun versor-tracking-hook ()
  "A post-command-hook function for versor to track normal movements."
  (save-excursion
    (condition-case evar
	(unless nil ; (versor-is-selected (point))
	  (versor-select-around-point (point)))
      (error nil))))

(defun versor-mouse-action (event)
  (interactive "e")
  (mouse-minibuffer-check event)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (let ((posn (event-start event)))
    (if (not (windowp (posn-window posn)))
	(error "Cursor not in text area of window"))
    (select-window (posn-window posn))
    (let ((where (posn-point posn)))
      (when (numberp where)
	(versor-select-around-point where)))))

(defun versor-mouse-setup ()
  "Make Versor use the mouse."
  (interactive)
  (global-set-key [mouse-1] 'versor-mouse-action))

;; used from versor-joystick, but could be useful elsewhere
(defun versor-tracking-backward-char (arg)
  "Like `backward-char' with versor tracking, even if not otherwise enabled.
Argument ARG is passed straight through."
  (interactive "p")
  (backward-char arg)
  (versor-select-around-point (point)))

(defun versor-tracking-forward-char (arg)
  "Like `forward-char' with versor tracking, even if not otherwise enabled.
Argument ARG is passed straight through."
  (interactive "p")
  (forward-char arg)
  (versor-select-around-point (point)))

(defun versor-tracking-previous-line (arg)
  "Like `previous-line' with versor tracking, even if not otherwise enabled.
Argument ARG is passed straight through."
  (interactive "p")
  (previous-line arg)
  (versor-select-around-point (point)))

(defun versor-tracking-next-line (arg)
  "Like `next-line' with versor tracking, even if otherwise enabled.
Argument ARG is passed straight through."
  (interactive "p")
  (next-line arg)
  (versor-select-around-point (point)))

(defun versor-tracking-forward-char-other-window (n)
  "Move forward N chars in the buffer of the other window."
  (interactive "p")
  (in-other-window 1 (versor-tracking-forward-char n)))

(defun versor-tracking-forward-char-other-other-window (n)
  "Move forward N chars in the buffer of the other other window."
  (interactive "p")
  (in-other-window 2 (versor-tracking-forward-char n)))

(defun versor-tracking-backward-char-other-window (n)
  "Move backward N chars in the buffer of the other window."
  (interactive "p")
  (in-other-window 1 (versor-tracking-backward-char n)))

(defun versor-tracking-backward-char-other-other-window (n)
  "Move backward N chars in the buffer of the other other window."
  (interactive "p")
  (in-other-window 2 (versor-tracking-backward-char n)))

(defun versor-tracking-next-line-other-window (n)
  "Move forward N lines in the buffer of the other window."
  (interactive "p")
  (in-other-window 1 (versor-tracking-next-line n)))

(defun versor-tracking-next-line-other-other-window (n)
  "Move forward N lines in the buffer of the other other window."
  (interactive "p")
  (in-other-window 2 (versor-tracking-next-line n)))

(defun versor-tracking-previous-line-other-window (n)
  "Move backward N lines in the buffer of the other window."
  (interactive "p")
  (in-other-window 1 (versor-tracking-previous-line n)))

(defun versor-tracking-previous-line-other-other-window (n)
  "Move backward N lines in the buffer of the other other window."
  (interactive "p")
  (in-other-window 2 (versor-tracking-previous-line n)))

(provide 'versor-tracking)

;;; end of versor-tracking.el
