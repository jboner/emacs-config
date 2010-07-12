;;; linum.el --- Display line numbers to the left of buffers

;; Copyright (C) 2007  Markus Triska

;; Author: Markus Triska <[EMAIL PROTECTED]>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Display line numbers for the current buffer. Copy linum.el to your
;; load-path and add to your .emacs:

;;    (require 'linum)

;; Then toggle display of line numbers with M-x linum.

;;; Code:

(defconst linum-version "0.8f")

(defvar linum-overlays nil)
(defvar linum-active nil)
(defvar linum-overlay-property (if (fboundp 'extent-property)
                                   'begin-glyph 'before-string))

(defgroup linum nil
  "Show line numbers to the left of buffers"
  :group 'convenience)

;;;###autoload
(defcustom linum-format "%6d  "
  "Format used to display line numbers. Either a format string like \"%6d  \",
or the symbol 'dynamic to adapt the width as needed. 'dynamic or
a format string that does not expand to a multiple of 8 can make
indentations look different if you indent using tab characters."
  :group 'linum
  :type 'sexp)

(when (< emacs-major-version 21)
    (require 'cl))

(when (fboundp 'make-extent)
  (fset 'make-overlay 'make-extent)
  (fset 'overlay-put 'set-extent-property)
  (fset 'overlay-get 'extent-property)
  (fset 'delete-overlay 'delete-extent)
  (fset 'move-overlay 'set-extent-endpoints)
  (fset 'overlays-in (lambda (from to)
                       (if (fboundp 'extent-list) ; suppress warning
                           (extent-list nil from to))))
  (fset 'overlay-start 'extent-start-position))

(mapc #'make-variable-buffer-local '(linum-overlays linum-active))

(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    "Compatibility definition for Emacs < 22, taken from CVS."
    (let ((opoint (or pos (point))) start)
      (save-excursion
        (goto-char (point-min))
        (setq start (point))
        (goto-char opoint)
        (forward-line 0)
        (1+ (count-lines start (point)))))))

;;;###autoload
(defun linum ()
  "Toggle display of line numbers."
  (interactive)
  (setq linum-active (not linum-active))
  (when (or (< emacs-major-version 21) (fboundp 'extent-property))
    (make-local-hook 'post-command-hook))
  (if linum-active
      (progn
        (add-hook 'post-command-hook 'linum-update nil t)
        (message "Linum enabled"))
    (remove-hook 'post-command-hook 'linum-update t)
    (mapc #'delete-overlay linum-overlays)
    (setq linum-overlays nil)
    (message "Linum disabled")))

(defun linum-dynamic-format ()
  "Compute a format string based on the number of lines in the
current buffer."
  (let ((lines (count-lines (point-min) (point-max)))
        (width 0))
    (while (> lines 0)
      (setq lines (/ lines 10))
      (setq width (1+ width)))
    (format "%%%dd  " width)))

(defun linum-update ()
  "Update displayed line numbers for the current buffer."
  (save-excursion
    (goto-char (window-start))
    (let ((line (line-number-at-pos))
          (limit (window-end nil t))
          (fmt (if (stringp linum-format) linum-format (linum-dynamic-format)))
          ov
          free)
      (unless (fboundp 'extent-property)
        (setq limit (1+ limit)))
      (dolist (ov (overlays-in (point) limit))
        (when (overlay-get ov 'linum)
          (push ov free)))
      ;; Create an overlay (or reuse an existing one) for each visible
      ;; line in this window.
      (while (and (not (eobp)) (< (point) limit))
        (if (null free)
            (progn
              (setq ov (make-overlay (point) (point)))
              (overlay-put ov 'linum t)
              (push ov linum-overlays))
          (setq ov (pop free))
          (move-overlay ov (point) (point)))
        (overlay-put ov linum-overlay-property
                     (funcall (if (fboundp 'extent-property)
                                'make-glyph 'identity) (format fmt line)))
        (forward-line)
        (setq line (1+ line)))
      (mapc #'delete-overlay free))))

;;;###autoload
(defun linum-version ()
  "Display version of linum."
  (interactive)
  (message "Using linum version %s" linum-version))

(provide 'linum)
;;; linum.el ends here
