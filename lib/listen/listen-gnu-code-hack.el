;;; This file contains code that enhances the use of EmacsListen, the
;;; ShortTalk Lisp source code when interpreted under the GNU EMACS
;;; editor.

;;; This code is not part of EmacsListen.  It does not contribute any
;;; essential functionality.

;;; What it does do: it allows an EmacsListen to work in situations
;;; where choose-completion-string is invoked.


;;; THIS IS THE ORIGINAL HEADER FROM simple.el
;;;
;;; simple.el --- basic editing commands for Emacs

;; Copyright (C) 1985, 86, 87, 93, 94, 95, 96, 97, 98, 1999
;;        Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A grab-bag of basic Emacs commands not specifically related to some
;; major mode or to file-handling.

;;; THIS CODE HAS BEEN MODIFIED BY NILS KLARLUND

(defun choose-completion-string (choice &optional buffer base-size)
  (let ((buffer (or buffer completion-reference-buffer)))
    ;; If BUFFER is a minibuffer, barf unless it's the currently
    ;; active minibuffer.
    (if (and (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
	     (or (not (active-minibuffer-window))
		 (not (equal buffer
			     (window-buffer (active-minibuffer-window))))))
	(error "Minibuffer is not active for completion")
      ;; Insert the completion into the buffer where completion was requested.
      (set-buffer buffer)
      (if base-size
	  (delete-region (+ base-size (point-min)) (point))
	(choose-completion-delete-max-match choice))
      (listen-insert-simple choice)
      (remove-text-properties (- (point) (length choice)) (point)
			      '(mouse-face nil))
      ;; Update point in the window that BUFFER is showing in.
      (let ((window (get-buffer-window buffer t)))
	(set-window-point window (point))
	(if (not (equal buffer (window-buffer (minibuffer-window))))
	    (select-window window)))
      ;; If completing for the minibuffer, exit it with this choice.
      (and (not completion-no-auto-exit)
	   (equal buffer (window-buffer (minibuffer-window)))
	   minibuffer-completion-table
	   ;; If this is reading a file name, and the file name chosen
	   ;; is a directory, don't exit the minibuffer.
	   (if (and (eq minibuffer-completion-table 'read-file-name-internal)
		    (file-directory-p (buffer-string)))
	       (select-window (active-minibuffer-window))
	     (exit-minibuffer))))))

(provide 'listen-gnu-code-hack)
