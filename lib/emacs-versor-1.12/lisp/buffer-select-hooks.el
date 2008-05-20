;;;; buffer-select-hooks.el -- do things when noticing that buffer or mode has changed
;;; Time-stamp: <2007-07-16 18:40:55 john>
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

(defvar buffer-before-command nil
  "The buffer which was current before the last command.")

(defvar mode-before-command nil
  "The mode which was current before the last command.")

(defun buffer-selection-hook-common ()
  "Common code for our before and after hooks.
Doing the checks before the command, as well as after, means that
our hook functions get called if a non-command changes
something (for example, sometimes coming out of the minibuffer
can do this; or , a process filter receiving voice commands that
don't come through the normal command system.)"
  ;; modes are less specific than buffers, so do them first
  (if (not (eq mode-before-command major-mode))
      (condition-case error-var
	  (run-hook-with-args 'mode-selection-hook mode-before-command)
	(error (message "error %S in mode-selection-hook" error-var))))
  (if (not (eq (current-buffer) buffer-before-command))
      (condition-case error-var
	  (run-hook-with-args 'buffer-selection-hook buffer-before-command)
	(error (message "error %S in buffer-selection-hook switching from %S to %S"
			error-var
			buffer-before-command
			(current-buffer)))))
  (setq buffer-before-command (current-buffer)
	mode-before-command major-mode))

(defun buffer-selection-pre-command-hook ()
  "Function to run before each command, for detecting commands changing the current buffer."
  (buffer-selection-hook-common)
  ;; to get round something that is meant to prevent danger -- should
  ;; I be doing this?  seems safe enough in this case -- I haven't had
  ;; any problems with it, and I've been using it all the time for
  ;; months now
  (add-hook 'post-command-hook 'buffer-selection-post-command-hook))

(defun buffer-selection-post-command-hook ()
  "Function to run after each command, for detecting commands changing the current buffer."
  (add-hook 'pre-command-hook 'buffer-selection-pre-command-hook)
  (buffer-selection-hook-common))

(add-hook 'pre-command-hook 'buffer-selection-pre-command-hook)
(add-hook 'post-command-hook 'buffer-selection-post-command-hook)

(defvar buffer-selection-hook
  nil
  "Functions to run on selecting a different buffer.
They are given the previous buffer as an argument, and can find the new
buffer using (current-buffer)")

(defvar mode-selection-hook
  nil
  "Functions to run on being in a different mode than before the last command.
This could be because of the command changing which buffer is active,
or changing the mode of the same buffer.
They are given the previous mode as an argument, and can find the new one
from major-mode.")

(defun mode-selection-in-title-bar (&rest ignore)
  "Indicate the mode in the title bar."
  (set-frame-name (concat "emacs@" (system-name) ":" mode-name)))

(provide 'buffer-select-hooks)

;; (add-hook 'mode-selection-hook 'mode-selection-in-title-bar)


