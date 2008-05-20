;;; remember --- a mode for quickly jotting down things to remember

;; Copyright (C) 1999, 2000, 2001 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Created: 29 Mar 1999
;; Version: 1.5
;; Keywords: data memory todo pim
;; X-URL: http://www.gci-net.com/users/j/johnw/emacs.html

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

;; The idea
;;
;; Todo lists, schedules, phone databases... everything we use
;; databases for is really just a way to extend the power of our
;; memory.  To be able to remember what our conscious mind may not
;; currently have access to.
;;
;; There are many different databases out there -- and good ones --
;; which this mode is not trying to replace.  Rather, it's how that
;; data gets there that's the question.  Most of the time, we just
;; want to say "Remember so-and-so's phone number, or that I have to
;; buy dinner for the cats tonight."  That's the FACT.  How it's
;; stored is really the computer's problem.  But at this point in
;; time, it's most definitely also the user's problem, and sometimes
;; so laboriously so that people just let data slip, rather than
;; expend the effort to record it.
;;
;; "Remember" is a mode for remembering data.  It uses whatever
;; back-end is appropriate to record and correlate the data, but it's
;; main intention is to allow you to express as _little_ structure as
;; possible up front.  If you later want to express more powerful
;; relationships between your data, or state assumptions that were at
;; first too implicit to be recognized, you can "study" the data later
;; and rearrange it.  But the initial "just remember this" impulse
;; should be as close to simply throwing the data at Emacs as
;; possible.
;;
;; Implementation
;;
;; Hyperbole, as a data presentation tool, always struck me as being
;; very powerful, but it seemed to require a lot of "front-end" work
;; before that data was really available.  The problem with BBDB, or
;; keeping up a Bibl-mode file, is that you have to use different
;; functions to record the data, and it always takes time to stop what
;; you're doing, format the data in the manner expected by that
;; particular data interface, and then resume your work.
;;
;; With "remember", you just hit `M-x remember' (you'd probably want
;; to bind this to an easily accessible keystroke, like C-x M-r), slam
;; in your text however you like, and then hit C-c C-c.  It will file
;; the data away for later retrieval, and possibly indexing.
;;
;; Indexing is to data what "studying" is in the real world.  What you
;; do when you study (or lucubrate, for some of us) is to realize
;; certain relationships implicit in the data, so that you can make
;; use of those relationships.  Expressing that a certain quote you
;; remembered was a religious quote, and that you want the ability to
;; pull up all quotes of a religious nature, is what studying does.
;; This is a more labor intensive task than the original remembering
;; of the data, and it's typical in real life to set aside a special
;; period of time for doing this work.
;;
;; "Remember" works in the same way.  When you enter data, either by
;; typing it into a buffer, or using the contents of the selected
;; region, it will store that data -- unindexed, uninterpreted -- in a
;; data pool.  It will also try to remember as much context
;; information as possible (any text properties that were set, where
;; you copied it from, when, how, etc).  Later, you can walk through
;; your accumulated set of data (both organized, and unorganized) and
;; easily begin moving things around, and making annotations that will
;; express the full meaning of that data, as far as you know it.
;;
;; Obviously this latter stage is more user-interface intensive, and
;; it would be nice if "remember" could do it as elegantly as
;; possible, rather than requiring a billion keystrokes to reorganize
;; your hierarchy.  Well, as the future arrives, hopefully experience
;; and user feedback will help to make this as intuitive a tool as
;; possible.
;;
;; Future Goals
;;
;; This tool hopes to track (and by doing it with as little new code
;; as possible):
;;
;;  - The raw data that gets entered
;;
;;  - The relationships between that data (either determined
;;    implicitly by parsing the input, or explicitly by the user's
;;    studying the data).
;;
;;  - Revisioning of the data
;;
;;  - Where it came from, and any context information that can be
;;    programmatically determined.
;;
;;  - Allowing particular views of the initially amorphous data pool
;;    (ala the Xanadu concept).
;;
;;  - Storage of the data in a manner most appopriate to that data,
;;    such as keeping address-book type information in BBDB, etc.
;;
;; Using "remember"
;;
;; As a rough beginning, what I do is to keep my .notes file in
;; outline-mode format, with a final entry called "* Raw data".  Then,
;; at intervals, I can move the data that gets appended there into
;; other places.  But certainly this should evolve into an intuitive
;; mechanism for shuffling data off to its appropriate corner of the
;; universe.
;;
;; Mapping the remember functions to very accessible keystrokes
;; facilities using the mode:
;;
;;  (autoload 'remember "remember" nil t)
;;  (autoload 'remember-region "remember" nil t)
;;
;;  (define-key global-map [f8] 'remember)
;;  (define-key global-map [f9] 'remember-region)
;;
;; Feedback
;;
;; Please send me your comments and ideas.  If Emacs could become a
;; more intelligent data store, where brainstorming would focus on the
;; IDEAS involved -- rather than the structuring and format of those
;; ideas, or having to stop your current flow of work in order to
;; record them -- it would map much more closely to how the mind
;; (well, at least mine) works, and hence would eliminate that very
;; manual-ness which computers from the very beginning have been
;; championed as being able to reduce.
;;
;; Have you ever noticed that having a laptop to write on doesn't
;; _actually_ increase the amount of quality material that you turn
;; out, in the long run?  Perhaps its because the time we save
;; electronically in one way, we're losing electronically in another;
;; the tool should never dominate one's focus.  As the mystic
;; Faridu'd-Din `Attar wrote: "Be occupied as little as possible with
;; things of the outer world but much with things of the inner world;
;; then right action will overcome inaction."

;;; History:

;;; Code:

(provide 'remember)

(defconst remember-version "1.5"
  "This version of remember.")

(defgroup remember nil
  "A mode to remember information."
  :group 'data)

;;; User Variables:

(defcustom remember-mode-hook nil
  "*Functions run upon entering remember-mode."
  :type 'hook
  :group 'remember)

(defcustom remember-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'remember)

(defcustom remember-filter-functions nil
  "*Functions run to filter remember data.
All functions are run in the remember buffer."
  :type 'hook
  :group 'remember)

(defcustom remember-handler-functions '(remember-append-to-file)
  "*Functions run to process remember data.
Each function is called with the current buffer narrowed to what the
user wants remembered.
If any function returns non-nil, the data is assumed to have been
recorded somewhere by that function. "
  :type 'hook
  :group 'remember)

;;; Internal Variables:

(defvar remember-buffer "*Remember*"
  "The name of the remember data entry buffer.")

;;; User Functions:

;;;###autoload
(defun remember ()
  "Remember an arbitrary piece of data."
  (interactive)
  (window-configuration-to-register remember-register)
  (let ((buf (get-buffer-create remember-buffer)))
    (switch-to-buffer-other-window buf)
    (remember-mode)
    (message "Use C-c C-c to remember the data.")))

;;;###autoload
(defun remember-url ()
  "Remember a URL in `bibl-mode' that is being visited with w3."
  (interactive)
  (require 'bibl-mode)
  (save-excursion
    (let ((url (url-view-url t))
	  (name (buffer-name))
	  (keywords (read-string "Keywords: "))
	  (desc (read-string "Description: ")))
      (bibl-visit-create)
      (insert name)
      (forward-line 1) (end-of-line)
      (insert keywords)
      (forward-line 1) (end-of-line)
      (insert desc)
      (forward-line 1) (end-of-line)
      (insert url)
      (bibl-tidy-record)
      (save-buffer)
      (bury-buffer))))

;;;###autoload
(defun remember-location ()
  "Remember a bookmark location in `bibl-mode'."
  (interactive)
  (require 'bookmark)
  (require 'bibl-mode)
  (save-excursion
    (let ((name (read-string "Title: "))
	  (keywords (read-string "Keywords: "))
	  (desc (read-string "Description: ")))
      (bookmark-set name)
      (bookmark-set-annotation name desc)
      (bibl-visit-create)
      (insert name)
      (forward-line 1) (end-of-line)
      (insert keywords)
      (forward-line 1) (end-of-line)
      (insert desc)
      (forward-line 1) (end-of-line)
      (insert (bookmark-get-filename name))
      (bibl-tidy-record)
      (bookmark-delete name)
      (save-buffer)
      (bury-buffer))))

(defsubst remember-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defsubst remember-mail-date (&optional rfc822-p)
  "Return a simple date.  Nothing fancy."
  (if rfc822-p
      (format-time-string "%a, %e %b %Y %T %z" (current-time))
    (format-time-string "%c" (current-time))))

(defun remember-buffer-desc ()
  "Using the first line of the current buffer, create a short description."
  (buffer-substring (point-min)
		    (save-excursion
		      (goto-char (point-min))
		      (end-of-line)
		      (if (> (- (point) (point-min)) 60)
			  (goto-char (+ (point-min) 60)))
		      (point))))

;; Remembering to UNIX mailboxes

(defcustom remember-mailbox "~/Mail/remember"
  "*The file in which to store remember data as mail."
  :type 'file
  :group 'remember)

(defcustom remember-use-bbdb nil
  "*If non-nil, use BBDB for completing names and addresses."
  :set (lambda (symbol value)
	 (when value
	   (load "bbdb")
	   (load "bbdb-com"))
	 (set symbol value))
  :type 'boolean
  :group 'remember)

(defcustom remember-default-priority "medium"
  "*The default priority for remembered mail messages."
  :type 'string
  :group 'remember)

(defun remember-store-in-mailbox ()
  "Store remember data as if it were incoming mail.
In which case `remember-mailbox' should be the name of the mailbox.
Each piece of psuedo-mail created will have an `X-Todo-Priority'
field, for the purpose of appropriate splitting."
  (let ((who (if remember-use-bbdb
		 (bbdb-completing-read-record
		  "Who is this item related to? ")
	       (read-string "Who is this item related to? ")))
	(moment
	 (format "%.0f" (remember-time-to-seconds (current-time))))
	(desc (remember-buffer-desc))
	(text (buffer-string))
	name address)
    (with-temp-buffer
      (if (vectorp who)
	  (let ((net (bbdb-record-net who)))
	    (setq name (bbdb-record-name who)
		  address (if (listp net)
			      (car net)
			    net)))
	(if (stringp who)
	    (setq name who)
	  (setq name (user-full-name)
		address user-mail-address)))
      (insert (format "
From %s  %s
Date: %s
From: %s
Message-Id: <remember-%s@%s>
X-Todo-Priority: %s
To: %s <%s>
Subject: %s\n\n"
		      (user-login-name)
		      (remember-mail-date)
		      (remember-mail-date t)
		      (if (and name address)
			  (format "%s <%s>" name address)
			(or name address))
		      moment (system-name)
		      remember-default-priority
		      (user-full-name) user-mail-address
		      desc))
      (let ((here (point)))
	(insert text)
	(unless (bolp)
	  (insert "\n"))
	(insert "\n")
	(goto-char here)
	(while (re-search-forward "^\\(From[: ]\\)" nil t)
	  (replace-match ">\\1")))
      (append-to-file (point-min) (point-max) remember-mailbox)
      t)))

(custom-add-option 'remember-handler-functions 'remember-store-in-mailbox)

;; Remembering to plain files

(defcustom remember-data-file "~/.notes"
  "*The file in which to store unprocessed data."
  :type 'file
  :group 'remember)

(defcustom remember-leader-text "** "
  "*The text used to begin each remember item."
  :type 'string
  :group 'remember)

(defun remember-append-to-file ()
  "Remember, with description DESC, the given TEXT."
  (let ((text (buffer-string))
	(desc (remember-buffer-desc)))
    (with-temp-buffer
      (insert "\n" remember-leader-text (current-time-string)
	      " (" desc ")\n\n" text)
      (if (not (bolp))
	  (insert "\n"))
      (if (find-buffer-visiting remember-data-file)
	  (let ((remember-text (buffer-string)))
	    (set-buffer (get-file-buffer remember-data-file))
	    (save-excursion
	      (goto-char (point-max))
	      (insert remember-text)
	      (save-buffer)))
	(append-to-file (point-min) (point-max) remember-data-file)))))

(custom-add-option 'remember-handler-functions 'remember-append-to-file)

;; Remembering to a planner.el task page

(defun remember-append-to-planner ()
  "Remember, with description DESC, the given TEXT."
  (require 'planner)
  (goto-char (point-min))
  (forward-line)
  (let ((text (buffer-string)))
    (save-window-excursion
      (planner-create-note)
      (insert text)
      (save-buffer))))

(custom-add-option 'remember-handler-functions 'remember-append-to-planner)

;;;###autoload
(defun remember-region (&optional beg end)
  "Remember the data from BEG to END.
If called from within the remember buffer, BEG and END are ignored,
and the entire buffer will be remembered.  If called from any other
buffer, that region, plus any context information specific to that
region, will be remembered."
  (interactive)
  (let ((b (or beg (min (point) (or (mark) (point-min)))))
	(e (or end (max (point) (or (mark) (point-max))))))
    (save-restriction
      (narrow-to-region b e)
      (run-hook-with-args-until-success 'remember-handler-functions)
    (when (equal remember-buffer (buffer-name))
      (kill-buffer (current-buffer))
      (jump-to-register remember-register)))))

;;;###autoload
(defun remember-clipboard ()
  "Remember the contents of the current clipboard.
Most useful for remembering things from Netscape or other X Windows
application."
  (interactive)
  (with-temp-buffer
    (insert (x-get-clipboard))
    (run-hook-with-args-until-success 'remember-handler-functions)))

;;;###autoload
(defun remember-buffer ()
  "Remember the contents of the current buffer."
  (interactive)
  (remember-region (point-min) (point-max)))

;;; Internal Functions:

(defvar remember-mode-map ())
(if remember-mode-map
    ()
  (setq remember-mode-map (make-sparse-keymap))
  (define-key remember-mode-map "\C-x\C-s" 'remember-buffer)
  (define-key remember-mode-map "\C-c\C-c" 'remember-buffer))

(defun remember-mode ()
  "Major mode for output from \\[remember].
\\<remember-mode-map>This buffer is used to collect data that you want
remember.  Just hit \\[remember-region] when you're done entering, and
it will go ahead and file the data for latter retrieval, and possible
indexing.  \\{remember-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (indented-text-mode)
  (use-local-map remember-mode-map)
  (setq major-mode 'remember-mode
	mode-name "Remember")
  (run-hooks 'remember-mode-hook))

;;; remember.el ends here

