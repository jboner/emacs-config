;;;; versor-menu.el -- part of dimensional navigation
;;; Time-stamp: <2007-10-27 22:51:54 jcgs>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2005, 2006, 2007  John C. G. Sturdy
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

;;; If you have even fewer keys than normal available for versor (for
;;; example, a more restricted set of input than that arranged by
;;; versor-pedals) you may need a menu for some of the actions -- for
;;; example, reversing motion if you only have a "move" key rather
;;; than a "next" and "previous" pair

(require 'versor-names)
(require 'versor-language-edits)
(require 'languide-keymap)

(defvar versor-control-menu-items
  `(["Out briefly" versor-out-briefly-only t]
    ["Backwards" versor-reverse [ :included (and versor-reversible (not versor-reversed))]]
    ["Forwards" versor-reverse [ :included (and versor-reversible versor-reversed)]]
    ["Set level" versor-select-named-level t]
    ["Set meta-level" versor-select-named-meta-level t]
    ["Copy" versor-copy t]
    ["Kill" versor-kill t]
    ["Mark" versor-mark t]
    )
  "Control of versor.")

(easy-menu-define versor-control-menu nil
  "Versor Control Menu" (cons "Versor control" versor-control-menu-items))

(defun versor-control-menu ()
  "Run the versor control menu."
  (interactive)
  (tmm-prompt versor-control-menu)
  ;; return t because we can be used in aux-pedal (pedals.el) as a hook
  t)

(defun versor-add-menu-item (menu name command)
  "Add to MENU an item made from NAME and COMMAND."
  (rplacd (cdr menu)
	  (cons
	   (list (intern name) 'menu-item name command)
	   (cdr (cdr menu)))))

(defvar versor-dynamic-menu-add-items-at-bottom-hook nil
  "Functions to run to add menu items at the bottom of the dynamic menu.
The menu keymap is passed as the only argument to each function.")

(defvar versor-dynamic-menu-add-items-at-top-hook nil
  "Functions to run to add menu items at the top of the dynamic menu.
The menu keymap is passed as the only argument to each function.")

(defvar versor-dynamic-menu 
  '(("versor insertions" . versor-insertions-menu)
    ("kill %s" . versor-kill)
    ("alter %s" . versor-begin-altering-item)
    ("language operations" . versor-languide-menu)
    ("versor control" . versor-control-menu)
    ("describe" . dwim-help)
    ("search for next %s" . versor-search-forward)
    ("search for previous %s" . versor-search-backward)
    ("find" . dwim-find-at-point)
    ("select surrounding" . versor-select-surrounding)
    ("mark %s" . versor-mark)
    ("copy %s" . versor-copy))
  "Versor dynamic menu.
This does things involving the current selection.")

(defun versor-generate-dynamic-menu ()
  "Generate dynamic menu for versor.
Allows various actions that depend on the current fine movement dimension."
  (let ((dynamic-menu (make-sparse-keymap "Versor dynamic menu")))
    (run-hook-with-args 'versor-dynamic-menu-add-items-at-bottom-hook
			dynamic-menu)
    (mapcar (function
	     (lambda (name-command)
	       (let* ((raw-name (car name-command))
		      (formatted-name (format raw-name
					      versor-current-level-name)))
		 (versor-add-menu-item dynamic-menu
				       formatted-name
				       (cdr name-command)))))
	    versor-dynamic-menu)
    (run-hook-with-args 'versor-dynamic-menu-add-items-at-top-hook
			dynamic-menu)
    dynamic-menu))

(defun versor-do-dynamic-menu ()
  "Dynamic menu for versor.
Allows various actions that depend on the current fine movement dimension."
  (interactive)
  (let ((versor-level-shadow versor-level)
	(versor-meta-level-shadow versor-meta-level))
    (tmm-prompt (versor-generate-dynamic-menu))))

(defun versor-insertions-menu ()
  "Run the versor insertions menu."
  (interactive)
  (message "versor-insertions-menu, (versor-current-item-valid) = %S" (versor-current-item-valid))
  (let ((versor-insertion-using-menu t))
    (tmm-prompt (versor-generate-insertions-menu))))

(defun versor-generate-insertions-menu ()
  "Generate insertions menu for versor.
Allows various actions that depend on the current fine movement dimension."
  (let ((dynamic-menu (make-sparse-keymap "Versor insertions menu")))
    (mapcar (function
	     (lambda (name-command)
	       (let* ((raw-name (car name-command))
		      (formatted-name (format raw-name versor-current-level-name))
		      )
		 (versor-add-menu-item dynamic-menu formatted-name (cdr name-command)))))
	    '(
	      ("insert before %s" . versor-insert-before)
	      ("insert after %s" . versor-insert-after)
	      ("replace %s" . versor-replace)
	      ("insert around %s" . versor-insert-around)
	      ))
    dynamic-menu))

;;;;experimental!!!!!!!!!!!!!!!!

(defun inform-about (string)
  "Tell the user something about STRING.
This will try the help system, tag files etc."
  (interactive "sInformation about: ")
  (let* ((symbol (intern-soft string))
	 (information
	  (cond
     ((and symbol
	   (documentation symbol)))
     (t
      "no information about %s" string))))
    (message "%s" information)
    information))

(provide 'versor-menu)

;;; end of versor-menu.el
