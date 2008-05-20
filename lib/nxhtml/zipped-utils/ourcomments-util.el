;;; ourcomments-util.el --- Utility routines
;;
;; Author: Lennart Borgman <lennart dot borgman at gmail dot com>
;; Created: Wed Feb 21 23:19:07 2007
(defconst ourcomments-util:version "0.21") ;;Version:
;; Last-Updated: Sat Apr 21 01:08:46 2007 (7200 +0200)
;; Keywords:
;; Compatibility: Emacs 22
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; The functionality given by these small routines should in my
;; opinion be part of Emacs (but they are not that currently).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popups etc.

(defun point-to-coord(point)
  "Return coordinates of point in selected window.
The coordinates are in the form \(\(XOFFSET YOFFSET) WINDOW)."
  (let* ((pn (posn-at-point point))
	 (x-y (posn-x-y pn))
	 (x (car x-y))
	 (y (cdr x-y))
	 (pos (list (list x (+ y 20)) (selected-window))))
    pos))

(defun popup-menu-at-point (menu &optional prefix)
  "Popup the given menu at point.
This is similar to `popup-menu' and MENU and PREFIX has the same
meaning as there.  The position for the popup is however where
the window point is."
  (let ((where (point-to-coord (point))))
    (popup-menu menu where prefix)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggles in menus

(defmacro define-toggle (symbol value doc &rest args)
  "Declare SYMBOL as a customizable variable with a toggle function.
The purpose of this macro is to define a defcustom and a toggle
function suitable for use in a menu.

The arguments have the same meaning as for `defcustom' with these
restrictions:

- The :type keyword cannot be used.  Type is always 'boolean.
- VALUE must be t or nil.

A `defcustom' named SYMBOL and a function named SYMBOL-toggle is
defined.  The function toggles the value of SYMBOL.  It takes no
parameters.

To create a menu item something similar to this can be used:

    \(define-key map [SYMBOL]
      \(list 'menu-item \"Toggle nice SYMBOL\"
            'SYMBOL-toggle
            :button '(:toggle . SYMBOL)))"
  (declare (doc-string 3))
  (list
   'progn
   (let ((var-decl (list 'custom-declare-variable
                         (list 'quote symbol)
                         (list 'quote value)
                         doc)))
     (while args
       (let ((arg (car args)))
         (setq args (cdr args))
         (unless (symbolp arg)
           (error "Junk in args %S" args))
         (let ((keyword arg)
               (value (car args)))
           (unless args
             (error "Keyword %s is missing an argument" keyword))
           (setq args (cdr args))
           (cond ((not (memq keyword '(:type)))
                  (setq var-decl (append var-decl (list keyword value))))
                 (t
                  (lwarn '(define-toggle) :error "Keyword %s can't be used here" keyword))))))
     (when (assoc :type var-decl) (error ":type is set. Should not happen!"))
     (setq var-decl (append var-decl (list :type '(quote boolean))))
     var-decl)
   (let* ((SYMBOL-toggle (intern (concat (symbol-name symbol) "-toggle")))
          (SYMBOL-name (symbol-name symbol))
          (fun-doc (concat "Toggles the \(boolean) value of `"
                           SYMBOL-name
                           "'.\n"
                           "For how to set it permanently see this variable.\n"
                           ;;"\nDescription of `" SYMBOL-name "':\n" doc
                           )))
     `(defun ,SYMBOL-toggle()
        ,fun-doc
        (interactive)
        (customize-set-variable (quote ,symbol) (not ,symbol)))
     )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation of regions

;; From an idea by weber <hugows@gmail.com>
(defun indent-line-or-region ()
  "Indent line or region.
Only do this if indentation seems bound to \\t.

Call `indent-region' if region is active, otherwise
`indent-according-to-mode'."
  (interactive)
  ;; Do a wild guess if we should indent or not ...
  (let* ((indent-region-mode)
         (t-bound (key-binding [?\t])))
    (if (not
         (save-match-data
           (string-match "indent" (symbol-name t-bound))))
        (call-interactively t-bound t)
      (if mark-active ;; there is a region selected
          (indent-region (region-beginning) (region-end))
        (indent-according-to-mode))))) ;; indent line

(define-minor-mode indent-region-mode
  "Use \\t to indent line or region.
The key \\t is bound to `indent-line-or-region' if this mode is
on."
  :global t
  :keymap '(([?\t] . indent-line-or-region)))
(when indent-region-mode (indent-region-mode 1))



(provide 'ourcomments-util)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ourcomments-util.el ends here
