;;;; versor-keypad.el -- define keypad keys for using keypad almost in isolation
;;; Time-stamp: <2007-07-25 18:11:25 jcgs>

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

(defun versor-keypad-setup ()
  "Set up the keypad for being the main editing input."
  (interactive)
  (keypad-separate)
  (global-set-key [ pause ] 'handsfree-main-menu)
  (global-set-key [ C-pause ] 'versor-do-dynamic-menu)
  ;; fill in the rest
)

(provide 'versor-keypad)

;;; end of versor-keypad.el
