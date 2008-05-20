;;;; versor-keymap.el -- set up a keymap for versor
;;; Time-stamp: <2007-04-09 14:51:06 jcgs>

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



(defvar versor-general-keymap (make-keymap "Versor"))

(define-key versor-general-keymap "n" 'versor-next)
(define-key versor-general-keymap "p" 'versor-prev)

(define-key versor-general-keymap "N" 'versor-over-next)
(define-key versor-general-keymap "P" 'versor-over-prev)

(define-key versor-general-keymap [ delete ] 'versor-kill)
(define-key versor-general-keymap "d" 'versor-kill)

(define-key versor-general-keymap "b" 'versor-insert-before)
(define-key versor-general-keymap "r" 'versor-insert-around)
(define-key versor-general-keymap "a" 'versor-insert-after)

(define-key versor-general-keymap "c" 'versor-begin-altering-item)

(define-key versor-general-keymap "v" 'versor-languide-convert-selection-to-variable)
(define-key versor-general-keymap "g" 'versor-languide-convert-selection-to-global-variable)
(define-key versor-general-keymap "f" 'versor-languide-convert-selection-to-function)

(define-key versor-general-keymap "(" 'versor-languide-surround-selection-with-call)
(define-key versor-general-keymap "{" 'versor-languide-unify-statements)
(define-key versor-general-keymap "?" 'versor-languide-make-conditional)
(define-key versor-general-keymap ";" 'versor-languide-comment-selection)

(define-key versor-general-keymap "u" 'versor-select-surrounding)

(define-key versor-general-keymap "q" 'versor-end-command-mode)

(defvar versor-command-old-map nil
  "The map before we started using versor-command-mode.")

(defun versor-command-mode-prompter ()
  (sit-for 4)
  (message "Versor command (q to quit): "))

(defun versor-end-command-mode ()
  (interactive)
  (use-local-map versor-command-old-map)
  (remove-hook 'post-command-hook 'versor-command-mode-prompter))

(defun versor-command-mode ()
  "Use a keymap that binds many normal characters to versor commands."
  (interactive)
  (setq versor-command-old-map (current-local-map))
  (use-local-map versor-general-keymap)
  (add-hook 'post-command-hook 'versor-command-mode-prompter)
  (message "q to return to normal editing"))

(provide 'versor-keymap)

;;; end of versor-keymap.el
