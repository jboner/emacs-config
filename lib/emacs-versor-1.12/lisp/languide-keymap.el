;;;; languide-keymap.el -- keymap and menu setup for languide
;;; Time-stamp: <2007-12-12 18:58:46 jcgs>

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


(require 'versor-language-edits)

(defvar versor-languide-menu-items
  '(["Use local variable for selection" versor-languide-convert-selection-to-variable t]
    ["Use global variable for selection" versor-languide-convert-selection-to-global-variable t]
    ["Convert selection to function" versor-languide-convert-selection-to-function t]
    ["Surround selection with function call" versor-languide-surround-selection-with-call t]
    ["Remove function call" versor-languide-remove-function-call t]
    ["Create function for call" versor-languide-create-function-for-call t]
    ["Remove control" versor-languide-remove-control]
    ["Unify statements" versor-languide-unify-statements t]
    ["Comment selection" versor-languide-comment-selection t]
    ["Make selection conditional" versor-languide-make-conditional t]
    ["Make selection iterative" versor-languide-make-conditional t]
    ["Move to enclosing scoping point" versor-languide-enclosing-scoping-point t]
    ["Move to enclosing decision point" versor-languide-enclosing-decision-point t]
    ["Employ variable" versor-languide-employ-variable t]
    ["Describe selection" versor-describe-selection t]
    ))

(easy-menu-define versor-languide-menu nil
  "Versor languide menu" (cons "Versor languide" versor-languide-menu-items))

;; todo: put it on the main menu

(defun versor-languide-menu ()
  "Run the versor languide menu."
  (interactive)
  (tmm-prompt versor-languide-menu))

(define-prefix-command 'languide-map nil "Language operations")

(define-key languide-map "=" 'versor-languide-convert-selection-to-variable)
(define-key languide-map "f" 'versor-languide-convert-selection-to-function)
(define-key languide-map "g" 'versor-languide-convert-selection-to-global-variable)
(define-key languide-map "e" 'versor-languide-create-function-for-call)
(define-key languide-map "(" 'versor-languide-surround-selection-with-call)
(define-key languide-map ")" 'versor-languide-remove-function-call)
(define-key languide-map "{" 'versor-languide-unify-statements)
; (define-key languide-map "=" 'versor-languide-enclosing-scoping-point)
(define-key languide-map "?" 'versor-languide-make-conditional)
(define-key languide-map "@" 'versor-languide-make-iterative)
(define-key languide-map "^" 'versor-languide-remove-control)
(define-key languide-map ";" 'versor-languide-comment-selection)
(define-key languide-map "\C-h" 'versor-describe-selection)
(define-key global-map "\M-#" 'languide-map)

;; todo: make languide toolbar -- see example from info:
;; (defvar info-tool-bar-map
;;    (if (display-graphic-p)
;;       (let ((tool-bar-map (make-sparse-keymap)))
;; 	(tool-bar-add-item-from-menu 'Info-exit "close" Info-mode-map)
;; 	(tool-bar-add-item-from-menu 'Info-prev "left_arrow" Info-mode-map)
;; ...


(provide 'languide-keymap)

;;; end of languide-keymap.el
