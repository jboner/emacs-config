;;;; versor-containers.el -- Delimit the container of the current selection
;;; Time-stamp: <2006-08-02 12:18:07 john>

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

(defun versor-select-container-contents ()
  "Select the container of the current selection."
  (interactive)
  (versor-as-motion-command current-item
   (unless (run-hook-with-args-until-success 'versor-end-hooks)
     (message "Using %S to get end of container" (versor-get-action 'last))
     (call-interactively (versor-get-action 'last)))
   (let ((end (versor-end-of-item-position)))
     (unless (run-hook-with-args-until-success 'versor-start-hooks)
       (message "Using %S to get start of container" (versor-get-action 'first))
       (call-interactively (versor-get-action 'first)))
     (versor-set-current-item (point) end))))

;; for debugging:
;; (global-set-key [ kp-subtract ] 'versor-select-container-contents)

(provide 'versor-containers)

;;;; end of versor-containers.el
