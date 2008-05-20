;;;; versor-chop-chop.el -- move quickly using binary chop
;;; Time-stamp: <2006-10-29 23:24:43 jcgs>

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


(require 'versor-containers)

(defvar chop-colours [ "red" "green" "blue" ])
(defvar versor-chops 3)

(defun versor-choose-chop ()
  (let ((choices '(("red" . 0) ("green" . 1) ("blue" . 2))))
    (cdr (assoc
	  (completing-read "Choose chop: " choices nil t)
	  choices))))

(defun versor-chop ()
  "Move the selection, using binary / ternary chop."
  (interactive)
  (versor-select-container-contents)
  (let* ((item (versor-get-current-item))
	 (start (car item))
	 (end (cdr item))
	 (count 0))
    (save-excursion
      (goto-char start)
      (let ((where 0))
	(while (and (/= where (point))
		    (<= (point) end))
	  (setq where (point))
	  (versor-next-action)
	  (message "%d" count)
	  (setq count (1+ count)))))
    (message "%d %s" count versor-current-level-name)
    (goto-char start)
    (let ((per-choice (/ count versor-chops))
	  (chops (make-vector versor-chops nil))
	  (chop-items (make-vector versor-chops 0))
	  (chop 0))
      (while (< chop versor-chops)
	(message "measuring out chop %d" chop)
	(let ((this-start (point))
	      (i 0))
	  (while (and (< (point) end)
		      (<= i per-choice))
	    (message "measuring %s %d of chop %d" versor-current-level-name i chop)
	    (versor-next-action)
	    (setq i (1+ i)))
	  (aset chop-items chop i)
	  (let ((olay (make-overlay this-start (point))))
	    (message "olay %d is %d..%d" chop this-start (point))
	    (overlay-put olay
			 'face (cons 'background-color (aref chop-colours chop)))
	    (aset chops chop olay))
	  )
	(setq chop (1+ chop)))
      (let ((chosen (versor-choose-chop)))
	
	)
      (let ((i 0))
	(while (< i versor-chops)
	  (delete-overlay (aref chops i))
	  (setq i (1+ i)))))

    ))

;; (global-set-key [ f4 ] 'versor-chop)

(provide 'versor-chop-chop)

;;; end of versor-chop-chop.el
