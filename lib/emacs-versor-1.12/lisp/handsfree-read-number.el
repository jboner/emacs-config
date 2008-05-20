;;;; handsfree-read-number.el
;;; Time-stamp: <2007-08-22 19:28:53 jcgs>
;;
;; Copyright (C) 2007, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2004
;; Copyright (C) 2004, 2006  John C. G. Sturdy
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

(defvar read-number-history nil
  "History variable for reading numbers")

(defun handsfree-read-number (prompt min max initial)
  "Read a number, using PROMPT, between MIN and MAX, starting at INITIAL."
  (let ((numbers nil)
	(i min))
    (while (<= i max)
      (push (int-to-string i) numbers)
      (incf i))
    (setq read-number-history numbers)
    (string-to-int
     (read-from-minibuffer prompt
			   (int-to-string initial)
			   nil
			   nil
			   (cons 'read-number-history
				 ;; (- max (- initial min))
				 (1+ (- max initial))
				 )))))

(defun handsfree-read-percentage (prompt initial)
  "Read a percentage, with PROMPT and INITIAL."
  (handsfree-read-number prompt 0 100 initial))

(provide 'handsfree-read-number)

;;; end of handsfree-read-number.el
