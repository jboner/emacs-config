;;;; handsfree-menus.el
;;; Time-stamp: <2007-08-23 11:12:45 jcgs>
;;
;; Copyright (C) 2007, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

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

(require 'tmm)				; this is based on tmm, at least for now
(require 'handsfree-menu-additions)

(setq;; tmm-mid-prompt nil		; save time
   tmm-completion-prompt nil)		; and space

(defvar type-break-handsfree-menu-count 0
  "How many menus we have popped up since the last type break")

(defvar handsfree-menu-count 0
  "How many menus we have popped up")

(defvar handsfree-main-menu-hook nil
  "*functions to run when asked to do the main menu.
If any of these returns non-nil, it is taken to have done the menu.")

(defun handsfree-main-menu ()
  "Pop up the main menu."
  (interactive)
  (unless (run-hook-with-args-until-success 'handsfree-main-menu-hook)
    (when (interactive-p)
      (incf type-break-handsfree-menu-count)
      (incf handsfree-menu-count))
    (tmm-menubar)))

(defun handsfree-type-break-stats-hook ()
  "Report on how many handsfree menu actions I've done"
  (princ (format "
Handsfree menus since last break: %d
Total handsfree menus           : %d

" type-break-handsfree-menu-count
handsfree-menu-count
)))

(add-hook 'type-break-statistics-hook 'handsfree-type-break-stats-hook)

(provide 'handsfree-menus)

;;; end of handsfree-menus.el
