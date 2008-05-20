;;; hexcolor.el --- Display hex colors
;;
;; Author: Lennart Borgman
;; Created: Sun Apr 30 00:19:18 2006
(defconst hexcolor:version "0.5");; Version: 0.5
;; Last-Updated:
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
;; Functions for displaying hex colors.  The minor mode part of this
;; is mostly from hexcolour.el on EmacsWiki by Oliver Scholz.
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

(defvar hexcolor-keywords
  '((
     ;; Why does not the next line work?
     ;;"#\\(?:[a-fA-F[:digit:]]\\{6\\}\\|[a-fA-F[:digit:]]\\{3\\}\\)[^a-fA-F[:digit:]]"
     "#\\(?:[a-fA-F[:digit:]]\\{6\\}\\|[a-fA-F[:digit:]]\\{3\\}\\)"
     (0 (put-text-property (match-beginning 0)
                           (match-end 0)
                           'face (list :background
                                       (match-string-no-properties 0)))))))

(define-minor-mode hexcolor-mode
  "Show hex color literals with the given color as background.
In this mode hexadecimal colour specifications like #3253ff are
displayed with the specified colour as background."
  :initial-value nil
  ;;(unless font-lock-syntax-table
  (unless font-lock-defaults
    (error "Can't use hexcolor-mode for this major mode"))
  (if hexcolor-mode
      (font-lock-add-keywords nil hexcolor-keywords)
    (font-lock-remove-keywords nil hexcolor-keywords))
  (font-lock-fontify-buffer))

;;(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
;;(remove-hook 'css-mode-hook 'hexcoulour-add-to-font-lock)

(defvar hexcolor-fg-history nil)
(defvar hexcolor-bg-history nil)
(defun hexcolor-test()
  "Test colors interactively.
The colors are displayed in the echo area. You can specify the
colors in any way Emacs knows about.  Example:

  red
  #f00
  #0C0
  #b0ff00
  rgb:ff/00/00"
  (interactive)
  (let* ((fg-color (read-string "Foreground color: " nil hexcolor-fg-history))
         (bg-color (read-string "Background color: " nil hexcolor-bg-history))
         (s (concat " Foreground: " fg-color ", Background: " bg-color " ")))
    (put-text-property 0 (length s)
                       'face (list
                              :foreground fg-color
                              :background bg-color)
                       s)
    (message "Here are the colors you choosed: %s" s)))


(provide 'hexcolor)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hexcolor.el ends here
