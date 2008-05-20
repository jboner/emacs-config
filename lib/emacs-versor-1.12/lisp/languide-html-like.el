;;;; languide-html-like.el -- HTML definitions for language-guided editing
;;; Time-stamp: <2006-08-02 12:18:55 john>
;;
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



(defstatement comment (html-helper-mode html-mode)
  "Comment"
  (head "<!-- *")
  (body "<!-- *" (upto " *-->"))
  (tail " *-->")
  (framework (remember "<!--") (remember "-->"))
  (whole (remember "<!--") (remember (upto "-->")) (remember "-->"))
  (create (template "<!-- " r " -->")))

(defstatement paragraph (html-helper-mode html-mode)
  "HyperText paragraph."
  (head "<p" (upto ">"))
  (body "<p[^>]*>" (upto "</p>"))
  (create
   (precondition (not-within "<p[^>]*>" "</p>"))
   (template & "<p>" r "</p> o")))

(defstatement blockquote (html-helper-mode html-mode)
  "HyperText blockquote."
  (head "<blockquote" (upto ">"))
  (body "<blockquote[^>]*>" (upto "</blockquote>>"))
  (create
   ;; todo: are these right? lots of >>>>s!
   (precondition (not-within "<blockquote[^>]*>" "</blockquote>>>"))
   (template & "<blockquote>" r "</blockquote>>>> o")))

(defstatement table-row (html-helper-mode html-mode)
  "HyperText table row."
  (head "<tr" (upto ">"))
  (body "<tr[^>]*>" (upto "</tr>"))
  (create
   (precondition (not-within "<tr[^>]*>" "</tr>"))
   (template & > "<tr>" r "</tr>" %)))

(defstatement table-cell (html-helper-mode html-mode)
  "HyperText table cell."
  (head "<td" (upto ">"))
  (body "<td[^>]*>" (upto "</td>"))
  (create
   (precondition (not-within "<td[^>]*>" "</td>"))
   (template "<td>" r "</td>")))

(defstatement hyper-link (html-helper-mode html-mode)
  "HyperText link."
  (head "<a href=\"" (upto "\">"))
  (body "<a href=\"[^\"]+\"[^>]*>" (upto "</a>"))
  (create
   (precondition (not-within "<a[^>]*>" "</a>"))
   (template "<a href=\"" p "\">" r "</a>")))

(provide 'languide-html-like)

;; end of languide-html-like.el
