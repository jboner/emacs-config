;;;; word-isearch.el -- isearch-like facility working in whole words
;;; Time-stamp: <2006-08-02 12:18:06 john>

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



;;; This is aimed primarily for use by voice, but could also be used
;;; with a menu-like selection system, and so could be suitable as the
;;; base for something for those who can use narrow-channel button
;;; interfaces such as pedals, but are uncomfortable with keyboards.

(defvar in-word-isearch nil
  "Whether we are currently in a word isearch.")

(defun word-isearch-following-words-match (place words)
  "Return whether the words after PLACE are WORDS."
  (if words
      (save-excursion
	(goto-char place)
	(catch 'missed
	  (while (and words (forward-word 1) (backward-word 1))
	    (if (looking-at (format "\\<%s\\>" (car words)))
		(progn
		  (forward-word 1)
		  (setq words (cdr words)))
	      (throw 'missed nil)))
	  (goto-char (match-end 0))
	  (match-end 0)))
    nil))

(defun word-isearch-after-change-function (begin end length)
  "Adjust the word-isearch to suit the latest state of the minibuffer."
  (when in-word-isearch
    (let ((search-string-high-water (nth 2 word-isearch-state)))
      (if (and (zerop length) (> begin search-string-high-water))
	  (let* ((search-string (buffer-substring-no-properties (point-min) (point-max)))
		 (search-words (split-string search-string))
		 (search-buffer (first word-isearch-state))
		 (occurrences (nth 1 word-isearch-state)))
	    ;; insertion
	    (set-buffer search-buffer)
	    (if occurrences
		nil
	      (let* ((first-word (car search-words))
		     (first-word-search-pattern (format "\\<%s\\>" first-word))
		     (following-words (cdr search-words)))
		(goto-char (point-min))
		(while (re-search-forward first-word-search-pattern (point-max) t)
		  (let* ((start (match-beginning 0))
			 (end (match-end 0))
			 (matched-rest (word-isearch-following-words-match end following-words)))
		    (if matched-rest
			(let ((hit (make-overlay start
						 matched-rest)))
			  (overlay-put hit 'face isearch)
			  (setq occurrences (cons hit occurrences))))))
		(aset word-isearch-state 1 occurrences)
		(aset word-isearch-state 2 (setq search-string-high-water (length search-string)))
		(aset word-isearch-state 3 search-string)
		(aset word-isearch-state 4 search-words)
		))
	    )
	;; deletion, or alteration before search-high-water
	)))
  )

(defun word-isearch-minibuffer-setup-hook ()
  "Internals for word-isearch."
  (when in-word-isearch
    (add-hook 'after-change-functions 'word-isearch-after-change-function))
  (setq mmessages nil))

(defun word-isearch-minibuffer-exit-hook ()
  "Internals for word-isearch."
  (remove-hook 'after-change-functions 'word-isearch-after-change-function))

(defvar word-isearch-state nil
  "The current search state.
This is a list of:
0  The buffer being searched
1  The list of occurrences
")

(defun word-isearch ()
  "Interactive search for whole words."
  (interactive)
  (let* ((in-word-isearch t)
	 (word-isearch-state (list (current-buffer) nil))
	 )
    (add-hook 'minibuffer-setup-hook 'word-isearch-minibuffer-setup-hook)
    (add-hook 'minibuffer-exit-hook 'word-isearch-minibuffer-exit-hook)
    (read-from-minibuffer "Search for: ")
    (mapcar 'delete-overlay (nth 1 word-isearch-state))
    (with-output-to-temp-buffer "*MMessages*"
      (dolist (line (nreverse mmessages))
	(princ line)
	(princ "\n")))))


(provide 'word-isearch)

;;; end of word-isearch.el
