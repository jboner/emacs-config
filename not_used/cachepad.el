;;;
;;; cache pad, written by Jonathan Epstein (Jonathan_Epstein@nih.gov), partially
; based upon demacs, written by Thomas Rene Nielsen (trn@imada.ou.dk)
;
; Copyright (C) 2000  Jonathan Epstein (Jonathan_Epstein@nih.gov)
; 
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
; Also see:
;   http://www.gnu.org/copyleft/gpl.html
;
; Version 1.18
;
;
;
; Description of intended purpose:
; 
; This package is primarily intended for users who program using voice
; recognition software.  It permits the caching of a small number (default:10)
; symbols such as variable names, which may subsequently be reinserted
; into source code using only a single voice utterance such as "recent 3".
; The list of cached symbols is managed in a least-recently-used fashion.
;
;
; 
; Installation instructions:
; 
; Place this file within your load-path, and include the following line
; in your .emacs:
;   (require 'cachepad)
; 
; To automatically create a cache pad window at the bottom of your initial
; emacs frame, include the following as well:
;   (create-cache-window)
; 
; Here are some sample DragonDictate macros suitable for using this package:
;   add-word "[cache buffer]" /keys {Esc}xinsert-selection-into-cache-pad{Enter} /nsc
;   add-word "[cache region]" /keys {Esc}xinsert-region-into-cache-pad{Enter} /nsc
;   add-word "[cache word]" /keys {Esc}xinsert-cache-pad{Enter} /nsc
;   add-word "[recent <Number/1 to 40>]" /script "i=_1_to_40_1
;     SendKeys \"{Ctrl+u}\"+i+\"{Esc}xinsert-nth-cache{Enter}\"" /nsc
;
; Note that this package should work using any voice recognition package
; which supports macros.
;
;
; 
; Usage instructions:
; 
; To cache the symbol which appears at the current Emacs point, say "cache
; word".  To include the most recently used symbol (as shown in the
; cache pad window), say "recent 1".
; 
; To cache the symbol which appears in the Clipboard, say "cache buffer".
; 
; Keyboard equivalents:
;   M-xinsert-cache-pad
;   C-u1M-xinsert-nth-cache
;   M-xinsert-selection-into-cache-pad
; 
; Import and export of sets of cache pad symbols is possible using the
; functions cache-pad-parse-input and cache-pad-print-parsable.
;
; For a demonstration of the cache pad, see the movies at:
;   http://voicerecognition.org/developers/jepstein/JavabyVoice/
; 
; As of early 1999, this package is integrated as part of the
; VoiceGrip continuous speech-based programming system.  See:
;   http://ai.iit.nrc.ca/II_public/VoiceCode/
; 
;
; Known bugs:
; 
; Large symbol names or symbols containing non-printing characters can result
; in a messy or unusable cache pad window.
; 
; There are occasional problems caching a symbol which appears at beginning
; of the buffer or the end of the buffer.
;
; 
; 
; LCD Archive Entry:
;  cachepad|Jonathan Epstein|Jonathan_Epstein@nih.gov
;  Cache of recently used symbols, suitable for use with voice recognition
;  11-Feb-00|Version 1.18|~/misc/cachepad.el.Z
; 
; 
; 
; 
; 
; Change Log
; Version  Changed by   Changes
; 1.10     Epstein      Eliminated dependency upon demacs's general.el by
; 11/20/98                cannibilizing portions of that code
;                       Introduced "provide" statement so that "require"
;                         may be used
;                       Changed module name from cache.el to cachepad.el
;                       Added new modules cache-pad-print-parsable and
;                         cache-pad-parse-input, primarily to interface
;                         with Alain Desilets's <Alain.Desilets@iit.nrc.ca>
;                         VoiceGrip package
;                       Trick emacs into ignoring changes in the cachepad file
;                       Avoid newlines within cached symbols
;                       Use "defvar" instead of "setq"
; 1.11     Epstein      Eliminated dependency upon emacs 20 by using
; 12/09/98              "borrowed" ginel-string-split instead of split-string.
; 1.12     Epstein      Incorporated cache pad printing protection provided by
; 12/10/98              Alain Desilets.  Fixed cache insertion logic error
;                       introduced in previous version.
; 1.13     Epstein      Updated documentation to make suitable for inclusion
; 01/13/99              in Emacs archive.
; 1.14     Epstein      Added the ability to search for a cached string
; 03/22/99              (isearch-nth-cache).
; 1.15     Epstein      Added GNU General Public License
; 08/04/99
; 1.16     Epstein      Added deferred caching, intended for use with
; 09/29/99              continuous speech input.
; 1.17     Epstein      Added macros for jumping to code by line number, as
; 02/11/00                well as caching the symbols at those locations.
;                       Also added support for horizontally positioned cache pad
;                       Also added ability to copy back and forth between cache
;                       and clipboard
; 1.18     Epstein      Removed erroneous parameter from deferred-insert-cache-pad
; 03/01/00

(defvar cache-pad-list nil)
(defvar cache-pad-filename nil)
(defvar cache-pad-default-filename "*CACHEPAD*")
;(setq cache-pad-default-filename "~/default.cpd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of modes
;;
;; Unfortunately, the syntax tables which are built into emacs major modes
;; cannot provide the necessary information to know whether underscores and
;; hyphens constitute legal characters within symbol names.  Therefore, we
;; must construct our own lists of which modes are associated with each symbol
;; style.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar c-modes '(c-mode perl-mode java-mode jde-mode matlab-mode))
(defvar lisp-modes '(lisp-mode emacs-lisp-mode))
;;(defvar tex-modes '(ams-latex-mode latex-mode plain-tex-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window coordinates and size
;;
;; Note that you can use the following settings to obtain a three by
;; three grid instead of the default two by five grid.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defvar cache-num-rows 3)
;;(defvar cache-num-columns 3)
;;(defvar cache-column-width 27)
(defvar cache-num-rows 5)
(defvar cache-height-fudge-factor 1)
(defvar cache-num-columns 2)
(defvar cache-column-width 40)
(defvar cache-horizontal-split nil)
(defvar cache-window nil)
(defvar cache-frame nil)
(defvar cache-place-point-before-rel-search t)

(defvar cache-deferral-timer 2.0)
(defvar cache-defer-list nil)
(defvar cache-defer-update nil)
;(defvar cache-after-changed-armed nil)
(defvar cache-timer nil)
(setq cache-what-env (or
	       (and(eq window-system 'nil)
		   "console")
	       (and (memq window-system '(win32 w32))
		    (or (getenv "OS") "Windows_95"))
	       (and (eq window-system 'x)
		    (concat "X" window-system-version "_windows"))
	       ))

(defun relative-line-to-absolute (relative-line radix)
  (let* ((top-line (count-lines 1 (window-start)))
	 (bottom-line (count-lines 1 (window-end)))
	 (middle-line (/ (+ top-line bottom-line) 2))
	 (x (+ middle-line (/ radix 2)))
	 (modulo (% x radix))
	 (base (- x modulo)))
	 (if (> relative-line modulo) (setq base (- base radix)))
	 (+ base relative-line)))

(defun cache-from-jumped-char-on-rel-line (relative-line char occurrence radix before-or-after)
  "cache the symbol at a certain character position on a relative line number, without moving (point)"
  (interactive "nRelative-line: \nsCharacter: \nnOccurrence: \nnRadix: \nnPlace cursor before (-1), after (1), or according to default (0): ")
  (save-excursion
    (if (jump-to-char-on-rel-line relative-line char occurrence radix before-or-after)
	(insert-cache-pad)))
)

(defun jump-to-char-on-rel-line (relative-line char occurrence radix before-or-after)
  "jump to a certain character position on a relative line number"
  (interactive "nRelative-line: \nsCharacter: \nnOccurrence: \nnRadix: \nnPlace cursor before (-1), after (1), or according to default (0): ")
  (let ((before (< before-or-after 0)))
    (if (< relative-line 0) (setq relative-line (% (+ (count-lines 1 (point)) 2 relative-line) radix)))
    (if (= before-or-after 0) (setq before cache-place-point-before-rel-search))
    (goto-line (relative-line-to-absolute relative-line radix))
    (if (> occurrence 0)
	(progn
	  (end-of-line)
	  (let ((endpoint (point)))
	    (beginning-of-line)
	    (if (search-forward char endpoint nil occurrence)
		(progn
		  (if before (backward-char))
		  t)
	      nil)
	    ))
      (progn
	(beginning-of-line)
	(let ((beginpoint (point))
	      (distance (if (< occurrence 0) (- 0 occurrence) 1)))
	  (end-of-line)
	  (if (search-backward char beginpoint nil distance)
	      (progn
		(if (null before) (forward-char))
		t)
	    nil)
	  )))))


; (defun cache-grab-all-symbols ()
;   "grab all symbols within the current defun"
;   (interactive)
;   (progn
;     (save-excursion
;       (setq first (beginning-of-defun))
;       (setq last (end-of-defun))

(defun cache-timer-routine ()
  "timer routine for updating of the cache order"
  (interactive)
  (progn
    (if cache-timer (cancel-timer cache-timer))
    (setq cache-timer nil)
    (setq cache-defer-update nil)
    (mapcar 'insert-word-into-cache-pad cache-defer-list)
    (setq cache-defer-list nil)
    ))

(defun deferred-insert-nth-cache (num)
  "deferred insertion"
  (interactive "p")
  (progn
    (setq cache-defer-update t)
    (cache-timer-arm)
    (insert-nth-cache num)
    ))

(defun deferred-insert-cache-pad ()
  "deferred insertion into the cache pad"
  (interactive "p")
  (progn
    (setq cache-defer-update t)
    (cache-timer-arm)
    (insert-cache-pad)
    ))

(defun cache-timer-arm ()
  (progn
    (if cache-timer (cancel-timer cache-timer))
    (setq cache-timer (run-with-timer cache-deferral-timer nil 'cache-timer-routine))
    ))

  

(defun region-to-x-selection ()
  "captures the current region into the X selection buffer"
  (interactive)
  (x-set-selection 'PRIMARY (buffer-substring (point) (mark))))

(defun create-cache-window ()
  "Creates the cache window"
  (interactive)
  (delete-other-windows)
  (if cache-horizontal-split
    (progn
      (split-window-horizontally (- 0 cache-column-width))
      (other-window 1))
    (progn  
      (split-window-vertically)
      (other-window 1)
      (shrink-window (- (- (window-height) cache-num-rows) (+ cache-height-fudge-factor 2)))))
  (setq cache-window (selected-window))
  (setq cache-frame (selected-frame))
  (find-file cache-pad-default-filename)
  (other-window 1))

; swiped from "ginel" to avoid depending upon Emacs 20+'s split-string,
; which is functionally identical, for our purposes
(defun ginel-string-split (string regexp)
  "Splits STRING into list of strings and returns the result.
Anything matching REGEXP is taken to be a field delimiter."  (save-match-data
	(let (list)       (while (string-match regexp string)
		(setq list (cons (substring string 0 (match-beginning 0)) list))
		(setq string (substring string (match-end 0))))   (if (length string)
		  (setq list (cons string list)))         (reverse list))))

(defun cache-pad-parse-input (filename &optional reverse-it flush-it)
  "Input consists of one symbol per line.  The symbols are inserted into the cache pad, following the LRU algorithm."
(let ((output-with-newlines nil)
      (word-list nil)
      (eol-match nil))
  (find-file filename)
  (beginning-of-buffer)
  (set-mark-command nil)
  (end-of-buffer)
  (setq output-with-newlines (buffer-substring (point) (mark)))
  (kill-buffer nil)

  (setq word-list
;       (split-string
	(ginel-string-split
	 output-with-newlines "[\n\r]+"))
  (if reverse-it (setq word-list (nreverse word-list)))
  (if flush-it (setq cache-pad-list nil))
    
  (mapcar 'insert-word-into-cache-pad word-list)
  )
)
  
(defun cache-pad-print-parsable (filename)
  " Write the cache pad in a format where it may be easily parsed by other applications.  The output format consists of one symbol per line"
  (interactive)
  (let ((the-list cache-pad-list))
  (find-file filename)
  (end-of-buffer nil)
  (while the-list
    (insert (concat (car the-list) "\n"))
    (setq the-list (cdr the-list)))
  (save-buffer)
  (kill-buffer nil)
  ))

;(defun test-print-cache ()
;  "test"
;  (interactive)
;  (cache-pad-print-parsable "c:/users/epstein/emacs/testc.txt"))
;(defun test-parse-cache ()
;  "test"
;  (interactive)
;  (cache-pad-parse-input "c:/users/epstein/emacs/testc.txt" t))

(defun print-cache-pad ()
  "prints the cache pad into the screen's cache buffer window"
  (interactive)
  (if (and cache-frame cache-window)
      (progn
  (let* ((num 1)
	 (str nil)
	 (row 0)
	 (col 0)
	 (temp-window (selected-window))
	 (temp-other-window nil)
	 (temp-frame (selected-frame))
	 (the-list cache-pad-list))
	(select-frame cache-frame)
	(setq temp-other-window (selected-window))
	(select-window cache-window)
	(erase-buffer)
	(while (and the-list (< row cache-num-rows))
	  (if (> row 0) (open-line 1))
	  (if (> row 0) (forward-line 1))
	  (setq row (+ row 1))
	  (setq col 0)
	  (while (and the-list (< col cache-num-columns))
	     (setq str (number-to-string num))
	     (insert "(")
	     (insert str)
	     (insert ") ")
	     (insert (car-safe the-list))
	     (setq col (+ col 1))
	     (setq num (+ num 1))
;; insert padding as necessary
	     (if (< col cache-num-columns)
	       (let ((n 0))
		 (setq n (- cache-column-width
			  (+ 3 (length str) (length (car-safe the-list)))))
		 (while (> n 0) (insert " ") (setq n (- n 1)))))
	     (setq the-list (cdr-safe the-list))))
;; a hack to make emacs refrain from asking us whether this buffer should
;; be saved when it exits ... there's probably a better way to do this.
	(set-buffer-modified-p nil)
;; find our way back to the original frame and window   
	(select-window temp-other-window)
	(select-frame temp-frame)
	(select-window temp-window)))))

(defun isearch-nth-cache (num)
  "fetch the nth element of the cache into isearch ring"
  (interactive "p")
  (let ((word (nth (- num 1) cache-pad-list)))
       (if word (isearch-update-ring word))
       (insert-word-into-cache-pad word)))

; not really related to caching, but we have the machinery available, so why not
(defun capture-filename ()
  (interactive)
  (if (eq cache-what-env  "X11_windows") (x-set-selection (buffer-file-name))
    (w32-set-clipboard-data (buffer-file-name))))

(defun capture-nth-cache (num)
  "fetch the nth element of the cache into clipboard"
  (interactive "p")
  (let ((word (nth (- num 1) cache-pad-list)))
       (if (eq cache-what-env  "X11_windows") (x-set-selection word) (w32-set-clipboard-data word))
       (insert-word-into-cache-pad word)))

(defun insert-nth-cache (num)
  "fetch the nth element of the cache"
  (interactive "p")
  (let ((word (nth (- num 1) cache-pad-list)))
       (if word (insert word))
       (insert-word-into-cache-pad word)))

(defun insert-word-into-cache-pad (the-word)
  "perform the actual insertion"
  (interactive)
  (let ((word the-word)
	(first-bad (string-match "[\n\r]" the-word)))
    ;; ensure that the string doesn't contain any embedded newlines, etc.
    (if (not (null first-bad)) (setq word (substring the-word 1 first-bad)))
    (if (not (string-equal word ""))
	(if cache-defer-update (setq cache-defer-list (cons word cache-defer-list))
	  (progn
	    (setq cache-pad-list (cons word (delete word cache-pad-list)))
	    ;; prevent the list from growing too long
	    (setq word (nth (+ 1 (* cache-num-rows cache-num-columns))
			    cache-pad-list))
	    (if word (setq cache-pad-list (delete word cache-pad-list))))))
      (print-cache-pad))
  )

(defun insert-selection-into-cache-pad ()
 "Inserts the current selection into cache-pad if 'word' is not there."
 (interactive)
  (insert-word-into-cache-pad (if (eq cache-what-env  "X11_windows") (x-get-selection) (w32-get-clipboard-data))))

(defun insert-region-into-cache-pad ()
  "Inserts the current region into cache-pad if 'word' is not there."
  (interactive)
  (insert-word-into-cache-pad (buffer-substring (point) (mark))))

(defun insert-cache-pad ()
  "Inserts the word at cursor in cache-pad if 'word' is not there."
  (interactive)
  ;(backward-word 1)
  (start-of-word)
  (if (member major-mode lisp-modes)
      (while (looking-at-num -1 "-")
	(backward-word 1)))
  (if (member major-mode c-modes)
      (while (looking-at-num -1 "_")
	(backward-word 1)))  

  (let ((start-pos (point)))
    (forward-word 1)
    (if (member major-mode lisp-modes)
	(while (looking-at "-")
	  (forward-word 1)))
    (if (member major-mode c-modes)
	(while (looking-at "_")
	  (forward-word 1)))
    (let ((end-pos (point)))
      (let ((word (buffer-substring start-pos end-pos)))
	    (insert-word-into-cache-pad word)))))

;; The following functions were blatantly copied from demacs to enable
;; this cache pad module to stand alone.
(defun looking-at-num (num exp)
  "Like 'looking-at', but using num as offset from point."
  (forward-char num)
  (let ((bool (looking-at exp)))
    (backward-char num)
    bool)
  )
(defun start-of-line ()
  "Moves cursor to start of line"
  (interactive)
  (goto-char (get-start-pos))
  )
(defun get-start-pos ()
  (let ((this-pos (point)))
    (beginning-of-line)
    (let ((start-pos (point)))
      (goto-char this-pos)
      start-pos))
  )
(defun start-of-word ()
  "Moves cursor to start of word. Returns true if cursor has been moved."
  (interactive)
    (while (and (>= (- (point) 1) (point-min)) (or (and (<= ?A (char-after (- (point) 1))) (>= ?Z (char-after (- (point) 1)))) (and (<= ?a (char-after (- (point) 1))) (>= ?z (char-after (- (point) 1))))))
      (backward-char 1))
  )

 


(provide 'cachepad)
