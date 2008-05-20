; /* ====================================================================
;  * Copyright (c) 2004 Carnegie Mellon University.  All rights
;  * reserved.
;  *
;  * Redistribution and use in source and binary forms, with or without
;  * modification, are permitted provided that the following conditions
;  * are met:
;  *
;  * 1. Redistributions of source code must retain the above copyright
;  *    notice, this list of conditions and the following disclaimer. 
;  *
;  * 2. Redistributions in binary form must reproduce the above copyright
;  *    notice, this list of conditions and the following disclaimer in
;  *    the documentation and/or other materials provided with the
;  *    distribution.
;  *
;  * This work was supported in part by AT&T Labs, CMU Sphinx Speech
;  * Consortium and Clairgrove, LLC.
;  *
;  * THIS SOFTWARE IS PROVIDED BY CARNEGIE MELLON UNIVERSITY ``AS IS'' AND 
;  * ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
;  * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;  * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY
;  * NOR ITS EMPLOYEES BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;  * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
;  * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
;  * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
;  * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
;  * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
;  * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;  *
;  * ====================================================================
;  *
;  */
 
(defvar listen-transcript-file (concat listen-path "/transcript.xml")
  "The file where all session transcripts are kept.")

(defun listen-mark-as-isearch-command (commandwords)
 "Mark COMMANDWORDS as a command that should not terminate isearch."
 (let ((entry (listen-lookup-command (downcase commandwords))))
   (put entry 'isearch-remain t)))

(mapcar 'listen-mark-as-isearch-command 
	'("reg sorch" "reg rorch" "inc sorch" "inc rorch" "loon"
	  "spooce" "speece"
	  "con W." "con Y." "meta N." "meta P." "meta toob" "meta y"))

(defconst listen-scopes
  '((char . char) 
    (ting . thing)
    (stretch . stretch)
    (word .  word)    
    (line . line-inclusive) 
    (tier . line) 
    (eed  . identifier)
    (term . term)    
    (block . block)    
    (inner . inner)
    (chunk . chunk)    
    (tisk . this)
    (tat . that)
    (buffer . buffer)
    (parra . para)
    (senten . sentence)
    (defi . def))
  "Note: there is some overlap with listen-common-positions.")
 
(defconst listen-scopes-re
  (concat
   "\\("
   (mapconcat (lambda (pair) (symbol-name (car pair))) listen-scopes
	      "\\|")
   "\\)"))

(defconst listen-default-scope 'identifier)

(defconst listen-common-positions
  '((hare . here)
    (gook . go)
    (tair . there)
    (lairk . mark)
    (loost . last)
    (tisk . this)
    (tat . that)))

(defconst listen-here-there-words 
  (concat (symbol-name (car (rassoc 'here listen-common-positions))) "\\|"
	  (symbol-name (car (rassoc 'there listen-common-positions)))))
(defconst listen-here-word (car (rassoc 'here listen-common-positions)))
(defconst listen-there-word (car (rassoc 'there listen-common-positions)))

(defconst listen-default-position 'here)   

(defconst listen-replace-policy '((on . on)))

(defconst listen-numbers
  '((ane . 1)
    (twain . 2)
    (traio . 3)
    (fairn . 4)
    (faif . 5)))

(defconst listen-numbers-re
  (concat
   "\\("
   (mapconcat (lambda (pair) (concat (symbol-name (car pair)) "\\|"
				     (capitalize (symbol-name (car
							       pair)))))
	      listen-numbers
	      "\\|")
   "\\)"))

(defconst listen-numbers-list
  (apply 'append (mapcar (lambda (pair)
		    (list (symbol-name (car pair))
			  (capitalize (symbol-name (car pair)))))
		  listen-numbers)))

(defconst listen-variation-numbers
  '((oon . 1)
    (twoon . 2)
    (truo . 3)
    (foorn . 4)
    (foof . 5)))

(defconst listen-signed-numbers-re
  (concat
   "\\("
   (mapconcat (lambda (pair) (concat (symbol-name (car pair)) "\\|"
				     (capitalize (symbol-name (car
							       pair)))))
	      (append listen-numbers listen-variation-numbers)
	      "\\|")
   "\\)"))

(defconst listen-signed-numbers-list
  (apply 'append (mapcar (lambda (pair)
		    (list (symbol-name (car pair))
			  (capitalize (symbol-name (car pair)))))
			 (append listen-numbers listen-variation-numbers))))


(defconst listen-directions
  '(
    (skaip . forward)
    (skoop . backward)
    (nairx .  forward)
    (noorx .  backward)))

(defconst listen-extremities
  '((ghin . beginning)
   (ex . end)
   (skaip . end)
   (skoop . end)
   (nairx .  beginning)
   (noorx .  beginning)))

(defconst listen-search-keywords
  '((aift . after-forward)
    (baif . before-forward)
    (ooft . after-backward)
    (boof . before-backward)
    (sorch . forward)
    (rorch . backward)))

(defconst listen-penultimate-keywords
  '((pen . penultimate)))

(defconst listen-paren-registry
  `((tech\ quote . ("``" . "''"))
    (tech\ sing . ("`" . "'"))
    (tech\ brace .  ("\\{" . "\\}"))
    (tech\ angle .  ("\\langle" . "\\rangle"))
    (X\.\ M\.\ quote . ("&apos;" . "&apos;"))
    (X\.\ M\.\ angle . ("&lt;" . "&gt;")) 
    (par . ("(" . ")"))
    (bracket . ("[" . "]"))
    (brace . ("{" . "}"))
    (angle . ("<" . ">"))
    (quote . ("\"" . "\""))
    (sing .  ("'" . "'"))
    (bing .  ("`" . "`")))
  "Available parenthetic contructs X for 'X pair' and 'X nix'. Longer
kinds that contain other kinds as substrings must be placed earlier in
list.")

(defconst listen-symbol-registry
  '((clam . "!")
    (lat . "@")   
    (dall . "$") ; this sound is problematic with NaturallySpeaking 
    (dollar . "$")
    (numb . "#")
    (per . "%")
    (crat . "^")
    (amp . "&")
    (star . "*")
    (laip . "(")
    (rye . ")")
    (plus . "+")
    (noos . "-")
    (eke . "=")
    (bar . "|")
    (lack . "[")
    (rack . "]")
    (lace . "{")
    (race . "}")
    (slash . "/")
    (beck . "\\")
    (till . "~") 
    (sem . ";")
    (col . ":")
    (colon . ":")
    (cam . ",")
    (comma . ",")
    (doot . ".")  
    (lang . "<") ;; Left ANGle
    (rang . ">")
    (quest . "?")
    (score . "_")
    (hive . "-")
    (sing . "'")
    (quote . "\"")
    (bing . "`"))) 

(defconst listen-letter-registry
  '((alpha . "a")(bravo . "b")(charlie . "c")(delta . "d")
    (echo . "e")(foxtrot . "f")(golf . "g")(hotel . "h")(india . "i")
    (juliet . "j")(kilo . "k")(lima . "l")(mike . "m")(november . "n")
    (oscar . "o")(papa . "p")(quebec . "q") (québec .  "q") (romeo . "r")(sierra . "s")
    (tango . "t")(uniform . "u")(victor . "v")(whiskey . "w")
    (x-ray . "x")(yankee . "y")(zulu . "z")))


; (defconst listen-digit-registry
;   '((zero . "0") (one . "1") (two . "2") (three . "3") (four . "4")
;     (five . "5") (six . "6") (seven . "7") (eight . "8") (nine . "9")))

(defconst listen-digit-registry
  '((0 . "0") (1 . "1") (2 . "2") (3 . "3") (4 . "4")
    (5 . "5") (6 . "6") (7 . "7") (8 . "8") (9 . "9")))

(defconst listen-phonetic-registry
  (append listen-symbol-registry listen-letter-registry
	  listen-digit-registry))

;;; COMMAND WORD FOR OTHER WINDOW

(defconst listen-window-management 
  '((vox . present) 
    (vaix . next) 
    (voox . previous) 
    (mini . delete)
    (sploot . split) 
    (maxi . full)))

;;; VARIOUS SMALL WORDS

(defconst listen-space-word "spooce")
(defconst listen-one-space-word "space")
(defconst listen-fix-word "fix")
(defconst listen-nospace-word "speece")
(defconst listen-camel-word "coomel")
(defconst listen-capitalize-word "capi")
(defconst listen-uppercase-word "oopper")
(defconst listen-newline-word "loon")
(defconst listen-backspace-word "choock")
(defconst listen-tab-word "toob")
(defconst listen-intra-line-skip-word "lstar")
(defconst listen-intra-para-skip-word "pstar")
(defconst listen-non-whitespace-skip-word "tingskip")
(defconst listen-letter-skip-word "wordskip")
(defconst listen-go-again-word "goink")
(defconst listen-yank-word "yank")
(defconst listen-start-recording-word "vox rec")
(defconst listen-stop-recording-re  "vox done\\|goink\\|vox play")

(defconst listen-contraction-words-re "\\(capi\\|oopper\\|speece\\|coomel\\)")

(defconst listen-spoken-written-custom
  '(("par pair" "()")
	    ("angle pair" "<>")
	    ("bracket pair" "[]")
	    ("dash" "--")
	    ("short dash" "--")
	    ("long dash" "---")
	    ("P. star" "pstar")
	    ("L. star" "lstar")
	    ("ting skaip" "tingskip")
	    ("word skaip" "wordskip")
	    ("dot" ".")))

(defconst listen-spoken-written-continuation  
  (make-vector (1- (expt 2 10)) 0)
  "Hash table of symbols that mark contination of spoken to written
translation. It summarizes listen-symbol-registry and
listen-phonetic-registry.")
(let ((registry listen-phonetic-registry))
 (while registry
   (let ((object (intern (downcase (format "%s" (caar registry)))
			 listen-spoken-written-continuation)))
     (set object (cdar registry)))
   (setq registry (cdr registry))))

(defconst listen-spoken-written (make-vector (1- (expt 2 12)) 0)
  "Make a hash table for all translations from spoken to written
forms.  In particular, include information in
listen-spoken-written-custom, the listen-phonetic-registry, and the
listen-number-argument.")


;;; PHONETIC STATUS

(defun listen-phonetic-status-initialize ()
  ;; any "toob" command such as "toob loon" is treated as phonetic,
  ;; so that say following "November" becomes "n"
  (listen-set-command-property listen-tab-word 'phonetic t)
  (listen-set-command-property listen-tab-word 'no-fix t))

(add-hook 'listen-initialize-hook 
	  'listen-phonetic-status-initialize)

;;; FIXING OR NO FIXING 

(defun listen-nofix-initialize ()
  (listen-set-command-property 
   (concat "\\(" 
	   listen-one-space-word "\\|"
	   listen-fix-word "\\)"
	   " hare")
   'nofix t))

(add-hook 'listen-initialize-hook
	  'listen-nofix-initialize)

(defconst listen-prevent-fixing-in-minibuffer 
  '(find-file write-file TeX-insert-macro insert-file dired-create-directory))

(if (not (fboundp 'call-interactively-original))
    (fset 'call-interactively-original 
	  (symbol-function 'call-interactively)))

(defun call-interactively (command &optional record-flag keys)
  (if (and (symbolp command)
	   (member command listen-prevent-fixing-in-minibuffer))
      (let ((listen-no-special-fix-in-minibuffer 'no-space))
	(call-interactively-original command record-flag keys))
    (call-interactively-original command record-flag keys)))

;;; ABBREVIATIONS, TEXT NORMALIZATION

(defconst listen-mr-mrs-etc  "\\(Mr[.]\\|Mrs[.]\\|Ms[.]?\\|Drs?[.]\\|Hon[.]\\)")

;;; REPETITION

;;; DELETE THIS STUFF HERE, not needed anymore
(defconst listen-non-repeatable-nullary
  "loon\\|spooce\\|speece\\|coomel\\|stroep"
  "Regular expression matching nullary commands that are not
repeatable.  They also don't count as commands for the purposes of
determining the last position (loost).")


;;; PARSING PREDICATES

(defconst listen-make-do-re 
  ;; "^\\( *\\)[^($%-.,\\/!\t ]*[^ ]?"
  "^\\( *\\)[^][(-;:.,\\/!@#$%^&*(){}'\"`~<>\t ]*[^ ]?")

(defconst listen-predicate-embedded-command-prefix nil)
;  (append '("loon" "spooce" "speece" "comma" )
; 	  (mapcar (lambda (e) (format "%s" (car e)))
;		  listen-phonetic-registry)))

(defconst listen-predicate-embedded-command-extension nil)
;  (append listen-numbers-list '("word"))
;  "The only command extensions currently in existence is the history
;reference mechanism for predicates that are not of type listen-predicate-one-argument-command.")

(defconst listen-predicate-allows-embedded-command-prefix nil)
;  '("choose" "save" "rem" "smack" "caip" "aipper" "laiw"
;    "go" "pen" "space" "speece" "choock" "chaiw" "loon" "spooce" "hive"
;    "fix" "join" "pound")
;  "The set of first words of a predicate command that allows a subset
;of commands in its argument list.")

(defconst listen-predicate-one-argument-command
  '("dent snex" "snex" "snoox" "x. change tag" "m. tech" "e. tech"
    "e. change tech" "go line" "l. ral")
  "The set of full commands, normalized to lower case, that take exactly one argument.")

(defconst listen-embedded-commands
  (concat
   " ?\\<\\(\\("
   (concat
    "\\("
    listen-scopes-re
    " \\)?\\("
    listen-signed-numbers-re
   " \\)?\\("
   (mapconcat (function (lambda (pair) (symbol-name (car pair))))
	      listen-common-positions "\\|")
   "\\)\\)\\|")
   (concat
    "\\("
    listen-yank-word
    "\\) \\("
    listen-numbers-re
    "\\)")
   "\\)\\>[ ]?"))

(defun listen-initialize-search-commands ()
  (let ((search-command-re 
	 (mapconcat (lambda (x) (symbol-name (car x))) 
		    listen-search-keywords
		    "\\|")))
    (listen-set-command-property
     search-command-re
     'embedded-commands
     listen-embedded-commands)
    (listen-set-command-property
     search-command-re
     'command-terminated
     listen-embedded-commands)
    (listen-set-command-property
     search-command-re
     'newline-nonterminator
     listen-embedded-commands)))

(add-hook 'listen-initialize-hook 
	  'listen-initialize-search-commands)

(defconst listen-search-structural-search
  "This is obsolete.  listen-search-for-tokens still contains
references to this code.  Maybe it could become useful someday."
  (concat "^"
	  listen-scopes-re
	  "[ ]?\\("
	  listen-numbers-re
	  "\\)$"))

(defconst listen-search-history-reference 
  listen-numbers-list
  "Strings for which search is a history reference (and nothing else)")

;; RECORDING

(put (listen-lookup-command listen-start-recording-word)
     'start-recording t)
(add-hook 'listen-initialize-hook
	  (function (lambda ()
		      (listen-set-command-property listen-stop-recording-re
						   'stop-recording t))))

;;; POST PREDICATE COMMANDS

;; introduce the listen-here-word, listen-there-word, and other
;; common position designations as post predicate commands
(mapcar (function 
	 (lambda (pair) 
	   (let ((entry (listen-lookup-command (symbol-name (car pair)))))
	     (put entry 'type 'listen-post-predicate)
	     (put entry 'effect 'listen-common-position-post-command)
	     (put entry 'phonetic t))))
	listen-common-positions)

(defun listen-common-position-post-command ()
  (interactive)
  (let ((position 
	 (listen-argument listen-common-positions 
			  listen-event-command-words t))
	(word-events (cdr
		      (assq 'events (listen-event-alist 
				     last-command-event)))))
    (assert position)
    (if (null word-events)
	(message "EmacsListen: Nothing to insert here"))
    (listen-do-at-position (apply 'vector word-events))
    (setq this-command 'listen-heard-post-predicate)))

;;; PEAK WINDOWS

(defconst listen-peek-excluded-buffers
  "[*]Buffer List[*]\\|[*]Messages[*]\\|[*]Minibuf[-].*[*]\\|SPEEDBAR.*")

;;; DELETE SELECTION MODE
(defvar listen-command-kill-selection 
  '(listen-yank paste-command))

(put 'listen-yank-command 'delete-selection 'yank)
(put 'paste-command 'delete-selection 'kill)
; (put 'listen-macro-tex 'delete-selection nil)
; (put 'listen-environment-tex 'delete-selection nil)

(put 'listen-go-again-by-voice 'no-go-again t)
(put 'listen-repeat-last-command-or-recording 'no-go-again t)
;(put 'listen-enter-key 'no-go-again t)
;(put 'listen-spacebar 'no-go-again t)
;(put 'listen-no-space 'no-go-again t)
;(put 'listen-no-space-capitalize 'no-go-again t)
(put 'listen-no-op 'no-go-again t)

(defvar listen-save-time "4:00am")
(defvar listen-save-transcript-no-timer nil
  "True if timer to save transcript is not enabled.")  

(defvar listen-tex-letter-template
  "\\documentclass[11pt]{letter}
\\usepackage[latin1]{inputenc}
\\begin{document} 
\\address{Dumpy\\\\
The Woods}
\\signature{D.}
\\begin{letter}{@}
\\opening{Dear :}

\\closing{Sincerely yours,}
\\end{letter}
\\end{document}
")

(provide 'listen-parameters)





