;;; erlext.el --- Socket communication with Erlang External Term Format

;; Copyleft (C) 2000 Luke Gorrie <luke@bluetail.com>
;; Version: $Id: erlext.el,v 1.2 2001/05/15 00:03:22 luke Exp $
;; Keywords: erlang

;; Borrowing heavily from corba.el by Lennart Staflin <lenst@lysator.liu.se>
;; and JInterface from OTP.

;;; Commentary:
;;
;; This is a library for communicating with Erlang
;; (http://www.erlang.org/) programs using the erlang external term
;; format. Messages are sent via the {packet, 4} protocol.
;;
;; Supported mappings from erlext to elisp:
;;   string  -> string
;;   atom    -> symbol
;;   integer -> integer
;;   tuple   -> vector
;;   list    -> list
;; Not mapped/supported yet:
;;   pid, ref, bin, port, float
;;
;; FIXME: Using recursion to cons up list/tuple elements limits the
;; size of the lists and tuples that we can read in proportion to
;; `max-lisp-eval-depth'. Because of this largeTuple support is
;; untested. Must switch to iteration when this becomes a problem.
;; ----------------------------------------------------------------------

(eval-when-compile (require 'cl))
(eval-when-compile (load "cl-extra"))

;; type tags

(put 'smallInt   'erlext-tag 97)
(put 'int        'erlext-tag 98)
(put 'float      'erlext-tag 99)
(put 'atom       'erlext-tag 100)
(put 'ref        'erlext-tag 101)
(put 'port       'erlext-tag 102)
(put 'pid        'erlext-tag 103)
(put 'smallTuple 'erlext-tag 104)
(put 'largeTuple 'erlext-tag 105)
(put 'nil        'erlext-tag 106)
(put 'string     'erlext-tag 107)
(put 'list       'erlext-tag 108)
(put 'bin        'erlext-tag 109)
(put 'smallBig   'erlext-tag 110)
(put 'largeBig   'erlext-tag 111)
(put 'newRef     'erlext-tag 114)

(defconst erlext-max-atom-length 255 "The maximum length of an erlang atom.")
(defconst erlext-protocol-version 131)

(defun make-unique-ref (&optional name)
  (generate-new-buffer (concat " *Ref:" (if name name "unique") "*")))

(defun erlext-get-work-buffer ()
  (let ((buf (generate-new-buffer " *ERLEXT")))
    (with-current-buffer buf
      (set-buffer-multibyte nil))
    buf))

(defmacro erlext-in-work-buffer (&rest body)
  "Run `body' with current-buffer set to an empty work buffer."
  `(let ((erlext-work-buffer (erlext-get-work-buffer)))
     (unwind-protect
	 (with-current-buffer erlext-work-buffer ,@body)
       (kill-buffer erlext-work-buffer))))

(defun erlext-send-message (sock msg)
  (erlext-in-work-buffer (erlext-write-obj msg)
			 (erlext-write-message sock)))

(put 'erlext-in-work-buffer 'lisp-indent-function 0)

(defun erlext-open-socket (host port)
  (let ((io-buffer (generate-new-buffer " *ERL-IO*")))
    (with-current-buffer io-buffer
      (set-buffer-multibyte nil))
  (open-network-stream "erlext" io-buffer host port)))

(defun erlext-call (host port msg)
  "Make a synchronous call to an erlang node by opening a socket to
`host':`port', sending `msg' in erlext {packet, 4} format, reading a
reply, and then closing the socket"
  (let ((socket (erlext-open-socket host port)))
    (erlext-send-message socket msg)
    (let ((reply (erlext-read-message socket)))
      (delete-process socket)
      reply)))

(defun erlext-sync-call (sock msg)
  (erlext-send-message sock msg)
  (erlext-read-message sock))

;; ------------------------------------------------------------
;; Asynchronous interface: persistent connection, callback used to
;; handle incoming messages.
;; ------------------------------------------------------------

(defun erlext-open-async (host port handler &optional sentinel)
  (let ((proc (erlext-open-socket host port)))
    (when sentinel
      (set-process-sentinel proc sentinel))
    (with-current-buffer (process-buffer proc)
      ;; the process-buffer will have erlext-async-handler as the
      ;; function to call with decoded messages
      (make-local-variable 'erlext-async-handler)
      (setq erlext-async-handler handler))
    (set-process-filter proc 'erlext-accept-async-output)
    proc))

(defun erlext-accept-async-output (proc string)
  (with-current-buffer (process-buffer proc)
    ;; insert new text at the end
    (goto-char (point-max))
    (insert string)
    (while (erlext-have-whole-message-p)
      (erlext-async-decode))))

(defun erlext-async-decode ()
  "Called from inside the process buffer after input arrives and is
inserted. Tries to decode 1 or more complete messages."
  ;; see if we've got enough to do anything
  (let* ((process-buffer (current-buffer))
	 (handler erlext-async-handler)
	 (message-len (erlext-read4))
	 ;; + 4 to skip the length, and then + 1 to point to the
	 ;; character *after* the end
	 (message-end (+ 5 message-len))
	 (message (string-as-unibyte (buffer-substring 5 message-end))))
    ;; snarfed the message: remove it from the buffer
    (delete-region 1 message-end)
    (erlext-in-work-buffer
      (insert message)
      (goto-char (point-min))
      (let ((msg (erlext-read-available-obj)))
	(with-current-buffer process-buffer
	  (funcall handler msg))))))

(defun erlext-have-whole-message-p ()
  "Predicate to see if the buffer has the full length of a
message. Checks from the start, and leaves the point there."
  (cond ((>= (point-max) 4)
	 (goto-char (point-min))
	 (prog1 (> (point-max) (+ 4 (erlext-read4)))
	   (goto-char (point-min))))))

;; testing

(defun erlext-async-test ()
  (interactive)
  (erlext-open-async "localhost"
		     8866
		     'erlext-async-test-handler))

(defun erlext-async-test-handler (msg)
  (message (format "Msg: %S" msg)))

;; ------------------------------------------------------------
;; Marshalling
;; ------------------------------------------------------------

(defun erlext-write-obj (obj)
  (cond ((listp obj)			; lists at top since (symbolp '()) => t
	 (erlext-write-list obj))
	((stringp obj)
	 (erlext-write-binary obj))
	((symbolp obj)
	 (erlext-write-atom obj))
	((vectorp obj)
	 (erlext-write-tuple obj))
	((integerp obj)
	 (erlext-write-int obj))
	(t
	 (error "erlext can't marshal %S" obj))))

(defun erlext-write1 (n)
  (assert (integerp n))
  (insert n))
(defun erlext-write2 (n)
  (assert (integerp n))
  (insert (logand (ash n -8) 255)
	  (logand n 255)))
(defun erlext-write4 (n)
  (assert (integerp n))
  (insert (logand (ash n -24) 255)
	  (logand (ash n -16) 255)
	  (logand (ash n -8) 255)
	  (logand n 255)))
(defun erlext-writen (bytes)
  (assert (stringp bytes))
  (insert bytes))
(defun erlext-insert4 (n offset)
  (erlext-seek offset)
  (erlext-write4 n)
  (erlext-seek-end))
(defun erlext-seek (offset)
  (assert (integerp offset))
  (goto-char offset))
(defun erlext-seek-end ()
  (erlext-seek (point-max)))
;; proper types
(defun erlext-write-atom (atom)
  (assert (symbolp atom))
  (let* ((string (symbol-name atom))
	 (len    (length string)))
    (assert (<= len erlext-max-atom-length))
    (erlext-write1 (get-erlext-tag 'atom))
    (erlext-write2 (length string))
    (erlext-writen string)))
(defun erlext-write-int (n)
  (assert (integerp n))
  (cond ((= n (logand n 255))
	 (erlext-write1 (get-erlext-tag 'smallInt))
	 (erlext-write1 n))
	;; elisp has small numbers (24 bit?) - so 32bit on the wire is as
	;; far as we need bother supporting
	(t
	 (erlext-write1 (get-erlext-tag 'int))
	 (erlext-write4 n))))
(defun erlext-write-list (lst)
  (assert (listp lst))
  (if (null lst)
      (erlext-write-nil)
    (progn (erlext-write-list-head (length lst))
	   (mapc 'erlext-write-obj lst)
	   (erlext-write-nil))))
(defun erlext-write-string (str)
  (assert (stringp str))
  (erlext-write1 (get-erlext-tag 'string))
  (erlext-write2 (length str))
  (erlext-writen str))
(defun erlext-write-binary (str)
  (assert (stringp str))
  (erlext-write1 (get-erlext-tag 'bin))
  (erlext-write4 (length str))
  (erlext-writen str))
(defun erlext-write-nil ()
  (erlext-write1 (get-erlext-tag 'nil)))
(defun erlext-write-list-head (arity)
  (assert (> arity 0))
  (erlext-write1 (get-erlext-tag 'list))
  (erlext-write4 arity))
(defun erlext-write-tuple (vec)
  (assert (vectorp vec))
  (let ((arity (length vec)))
    (if (< arity 256)
	(progn (erlext-write1 (get-erlext-tag 'smallTuple))
	       (erlext-write1 arity))
      (progn (erlext-write1 (get-erlext-tag 'largeTuple))
	     (erlext-write4 arity))))
  (mapc 'erlext-write-obj vec))

(defun get-erlext-tag (tag)
  (get tag 'erlext-tag))

(defmacro erlext-insert-at-beginning (&rest body)
  `(progn (erlext-seek 0)
	  ,@body
	  (erlext-seek-end)))

(defun erlext-write-message (socket)
  (assert (processp socket))
  ;; version
  (erlext-insert-at-beginning (erlext-write1 erlext-protocol-version))
  ;; size
  (erlext-insert-at-beginning (erlext-write4 (buffer-size)))
  (process-send-region socket (point-min) (point-max)))

;; ------------------------------------------------------------
;; Unmarshalling
;; ------------------------------------------------------------

(defvar erlext-message-size nil)
(make-variable-buffer-local 'erlext-message-size)

(defun erlext-read-message (socket)
  (let (msg)
    (loop do (accept-process-output)
	  (setq msg (erlext-maybe-read-message socket))
	  until (not (processp msg))
	  finally return msg)))

;; Returns `socket' if the object isn't available yet, otherwise the
;; object that was read.  A socket is not terribly descriptive, but
;; it's distinct from any valid erlang message - so that's what I
;; used.
(defun erlext-maybe-read-message (socket)
  (assert (processp socket))
  (save-excursion
    (set-buffer (process-buffer socket))
    ;; flush out any old message
    (when erlext-message-size
      (goto-char (point-min))
      (delete-char erlext-message-size)
      (setq erlext-message-size nil))
    (cond
     ;; have we read enough to see how big the message is?
     ((>= (point-max) 4)
      (goto-char (point-min))
      (let ((msglen (erlext-read4)))
	(if (<= (point-max) msglen)	; read whole message?
	    socket
	  ;; we have the whole message
	  (erlext-read-available-obj))))
     (t socket))))

(defun erlext-read-available-obj ()
  (let ((version (erlext-read1)))
    (assert (= version erlext-protocol-version))
    (erlext-read-obj)))

(defmacro erlext-tag-case (obj &rest clauses)
  `(let (($tag ,obj))
     (cond ,@(mapcar 'erlext-tag-case-clause clauses))))

(put 'erlext-tag-case 'lisp-indent-function 0)

(defun erlext-tag-case-clause (clause)
  (let ((tag  (car clause))
	(body (cdr clause)))
    (if (eq tag 'else)
	`(t ,@body)
      `((= $tag (get-erlext-tag ',tag))
	,@body))))

(defun erlext-read-obj ()
  (let ((tag     (erlext-read1)))
    (erlext-tag-case
      tag
      (smallInt   (erlext-read1))
      (int        (erlext-read4))
      (atom       (erlext-read-atom))
      (smallTuple (erlext-read-small-tuple))
      (largeTuple (erlext-read-large-tuple))
      (list       (erlext-read-list))
      (string     (erlext-read-string))
      (bin        (erlext-read-binary))
      (nil        nil)
      (else       (error "Unknown tag: %S" tag)))))

(defun erlext-read-octet ()
  (prog1 (following-char) (forward-char 1)))

(defun erlext-read1 ()
  (erlext-read-octet))
(defun erlext-read2 ()
  (logior (ash (erlext-read1) 8)
	  (erlext-read1)))
(defun erlext-read4 ()
  (logior (ash (erlext-read1) 24)
	  (ash (erlext-read1) 16)
	  (ash (erlext-read1) 8)
	  (erlext-read1)))
(defun erlext-readn (n)
  (assert (integerp n))
  (let ((start (point))
	(end   (+ (point) n)))
    (prog1 (string-as-unibyte (buffer-substring start end))
      (goto-char end))))
(defun erlext-read-atom ()
  (let ((length (erlext-read2)))
    (intern (erlext-readn length))))
(defun erlext-read-small-tuple ()
  (let ((arity (erlext-read1)))
    (apply 'vector (ntimes (lambda () (erlext-read-obj)) arity))))
(defun erlext-read-large-tuple ()
  (let ((arity (erlext-read4)))
    (apply 'vector (ntimes (lambda () (erlext-read-obj)) arity))))
(defun erlext-read-list ()
  (let ((arity (erlext-read4)))
    (ntimes (lambda () (erlext-read-obj)) arity)))

(defun ntimes (f n)
  "Apply function `f' with no arguments `n' times and return a list of
results"
  (assert (>= n 0))
  (if (= n 0)
      nil
    (cons (funcall f) (ntimes f (- n 1)))))

(defun erlext-read-string ()
  (erlext-readn (erlext-read2)))

(defun erlext-read-binary ()
  (erlext-readn (erlext-read4)))

(defun erlext-binary-to-term (str)
  (assert (stringp str))
  (erlext-in-work-buffer
    (insert str)
    (goto-char (point-min))
    (erlext-read-available-obj)))

(defun erlext-term-to-binary (term)
  (erlext-in-work-buffer
    (insert erlext-protocol-version)
    (erlext-write-obj term)
    (buffer-string)))

(provide 'erlext)

