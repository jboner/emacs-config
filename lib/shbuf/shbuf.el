;;; shbuf.el --- Network-shared buffers minor mode

;; Copyleft (C) 2001 Luke Gorrie <luke@bluetail.com>
;; Version: $Id: shbuf.el,v 1.2 2001/05/15 00:04:30 luke Exp $
;; Keywords: network, erlang, shared buffers

;;; Boring Commentary:

;; This program implements shared buffers, or "shbufs", which are
;; shared by many emacsen on the network and edited by them
;; one-at-a-time with the changes broadcast to the others. The effect
;; is that you interactively see the changes that other people make.
;;≈≈

;; Here's how it works:
;;
;; We connect to a server program which holds the master copy of a
;; buffer. It sends us the current version, and we insert that into a
;; new "*shbuf*" buffer.
;;
;; Now we are in "peer" mode: we're a passive observer of the buffer,
;; which is set to read-only. In peer mode we can receive asynchronous
;; messages as a result of someone else making an edit. These messages
;; tell us how to update our copy: "replace the region between START
;; and END with this TEXT". When we get them they're applied to our
;; buffer so that we're in sync with the master copy.
;;
;; When we decide to make a change, we do a "takeover" of the buffer
;; to get from peer mode to "master" mode. Because we're using an
;; asynchronous messaging interface, we just send a "takeover" message
;; to the server and then carry on without taking any immediate
;; action. When the server receives our takeover request, it records
;; the fact that we're the only one allowed to write and it sends us
;; back a "you are master" message.  It also informs the previous
;; master that he is now downgraded to "peer". When our message
;; handler gets the "you are master", it marks our buffer as editable
;; and we're truely in master mode.
;;
;; While we're in master mode, we can make changes to the buffer using
;; all the normal emacs commands. The trick is that whenever a change
;; is made to the text, a hook is triggered which sends a message to
;; the server saying "I changed the region between START and END to
;; TEXT". When the server receives that, it will usually incoporate
;; our change into its master copy, broadcast it to our peers, and
;; send us back an "accepted" message. It's also possible that the
;; server receives a "takeover" from someone else before it gets our
;; "change" messages. When we get a "you are peer" message and have
;; some changes that haven't been accepted, we know that we are in
;; this "collision" case. We resolve it be undoing all of our
;; unaccepted changes - then we're back in sync with the server.
;;
;; And that's how it works. One person is master at a time, and at
;; anytime someone else can take over. All buffers are kept in sync
;; with the master copy on the server.
;;
;; How it *really* works at a lower level:
;;
;; To successfully rollback in a "collision" case, we need to know
;; which changes we've made locally but haven't had accepted. To do
;; this we maintain the `shbuf-undo-list', adding elements as we make
;; changes and removing them as we get accept messages. When we get a
;; "you are peer", we apply all the undos that we have, and we're back
;; in sync.
;;
;; The server part is written in the Wonderful programming language
;; Erlang (http://www.erlang.org/), because elisp doesn't have
;; listen-sockets.
;;
;; The protocol on the wire is the "Erlang External Term Format",
;; which is a set of binary encodings for erlang's datatypes. These
;; datatypes are very similar to lisp's so we can just throw lisp
;; terms like lists, vectors, symbols, strings, etc at the server and
;; the `erlext' package marshals them for us. Similarly, incoming
;; messages arrive in a friendly lispy format.
;;
;; Today the whole thing is implemented like an application. Perhaps
;; it could be made more library-like so that other programs
;; (e.g. chat-rooms) could use shbufs as a communications medium.

(require 'cl)
(require 'erlext)

(defvar shbuf-mode nil
  "(internal variable)
*Non-nil when in shbuf-minor-mode.")

(defvar shbuf-socket nil
  "(internal variable)
Socket connection to the shbuf server.")

(defvar shbuf-undo-list nil
  "(internal variable)
List of undo actions corresponding to changes that we've sent to the
server but haven't been acknowledged. Most-recent-first.

For simplicity, each element X of this list can be used in (eval X) to
affect the undo.

(More efficient to make this a queue)")

(defconst shbuf-default-port 9091
  "Default TCP port to connect to the shbuf server on.")

(defvar shbuf-deleted-text nil
  "(internal variable)
Very internal indeed.")

(defun shbuf-mode (&optional arg)
  (interactive "P")
  (make-local-variable 'shbuf-mode)
  (setq shbuf-mode (not (or (null arg)
			    (< arg 0)))))

(defun shbuf-connect (host &optional port)
  "Connect to a shbuf server to partake of the thrills of shared
editing.

This function creates a new \"*shbuf*\" buffer."
  (interactive "sHost: ")
  (unless port
    (setq port shbuf-default-port))
  (let ((buffer (generate-new-buffer "*shbuf*")))
    ;; Enter the *shbuf* buffer that the user will see.
    (switch-to-buffer buffer)
    (setq buffer-read-only t)
    (make-local-variable 'shbuf-socket)
    (make-variable-buffer-local 'after-change-functions)
    (make-local-variable 'shbuf-undo-list)
    (make-local-variable 'shbuf-deleted-text)
    (make-variable-buffer-local 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'shbuf-kill-buffer-hook)
    (add-to-list 'before-change-functions 'shbuf-before-change-hook)
    (add-to-list 'after-change-functions 'shbuf-after-change-hook)
    (shbuf-mode 1)
    (setq shbuf-socket (erlext-open-async host
					  port
					  'shbuf-erlext-handler
					  'shbuf-erlext-sentinel))
    ;; Give the process's buffer a local variable `shbuf-buffer' that
    ;; points to the buffer that the user views.
    (with-current-buffer (process-buffer shbuf-socket)
      (set-buffer-multibyte nil)
      (make-local-variable 'shbuf-buffer)
      (setq shbuf-buffer buffer))
    t))

;;; Callbacks

(defun shbuf-kill-buffer-hook ()
  (when shbuf-socket
    (delete-process shbuf-socket)
    (kill-buffer (process-buffer shbuf-socket))))

(defun shbuf-erlext-sentinel (proc string)
  "Sentinel called to cleanup when the socket closes. (Hopefully not
at other times..?)"
  ;; assuming that the socket is dead
  (shbuf-mode -1)
  ;; Try to let the user know the socket is dead: leave shbuf minor
  ;; mode, and make the buffer read-only.
  (with-current-buffer shbuf-buffer
    (setq shbuf-socket nil)
    (setq shbuf-mode nil)
    (setq buffer-read-only t))
  (kill-buffer (current-buffer)))

(defun shbuf-erlext-handler (msg)
  "Asynchronous message handler."
  ;; `Msg' is one of these:
  ;;
  ;; Control mesages:
  ;;   you_are_master
  ;;   you_are_peer
  ;; Replies to change requests:
  ;;   accepted
  ;;   rejected
  ;; State updates:
  ;;   [text STRING]
  ;;   [replace START END STRING]
  (with-current-buffer shbuf-buffer
    (let ((inhibit-read-only t)
	  ;; inhibit our change hooks inside here too: we don't want to
	  ;; bounce back the changes that we incorporate from the
	  ;; server.
	  (before-change-functions (remove 'shbuf-before-change-hook
					   before-change-functions))
	  (after-change-functions (remove 'shbuf-after-change-hook
					  after-change-functions)))
      (cond ((symbolp msg)
	     (case msg
	       ((you_are_master)
		(message (format "%s: Now master." (buffer-name)))
		(setq buffer-read-only nil))
	       ((you_are_peer)
		;; Rollback any changes that didn't get accepted while
		;; we were master.
		(dolist (form shbuf-undo-list)
		  (message (format "UNDOING: %S" shbuf-undo-list))
		  (eval form))
		(setq shbuf-undo-list nil)
		(setq buffer-read-only t)
		(message (format "%s: taken over - now peer" (buffer-name))))
	       ((accepted)
		;; Accepted: this means that the last element of our
		;; undo list has been committed and we can forget about
		;; it.
		(setq shbuf-undo-list (butlast shbuf-undo-list)))))
	    ((vectorp msg)
	     (case (elt msg 0)
	       ((text)		; [text STRING]
		(let ((string (elt msg 1)))
		  (erase-buffer)
		  (insert string)))
	       ((replace)		; [replace START END STRING]
		(let ((start (elt msg 1))
		      (end   (elt msg 2))
		      (text  (elt msg 3)))
		  (delete-region start end)
		  (goto-char start)
		  (insert text)))))))))

(defun shbuf-before-change-hook (start end)
  "Hook to record which area was removed, which
`shbuf-after-change-hook' then uses to build the correct undo
action."
  (setq shbuf-deleted-text (buffer-substring start end)))

(defun shbuf-after-change-hook (start end len)
  ;; NB: The argument `end' is the new end position after the change
  ;; has been made. However, in the wire protocol `end' means the end
  ;; position before the change.
  (let ((repl-start start)
	(repl-end (+ start len))
	(repl-text (buffer-substring start end)))
    ;; Make the undo action
    (let ((undo-form
	   `(progn
	      ;; kill the added text
	      (delete-region ,start ,end)
	      ;; restore the old
	      (goto-char ,start)
	      (insert ,shbuf-deleted-text))))
      (push undo-form shbuf-undo-list))
    ;; Inform the server of our change
    (erlext-send-message shbuf-socket
			 `[change [replace ,repl-start
					   ,repl-end
					   ,repl-text]])))

;;; Commands

(defun shbuf-takeover ()
  (interactive)
  (unless shbuf-socket
    (error "Not connected!"))
  (message (format "%s: Requesting takeover.." (buffer-name)))
  (erlext-send-message shbuf-socket 'takeover))

;;; Install

(add-to-list 'minor-mode-alist '(shbuf-mode " ShBuf"))

;; [insert code to give us a keymap with shbuf-takeover in it here]

(provide 'shbuf)

