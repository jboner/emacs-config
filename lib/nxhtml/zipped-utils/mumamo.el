;;; mumamo.el --- Different major modes for parts of buffer
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Maintainer:
;; Created: Fri Mar 09 18:15:25 2007
(defconst mumamo:version "0.64") ;;Version:
;; Lxast-Updated: Fri May 25 02:04:34 2007 (7200 +0200)
;; URL: http://OurComments.org/Emacs/Emacs.html
;; Keywords:
;; Compatibility:
;;
;; FXeatures that might be required by this library:
;;
;;   `cl'.
;;
;;
;; Thanks to Stefan Monnier for beeing a good and knowledgeable
;; speaking partner while I am trying to develop this.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; MUltiple MAjor MOdes (mumamo) in a buffer.
;;
;; When `mumamo-mode' minor mode is on the buffer is divided into
;; chunks , each with its own major mode. The actual major mode used
;; in the buffer is changed when moving point between these
;; chunks.
;;
;; The chunks are fontified according the major mode assigned to
;; them. Basically Mumamo handles only major modes that uses
;; jit-lock. However as a special effort also `nxml-mode' and
;; derivatives thereof are handled. Since it seems impossible to me to
;; restrict those major modes fontification to only a chunk without
;; changing `nxml-mode' the fontification is instead done by
;; `html-mode'/`sgml-mode' for chunks using `nxml-mode' and its
;; derivates.
;;
;;
;;; Usage:
;;
;; This is part of nXhtml and to use it there you normally do not need
;; to do anything. However for other cases see below.
;;
;; In the most simple case just load this file and then in the buffer
;; where you want to use multiple major modes just do
;;
;;   M-x mumamo-mode
;;
;; Then, if mumamo-mode knows how to handle the current major mode,
;; you will immediately have a buffer with multiple major modes (in
;; mumamo's sense of course).
;;
;; Otherwise mumamo will ask you for the chunk family to use. A chunk
;; family is just a main major modes and submodes.
;;
;; If this does not work as you would like it to then perhaps some
;; customization or extension is needed.  There is a customization
;; group `mumamo'.  Please see `mumamo-mode' for information about
;; extending mumamo.
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Background:
;;
;; This is yet another way to try to get different major modes for
;; different chunks of a buffer to work. (I borrowed the term "chunk"
;; here from multi-mode.el.) I am aware of two main previous elisp
;; packages that tries to do this, multi-mode.el and mmm-mode.el. (See
;; http://www.emacswiki.org/cgi-bin/wiki/MultipleModes where there are
;; also some other packages mentioned.) The solutions in those are a
;; bit different from the approach here.
;;
;; `mumamo-mode' divides the buffer into chunks (where each chunk is
;; appropirate for one major mode) like the other packages. Then it
;; works a bit different. `mumamo-mode' works like this:
;;
;; 1) When `point' moves into one of those chunks major mode is
;;    switched to the one appropriate for that chunk.
;;
;; 2) Syntax highlighting is done per chunk using the major mode
;;    syntax highlighting for that chunk. This is done by replacing
;;    the value of `font-lock-fontify-region-function'.
;;
;; 3) Indentation is handled similarly by using the appropriate value
;;    of `indent-line-function'.
;;
;; The idéa of doing it this way is of course based on a hope that
;; switching major mode in 1 should be quick. I found that it took
;; from 0 - 62 000 ms, typically 0 - 16 000 ms on a 3ghz cpu. However
;; unfortunately this is not the whole truth. It could take longer
;; time, depending on what is run in `after-change-major-mode-hook'
;; and `change-major-mode-hook'.
;;
;; Since the intention is to set up the new major mode the same way as
;; it should have been done if this was a major mode for the whole
;; buffer these hooks must be run. However if this idea is developed
;; further some of the things done in these hooks (like switching on
;; minor modes) could perhaps be streamlined so that switching minor
;; modes off and then on again could be avoided.
;;
;; Another problem is that the major modes must use
;; `font-lock-fontify-region-function' for 2 to work. Currently the
;; only major modes I know that does not do this are `nxml-mode' and
;; its derivatives. (They are however not yet a part of Emacs - but I
;; hope they will be.)
;;
;; The indentation in 3 is currently working ok, but with the price
;; that major mode actually have to be switched when indenting a
;; region including different major mode chunks. That seems a bit
;; unnecessary and I believe the indentation functions for the various
;; major modes could be rewritten to avoid this. (Or `mumamo-mode'
;; could do this better, but I do not know how right now.)
;;
;; See also "Known bugs and problems etc" below.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Known bugs:
;;
;; - See the various FIX-ME + below.
;;
;; - Does not handle comments in main major mode entirely correct
;;   since chunk dividing is also done in the comments.
;;
;;
;;
;;; Known problems:
;;
;; - There is no way in Emacs to tell a mode not to change
;;   fontification at changing to or from that mode.
;;
;; - Only two levels, main and exception. Probably enough right now.
;;
;; - Maybe chunks for submodes should be delimited to whole lines when
;;   applicable?
;;
;; - The dividing into chunk is not that simple as I first thought. I
;;   have not gone through the logic of this very carefully. Perhaps
;;   that is needed. The current logic is mainly in
;;   `mumamo-get-chunk-at' and `mumamo-chunk'. (Some other routines
;;   tries to behave like `mumamo-chunk' too: `mumamo-chunk-attr=' and
;;   `mumamo-easy-make-chunk-fun'.
;;
;;
;;
;;; Fixed:
;;
;; - Validation error mark from nxhtml-mode not removed when changing
;;   chunk setttings.
;;
;; - mumamo hooks not run at right places.
;;
;; - Can't turn off mumamo-mode when mumamo-global-mode is on.
;;
;; - mumamo-global-mode causes refontification when switching mode.
;;
;; - When using JSP: Error during redisplay: (wrong-type-argument
;;   stringp nil) Unfortunately this is inside
;;   font-lock-fontify-region, in java-mode.
;;
;; - hs-minor-mode adds something strange to
;;   `change-major-mode-hook'. What to do with that? Fixed with a
;;   change in CVS Emacs.
;;
;; - Still some problems with attr=. Works most of the time, but
;;   sometimes I get an error?? Gone?
;;
;; - Too much may be refontified for small changes currently. Fixed
;;   with a change in `mumamo-remove-chunk-overlays'.
;;
;; - !!!!!!! The undo list seems to get confused by switching
;;   mode. Seems to be cured.
;;
;; - Sometimes nxhtml-mode fontifies the buffer, but not now??? I
;;   believe this is fixed.
;;
;; - Some part of the html-mode fontification is not done in
;;   nxhtml-mode.
;;
;; - Chunks do not get recomputed correctly after changes.
;;
;; - Multiline style= makes the tag fontification black
;;   (css-mode?). Probably redo fontifying of prev chunk. This is due
;;   to the way fontification of a block starts.
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging etc

(eval-when-compile (require 'cl)) ;; assert, dolist

;; Some debugging aids:
(defconst mumamo-debug nil)
(defmacro mumamo-msgfntfy(format-string &rest args)
  ;;(list 'apply (list 'quote 'message) format-string (append '(list) args))
  ;;(list 'apply (list 'quote 'message) (list 'concat "%s: " format-string) (list 'get-internal-run-time) (append '(list) args))
  )
;;(mumamo-msgfntfy "my-format=%s" (get-internal-run-time))

(defun mumamo-list-timers()
  (interactive)
  (mapatoms (lambda (s)
              (when (timerp s)
                (message "%s: %s" (symbol-name s) (symbol-value s))))))

(defun mumamo-bt-to-msg(msg)
  (mumamo-msgfntfy "%s: %s" msg
           (with-output-to-string
             (backtrace))))

(defvar mumamo-display-error-lwarn nil)
(defvar mumamo-display-error-stop nil)

;;(run-with-idle-timer 1 nil 'mumamo-show-report-message)
(defun mumamo-show-report-message()
  (let ((msg "MuMaMo error, please look in the *Message* buffer"))
    (put-text-property 0 (length msg) 'face 'highlight msg)
    (message "%s" msg)))

;;(mumamo-display-error 'test-lwarn-type "testing 1=%s, 2=%s" "one" 'two)
(defun mumamo-display-error(lwarn-type msg &rest args)
  "Display mumamo special errors.
These errors are those happening during fontification.

A simple error message is shown by 'message. A backtrace colored
with the 'highlight face is placed in the message buffer.

If `mumamo-display-error-lwarn' is non-nil a warning is written
to *Warnings* and this buffer is displayed.

If `mumamo-display-error-stop' is non-nil an error that may stop
fontification is raised."

  ;; Warnings are sometimes disturbning, make it optional:
  (when mumamo-display-error-lwarn
    (apply 'lwarn lwarn-type :error msg args))

  (let ((msg2 (concat "%s: " msg))
        (bt (with-output-to-string (backtrace)))
        (start (+ (with-current-buffer "*Messages*"
                     (point-max))
                  0)))

    ;; Output message together with backtrace:
    (apply 'message msg2 lwarn-type args)

    ;; Does not work here, why?
    ;;(put-text-property 0 (length bt) 'face 'highlight bt)

    ;; Backtrace to message buffer:
    (message "%s" bt)
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (backward-char)
      (put-text-property start (point)
                         'face 'highlight))

    ;; Output message once again so the user can see it:
    (apply 'message msg2 lwarn-type args)
    (run-with-idle-timer 1 nil 'mumamo-show-report-message)

    ;; Stop fontifying:
    (when mumamo-display-error-stop
      (apply 'error msg2 lwarn-type args))))

(defgroup mumamo nil
  "Customization group for multiple major modes in a buffer.
These settings are for the minor mode `mumamo-mode'."
  :group 'editing
  :group 'languages
  :group 'sgml
  :group 'nxhtml
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation

;; FIX-ME: Indentation in perl here doc indents the ending mark which
;; corrupts the perl here doc.

(defcustom mumamo-submode-indent-offset 2
  "Indentation of submode relative main major mode.
See also `mumamo-submode-indent-offset-0'."
  :type 'integer
  :group 'mumamo)

(defcustom mumamo-submode-indent-offset-0 0
  "Indentation of submode at column 0.
This value overrides `mumamo-submode-indent-offset' when the main
major mode above has indentation 0."
  :type 'integer
  :group 'mumamo)

;; The indent-line-function
(defun mumamo-indent-line-function()
  (let ((here (point-marker))
        (before-text (<= (current-column) (current-indentation))))
    (mumamo-indent-line-function-1 nil nil)
    ;; If the marker was in the indentation part strange things happen
    ;; if we try to go back to the marker, at least in php-mode parts.
    (if before-text
        (back-to-indentation)
      (goto-char here))))

(defun mumamo-indent-line-majormode()
  "Return major mode for indenting current line."
  (let* ((ovl (mumamo-get-chunk-at (line-end-position)))
         (major (overlay-get ovl 'mumamo-major-mode))
         (main-major (mumamo-main-major-mode)))
    (unless (eq major main-major)
      (setq ovl (mumamo-get-chunk-at (line-beginning-position)))
      (setq major (overlay-get ovl 'mumamo-major-mode)))
    major))

;; FIX-ME: This is a terrible way to get the indentation the major
;; mode will do, but I can not find any better ... -- The trouble is
;; that Emacs specifies that indent-line-function should indent. It
;; would have been better here if it just returned the indentation to
;; use.
(defun mumamo-get-major-indent-column()
  (let ((got-indent 0)
        (this-line (line-beginning-position)))
    (save-restriction
      (let ((chunk (mumamo-get-chunk-at (point))))
        (narrow-to-region (overlay-start chunk)
                          (overlay-end chunk))
        (mumamo-save-buffer-state-with-undo nil
          (mumamo-call-indent-line)
          (setq got-indent (current-indentation))
          (undo-start)
          (while (and (not (eq t pending-undo-list))
                      pending-undo-list)
            (undo-more 1)))))
    (goto-char this-line)
    got-indent))

(defun mumamo-indent-line-function-1(prev-line-major last-major-indent)
  (let ((this-line-major (mumamo-indent-line-majormode))
        major-indent-line-function
        (main-major (mumamo-main-major-mode))
        (old-indent (current-indentation))
        want-indent ;; The indentation we desire
        got-indent
        this-pending-undo-list)
    (mumamo-msgfntfy "===> mumamo-indent-line-function-1 %s %s, this-line-major=%s" prev-line-major last-major-indent this-line-major)
    (unless prev-line-major
      (save-excursion
        (goto-char (line-beginning-position 0))
        (setq prev-line-major (mumamo-indent-line-majormode))))
    (unless last-major-indent
      (save-excursion
        (while (not last-major-indent)
          (if (bobp)
              (setq last-major-indent 0)
            (goto-char (line-beginning-position 0))
            (when (eq main-major (mumamo-indent-line-majormode))
              (skip-chars-forward " \t")
              (unless (eolp)
                (setq last-major-indent (current-column))))))))
    (if (eq prev-line-major this-line-major)
        (progn
          ;; We have to change major mode, because we know nothing
          ;; about the requirements of the indent-line-function:
          (unless (eq this-line-major major-mode)
            (mumamo-set-major this-line-major))
          (if (eq this-line-major main-major)
              ;;(mumamo-with-major-mode-setup-nosubst this-line-major
              ;;  (funcall indent-line-function))
              (mumamo-call-indent-line)
            (setq got-indent (mumamo-get-major-indent-column))
            (setq want-indent (if (= 0 last-major-indent)
                                  mumamo-submode-indent-offset-0
                                (+ last-major-indent mumamo-submode-indent-offset )))
            (when (> got-indent want-indent)
              (setq want-indent got-indent))
            (unless (= want-indent old-indent)
              (indent-line-to want-indent))))
      (if (eq this-line-major main-major)
          (unless (= old-indent last-major-indent)
            (indent-line-to last-major-indent))
        (setq want-indent (if (= 0 last-major-indent)
                              mumamo-submode-indent-offset-0
                            (+ last-major-indent mumamo-submode-indent-offset )))
        (unless (= want-indent old-indent)
          (indent-line-to want-indent))))))

(defun mumamo-call-indent-line ()
  (let ((indent-func (cadr (assq major-mode mumamo-indent-line-alist))))
    (condition-case err
        ;; FIX-ME: Get the correct syntax-table, but why is this
        ;; necessary here??? It should be correct already:
        (mumamo-with-major-mode-setup major-mode
          (funcall indent-func))
      (error
       (lwarn 'mumamo-call-indent-line :warning
              "(funcall indent-func),\n  indent-func=%s => error=%s"
              indent-func
              (error-message-string err))))))

(defun mumamo-indent-region-function (start end)
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (let ((old-point -1))
      (while (and (< (point) end)
                  (not (= old-point (point))))
        (or (and (bolp) (eolp))
            (mumamo-indent-line-function-1 nil nil))
        (setq old-point (point))
        (forward-line 1)))
    (message "Ready indenting region")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fontification base

;; Borrowed from font-lock.el
(defmacro mumamo-save-buffer-state (varlist &rest body)
  "Bind variables according to VARLIST and eval BODY restoring buffer state."
  (declare (indent 1) (debug let))
  (let ((modified (make-symbol "modified")))
    `(let* ,(append varlist
                    `((,modified (buffer-modified-p))
                      (buffer-undo-list t)
                      (inhibit-read-only t)
                      (inhibit-point-motion-hooks t)
                      (inhibit-modification-hooks t)
                      deactivate-mark
                      buffer-file-name
                      buffer-file-truename))
       (progn
         ,@body)
       (unless ,modified
         (restore-buffer-modified-p nil)))))

(defmacro mumamo-save-buffer-state-with-undo (varlist &rest body)
  "Bind variables according to VARLIST and eval BODY restoring buffer state."
  (declare (indent 1) (debug let))
  (let ((modified (make-symbol "modified")))
    `(let* ,(append varlist
                    `((,modified (buffer-modified-p))
                      (buffer-undo-list nil)
                      ;;(inhibit-read-only t)
                      ;;(inhibit-point-motion-hooks t)
                      ;;(inhibit-modification-hooks t)
                      deactivate-mark
                      buffer-file-name
                      buffer-file-truename))
       (progn
         ,@body)
       (unless ,modified
         (restore-buffer-modified-p nil)))))


(defconst mumamo-internal-major-modes-alist nil
  "Alist with info for different modes.
Internal use only. This is automatically set up by
`mumamo-get-major-mode-setup'.")

;; FIX-ME: Should this be a defun? What happens at compilation? What
;; use is it with a defmacro now?
(defmacro mumamo-with-major-mode-setup (major &rest body)
  "With some local variables set as in major mode run body.
See `mumamo-fetch-major-mode-setup' for which local variables
that are set."
  `(mumamo-fun-with-major-mode-setup ,major (quote ,body)))
(defun mumamo-fun-with-major-mode-setup (major &rest body)
  (let ((major-info (mumamo-get-major-mode-setup major))
        (major-mode (mumamo-get-major-mode-fontification major)))
    (eval
     `(let ,major-info
        (if (and (boundp 'major-syntax-table)
                 major-syntax-table)
            (with-syntax-table major-syntax-table
              (eval (caar body)))
          (eval (caar body))
          )))))

;; (defmacro mumamo-with-major-mode-setup-1 (major-info body)
;;   (declare (indent 1) (debug t))
;;   `(let ,major-info
;;      (let ((b2 (quote (car ,body))))
;;       `(with-syntax-table major-syntax-table
;;             ,b2))))

(defmacro mumamo-with-major-mode-setup-nosubst (major &rest body)
  "With some local variables set as in major mode run body.
Like `mumamo-with-major-mode-setup' but does not substitue the
actual `major-mode'."
  (declare (indent 1) (debug t))
  `(let ((major-info (mumamo-get-major-mode-setup ,major))
         (major-mode ,major))
     (eval
      (list 'let major-info
            (car ',body)))))

;; Keep this separate for easier debugging.
(defun mumamo-do-fontify (start end verbose narrow-to-chunk)
  (mumamo-msgfntfy "mumamo-do-fontify %s %s" start end)
  (condition-case err
      (when t
        (let ((font-lock-dont-widen narrow-to-chunk))
          (if narrow-to-chunk
              (save-restriction
                (mumamo-msgfntfy "NARROW TO CHUNK!")
                (narrow-to-region start end)
                (font-lock-fontify-region start end verbose))
            (font-lock-fontify-region start end verbose))))
    (error
     (when mumamo-debug
       (lwarn 'mumamo-do-fontify :error
              "mumamo-do-fontify m=%s, s=%s, e=%s: %s" major start end
              (error-message-string err))))))

(defun mumamo-do-unfontify (start end)
  (condition-case err
      (save-restriction
        (widen)
        (font-lock-unfontify-region start end)
        (mumamo-save-buffer-state nil
            (remove-list-of-text-properties start end
                                            (list 'fontified))))
    (error
     (mumamo-display-error 'mumamo-do-unfontify "%s" (error-message-string err)))))

(defun mumamo-fontify-region-with(start end verbose major narrow-to-chunk)
  "Fontify from start to end as in major mode MAJOR."
  ;; 'fontified is always t here due to the way jit-lock works!
  (mumamo-msgfntfy "mumamo-fontify-region-with %s %s %s %s, ff=%s" start end verbose major (get-text-property start 'fontified))
  (when mumamo-just-changed-major ;(and (= 1 start) (get-text-property start 'fontified))
    (mumamo-bt-to-msg
     (format "mumamo-fontify-region-with %s %s %s %s, ff=%s, mjcm=%s" start end verbose major (get-text-property start 'fontified)
             mumamo-just-changed-major)))
  (condition-case err
      (mumamo-with-major-mode-setup major
                                    (mumamo-do-fontify start end verbose narrow-to-chunk))
    (error
     (mumamo-display-error 'mumamo-fontify-region-with "%s"
                           (error-message-string err)))))

(defun mumamo-unfontify-region-with(start end major)
  "Unfontify from start to end as in major mode MAJOR."
  (mumamo-msgfntfy "mumamo-unfontify-region-with %s %s %s, ff=%s" start end major (get-text-property start 'fontified))
  ;;(when (and (= 1 start) (get-text-property start 'fontified)) (bt-to-msg))
  (mumamo-with-major-mode-setup major
    (mumamo-do-unfontify start end)))

(defun mumamo-unfontify-region(start end)
  (mumamo-msgfntfy "mumamo-unfontify-region %s %s" start end))


(defun mumamo-unfontify-buffer()
  (mumamo-msgfntfy "===> mumamo-unfontify-buffer called")
;;   (unless mumamo-mode
;;     (mumamo-msgfntfy "----- removing 'fontified from mumamo-unfontify-buffer")
;;     (mumamo-save-buffer-state nil
;;         (remove-text-properties (point-min) (point-max) '(fontified))))
  )

(defun mumamo-fontify-buffer()
  "For `font-lock-fontify-buffer-function' calls."
  (mumamo-msgfntfy "===> mumamo-fontify-buffer-function called")
  ;;(font-lock-default-fontify-buffer)
  )


(defun mumamo-unfontify-chunk(chunk)
  (let ((major (overlay-get chunk 'mumamo-major-mode)))
    (mumamo-unfontify-region-with (overlay-start chunk)
                                  (overlay-end   chunk)
                                  major)))

(defun mumamo-fontify-region(start end &optional verbose)
  "Fontify between START and END.
Take the major mode chunks into account while doing this.

The value of `font-lock-fontify-region-function' when
`mumamo-mode' is on is this function."
  (mumamo-msgfntfy "++++++ mumamo-fontify-region %s %s %s, skip=%s" start end verbose mumamo-just-changed-major)
  ;;(lwarn 't :warning "++++++ mumamo-fontify-region %s %s %s, skip=%s" start end verbose mumamo-just-changed-major)
  (if mumamo-just-changed-major
      (mumamo-display-error 'mumamo-fontify-region "Should not happen")
    ;;(when (> end (window-end)) (setq mumamo-just-changed-major nil))
    (condition-case err
        (mumamo-fontify-region-1 start end verbose)
      (error
       (mumamo-display-error 'mumamo-fontify-region "%s"
                             (error-message-string err))))))

(defun mumamo-fontify-region-1(start end verbose)
  (save-match-data
    (let* ((old-point (point))
           (here start)
           (main-major (mumamo-main-major-mode))
           (all-main t)
           narrow-to-chunk
           ;; Fix-me: I am not sure about how the queueing for
           ;; fontification is done. I suspect however that a new
           ;; request can be made after changes while fontifiying
           ;; here. I believe that request will remove 'fontified t so
           ;; I test that here for now. Indeed it happens that
           ;; 'fontified is nil. However a new problem occurs, now it
           ;; looks like everything is not refontified as perhaps
           ;; needed. The overlays can get removed and not put back
           ;; and everything is not refontified as it should be.
           (fontified-t ;;(get-text-property here 'fontified)
                        t)
           (first-new-ovl nil)
           (last-new-ovl nil))
      (while (and fontified-t
                  (< here end))
        (let* ((chunk (mumamo-get-chunk-at here))
               (chunk-min (overlay-start chunk))
               (chunk-max (overlay-end chunk))
               (max)                    ; (min chunk-max end))
               (major (overlay-get chunk 'mumamo-major-mode))
               (narrow-to-chunk (overlay-get chunk 'mumamo-narrow))
               prev-major
               prev-chunk
               )
          (if first-new-ovl
              (setq last-new-ovl chunk)
            (setq last-new-ovl chunk)
            (setq first-new-ovl chunk))
          ;; Only first loop:
          (when (= here start)
            ;; FIX-ME: Testing handling this:
            ;;(setq font-lock-syntactically-fontified (1- here))
            ;; FIX-ME: Is all-main=t a performance problem?
            (when (and all-main
                       ;;(or (>= chunk-max end)
                       ;;(eq major main-major)
                       )
              (mumamo-fontify-region-with start end verbose main-major nil)))
          (mumamo-msgfntfy "*** here=%s, mumamo-fontify-region.chunk=%s" here chunk)
          ;;(when (< end chunk-max) (setq end chunk-max))
          (setq max (min chunk-max end))
          (when narrow-to-chunk
            (mumamo-msgfntfy "mumamo-fontify-region: narrow-to-chunk %s" narrow-to-chunk))
          (assert chunk)
          (assert (overlay-buffer chunk))
          (assert chunk-min)
          (assert chunk-max)
          (assert major)
          ;; Fix-me: The next assertion sometimes fails.  Could it be
          ;; that this loop is continuing even after a change in the
          ;; buffer? How do I stop that? When?:
          (assert (or (= here start) (= here chunk-min)) nil
                  "h=%s, s=%s, cm=%s-%s, e=%s, major=%s"
                  here start chunk-min chunk-max end major)
          (assert (not (eq prev-major major)))
          (when prev-chunk
            (assert (= (overlay-end prev-chunk) (overlay-start chunk))))
          ;; This avoids a problem in HTML chunks after things like
          ;; style="..." where the region to fontify with html-mode
          ;; starts with the char ".
          ;; FIX-ME: Is this needed any more?:
          (when (and nil ;;(not (eq major main-major))
                 (= here (overlay-start chunk))
                 (not (eq ?\" (char-after here))))
            (setq narrow-to-chunk t))
          ;; Take care of single ':s in text.
          ;; FIX-ME: Maybe for more main-major modes?
          (and (not (eq major main-major))
               (memq main-major '(html-mode nxhtml-mode nxml-mode sgml-mode))
               (setq narrow-to-chunk t))
          (unless (and (eq major main-major)
                       all-main)
            (if (not (eq ?\" (char-after here)))
                (mumamo-fontify-region-with here max verbose major narrow-to-chunk)
              (mumamo-fontify-region-with here here verbose major narrow-to-chunk)
              (mumamo-fontify-region-with (1+ here) max verbose major narrow-to-chunk)))
          (setq prev-major major)
          (setq prev-chunk chunk)
          (setq here max)
          (setq fontified-t (get-text-property here 'fontified))
          ;;(redisplay t) (sleep-for 0) (lwarn 't :warning "sleeping") (redisplay t)
          ))
      ;;(unless fontified-t (lwarn 't :warning "fontified-t false"))
      ;;(when fontified-t (lwarn 't :warning "fontified-t true"))
      (goto-char old-point)
      ;;(lwarn 't :warning "done") (redisplay t)
      (unless fontified-t
        ;; Fix-me: I am not sure what to do here. Probably just
        ;; refontify the rest between start and end. But does not this
        ;; lead to unnecessary refontification?
        (mumamo-mark-for-refontification here end))
      ;; Check if more should be refontified due to major mode
      ;; changes. Compare with old overlays.
      (let ((ovl-start (min start (overlay-start first-new-ovl)))
            (ovl-end   (max end   (overlay-end   last-new-ovl)))
            (first-new-major (overlay-get first-new-ovl 'mumamo-major))
            (last-new-major  (overlay-get last-new-ovl  'mumamo-major)))
        ;;(lwarn 't :warning "p=%s ovl-start=%s start=%s end=%s ovl-end=%s" (point) ovl-start start end ovl-end)
        (when (< ovl-start start)
          ;; Check all old overlays in this region
          (dolist (old-o mumamo-chunks-to-remove)
            (let ((old-start (overlay-start old-o))
                  (old-end   (overlay-end   old-o))
                  min-refont
                  max-refont)
              ;; The trick here is writing this in a manner so that
              ;; you do not have to use paper and pencil to check it:
              (when (< ovl-start old-end)
                (setq max-refont (min ovl-start old-start)))
              (when (< old-start start)
                (setq min-refont (max start old-end)))
              (and min-refont
                   max-refont
                   (< min-refont max-refont)
                   (not (eq first-new-major (overlay-get old-o 'mumamo-old)))
                   (progn
                     (mumamo-mark-for-refontification min-refont max-refont)
                     )
                ))))
        (when (< end ovl-end)
          ;; Check all old overlays in this region
          (dolist (old-o mumamo-chunks-to-remove)
            (let ((old-start (overlay-start old-o))
                  (old-end   (overlay-end   old-o))
                  min-refont
                  max-refont)
              (when (< end old-end)
                (setq max-refont (min ovl-end old-end)))
              (when (< old-start ovl-end)
                (setq min-refont (max end old-start)))
              (and min-refont
                   max-refont
                   (< min-refont max-refont)
                   (not (eq last-new-major (overlay-get old-o 'mumamo-old)))
                   (progn
                     ;;(lwarn 't :warning " min-refont=%s max-refont=%s" min-refont max-refont)
                     (mumamo-mark-for-refontification min-refont max-refont)
                     )
                ))))
        )
      (mumamo-remove-old-overlays)
      )))

(defun mumamo-remove-old-overlays ()
  (while mumamo-chunks-to-remove
    (let ((ovl (car mumamo-chunks-to-remove)))
      (setq mumamo-chunks-to-remove (cdr mumamo-chunks-to-remove))
      (overlay-put ovl 'mumamo-old nil)
      (delete-overlay ovl)
      (setq mumamo-chunk-ovls (cons ovl mumamo-chunk-ovls)))))

(defun mumamo-fetch-major-mode-setup(major)
  "Fetch local variable values of major mode for use in `mumamo-mode'."
  ;; FIX-ME: Maybe just use `font-lock-defaults' + "(elisp) Other Font
  ;; Lock Variables" + "(elisp) Syntactic Font Lock"?
  ;;
  (with-temp-buffer
    (mumamo-msgfntfy "mumamo-fetch-major-mode-setup %s" major)
    (let ((mumamo-fetching-major t))
      (funcall major))
    ;;(when (eq major 'html-mode) (setq sgml-xml-mode t))
    (make-local-variable 'font-lock-mode)
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (list
     (list 'major (list 'quote major))
     (list 'major-syntax-table (list 'set-syntax-table (syntax-table)))
     (list 'font-lock-comment-face (list 'quote font-lock-comment-face))
     (list 'font-lock-comment-delimiter-face (list 'quote font-lock-comment-delimiter-face))
     (list 'font-lock-string-face (list 'quote font-lock-string-face))
     (list 'font-lock-doc-face (list 'quote font-lock-doc-face))
     (list 'font-lock-keyword-face (list 'quote font-lock-keyword-face))
     (list 'font-lock-builtin-face (list 'quote font-lock-builtin-face))
     (list 'font-lock-function-name-face (list 'quote font-lock-function-name-face))
     (list 'font-lock-variable-name-face (list 'quote font-lock-variable-name-face))
     (list 'font-lock-type-face (list 'quote font-lock-type-face))
     (list 'font-lock-constant-face (list 'quote font-lock-constant-face))
     (list 'font-lock-warning-face (list 'quote font-lock-warning-face))
     (list 'font-lock-negation-char-face (list 'quote font-lock-negation-char-face))
     (list 'font-lock-preprocessor-face (list 'quote font-lock-preprocessor-face))
     (list 'font-lock-reference-face (list 'quote font-lock-reference-face))

     (list 'font-lock-keywords (list 'quote font-lock-keywords))
     (list 'font-lock-keywords-alist (list 'quote 'font-lock-keywords-alist))
     (list 'font-lock-removed-keywords-alist (list 'quote 'font-lock-removed-keywords-alist))
     (list 'font-lock-keywords-only (list 'quote font-lock-keywords-only))
     (list 'font-lock-keywords-case-fold-search (list 'quote font-lock-keywords-case-fold-search))
     ;; FIX-ME: How to handle `font-lock-syntactically-fontified' when switching major modes?
     (list 'font-lock-syntactically-fontified 0) ; Check this
     (list 'font-lock-syntactic-face-function (list 'quote font-lock-syntactic-face-function))
     (list 'font-lock-syntactic-keywords (list 'quote font-lock-syntactic-keywords))
     (list 'font-lock-syntax-table (list 'quote font-lock-syntax-table))
     (list 'font-lock-beginning-of-syntax-function (list 'quote font-lock-beginning-of-syntax-function))
     (list 'font-lock-mark-block-function (list 'quote font-lock-mark-block-function))
     (list 'font-lock-fontify-buffer-function (list 'quote font-lock-fontify-buffer-function))
     (list 'font-lock-unfontify-buffer-function (list 'quote font-lock-unfontify-buffer-function))
     (list 'font-lock-fontify-region-function (list 'quote font-lock-fontify-region-function))
     (list 'font-lock-unfontify-region-function (list 'quote font-lock-unfontify-region-function)) ;; FIX-ME
     (list 'font-lock-multiline (list 'quote font-lock-multiline))
     ;; FIX-ME: How to handle `font-lock-fontified' when switching major modes?
     (list 'font-lock-fontified nil) ; Whether we have fontified the buffer.
     ;; FIX-ME: How to handle `font-lock-extend-after-change-region-function'?
     ;;(list 'font-lock-extend-after-change-region-function 'font-lock-extend-after-change-region-function)
     (list 'font-lock-dont-widen (list 'quote font-lock-dont-widen))
     ;;(list 'font-lock-beg) (defvar font-lock-end)
     (list 'font-lock-extend-region-functions (list 'quote font-lock-extend-region-functions))
     (list 'font-lock-extra-managed-props (list 'quote font-lock-extra-managed-props))
     ;;(list 'jit-lock-start) (defvar jit-lock-end)
     (list 'font-lock-comment-start-skip (list 'quote font-lock-comment-start-skip))
     (list 'font-lock-comment-end-skip (list 'quote font-lock-comment-end-skip))
     (list 'font-lock-set-defaults t) ; whether we have set up defaults.
     (list 'font-lock-mode-major-mode (list 'quote font-lock-mode-major-mode))

     (list 'indent-line-function (list 'quote indent-line-function))
     (list 'forward-sexp-function (list 'quote forward-sexp-function))
     )))

;; (assq 'nxml-mode mumamo-major-mode-substitute)
(defun mumamo-get-major-mode-setup(major)
  "Get some local variable values to use in regions of mode major.
These are used for indentation and fontification."
  (let* (;(use-major (let ((m (assq major mumamo-major-mode-substitute))) (if m (nth 1 m) major)))
         (use-major (mumamo-get-major-mode-fontification major))
         (fontify-info (assq use-major mumamo-internal-major-modes-alist)))
    ;;(unless (eq major use-major) (setq use-major 'php-mode))
    ;;(unless (eq major use-major) (mumamo-msgfntfy "mumamo-get-major-mode-setup.use-major=%s, major=%s" use-major major))
    (setq fontify-info (assq use-major mumamo-internal-major-modes-alist))
    (unless fontify-info
      (add-to-list 'mumamo-internal-major-modes-alist
                   (list use-major (mumamo-fetch-major-mode-setup use-major)))
      (setq fontify-info (assq major mumamo-internal-major-modes-alist)))
    (setq fontify-info (cadr fontify-info))
    ;;(mumamo-msgfntfy "  not fontify-info=%s" (not fontify-info))
    fontify-info))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fontification of chunks

(defun mumamo-add-help-tabs()
  (local-set-key [tab] 'forward-button)
  (local-set-key [(meta tab)] 'backward-button)
  (local-set-key [(shift tab)] 'backward-button))

(defun mumamo-insert-buffer-button(buffer)
  (let ((func `(lambda(btn)
                 (switch-to-buffer-other-window ',buffer))))
    (mumamo-add-help-tabs)
    (insert-text-button
     (buffer-name buffer)
     :type 'help-function
     'face 'link
     'action func)))

(defun mumamo-insert-describe-button(symbol type)
  (let ((func `(lambda(btn)
                 (funcall ',type ',symbol))))
    (mumamo-add-help-tabs)
    (insert-text-button
     (symbol-name symbol)
     :type 'help-function
     'face 'link
     'action func)))

(defun mumamo-select-chunk-family()
  "Select chunk family for current buffer.
First call `mumamo-select-chunk-family-1' to try decide the chunk
family automatically.

If no automatic choice could be made then ask the user to choose
from `mumamo-chunk-family-list'.

Finally call `mumamo-set-chunk-family' to set the chunk family.

This function is called when turning on `mumamo-mode' if the
chunk family for the current buffer has not been set already."
  (mumamo-msgfntfy "mumamo-select-chunk-family %s, mumamo-mode=%s" (current-buffer) mumamo-mode)
  (let ((chunk-family-name (mumamo-select-chunk-family-1 major-mode)))
    (if chunk-family-name
        (mumamo-set-chunk-family chunk-family-name)
      (call-interactively 'mumamo-set-chunk-family chunk-family-name))))

(defvar mumamo-chunk-not-found-warned nil)

(defun mumamo-select-chunk-family-1(major)
  "Return a chunk family name for current buffer.
First the buffer file name is tried against
`mumamo-filenames-list'.

Then the major mode MAJOR is matched first against
`mumamo-chunk-family-by-mode' and then
`mumamo-chunk-family-list'.

See also `mumamo-set-chunk-family' for how to override this."
  (let ((chunk-family-name
         (catch 'family
           ;; First file name
           (when (buffer-file-name)
             (dolist (key-regexp mumamo-filenames-list)
               (when (string-match (car key-regexp) (buffer-file-name))
                 (throw 'family (nth 1 key-regexp)))))
           ;; Then by mode
           (dolist (elt mumamo-chunk-family-by-mode)
             (when (eq major (car elt))
               (throw 'family (cadr elt))))
           ;; Finally use the chunk family list
           (dolist (elt mumamo-chunk-family-list)
             (when (eq major (cadr elt))
               (throw 'family (car elt)))))))
    (when chunk-family-name
      (unless (assoc chunk-family-name mumamo-chunk-family-list)
        (unless mumamo-chunk-not-found-warned
          (setq mumamo-chunk-not-found-warned t)
          (lwarn '(mumamo) :error "Chunk family not found.")
          (let ((buf (current-buffer)))
            (with-current-buffer "*Warnings*"
              (insert "\tChunk family \"" chunk-family-name "\" from ")
              (mumamo-insert-describe-button 'mumamo-chunk-family-by-mode 'describe-variable)
              (insert "\n\tfor buffer ")
              (mumamo-insert-buffer-button buf)
              (insert " was not found in ")
              (mumamo-insert-describe-button 'mumamo-chunk-family-list 'describe-variable)
              (insert ".\n"))))
        (setq chunk-family-name nil)))
    chunk-family-name))

(defun mumamo-get-major-mode-indentation(major)
  (mumamo-get-major-mode-substitute major 'indentation))
(defun mumamo-get-major-mode-fontification(major)
  (mumamo-get-major-mode-substitute major 'fontification))
(defun mumamo-get-major-mode-substitute(major what)
  (let ((m (assq major mumamo-major-mode-substitute)))
    (if (not m)
        major
      (setq m (nth 1 m) major)
      (cond
       ((eq what 'fontification)
        (nth 0 m))
       ((eq what 'indentation)
        (nth 1 m))
       (t
        (mumamo-display-error 'mumamo-get-major-mode-substitute
                              "Bad parameter, what=%s" what))))))

;;(mumamo-get-major-mode-indentation 'nxhtml-mode)
;;(mumamo-get-major-mode-fontification 'nxhtml-mode)
;;(mumamo-get-major-mode-fontification 'css-mode)
(defconst mumamo-major-mode-substitute
  '(
    (nxhtml-mode (html-mode html-mode))
    (nxml-mode (sgml-mode))
    )
  "Major modes substitute to use for fontification and indentation.")

;; (customize-option 'mumamo-major-modes)
(defcustom mumamo-major-modes
  '(
    (xml-pi-php
     php-mode)
    (inlined-css-mode
     css-mode)
    (jsp-java-mode
     java-mode)
    (eruby-mode
     ruby-mode)
    (inlined-javascript-mode
     javascript-mode
     ecmascript-mode)
    )
  "Alist for conversion of major mode specifier to major mode.
Each entry has the form

  \(MAJOR-SPEC MAJORMODE ...)

where MAJOR-SPEC specifies the code type and should match the
value returned from `mumamo-chunk'. The MAJORMODEs are major
modes that can be used for editing that code type. The first
available MAJORMODE is the one that is used."
  :type '(alist
          :key-type symbol
          :value-type (repeat (choice function symbol))
          )
  :group 'mumamo
  )

(defconst mumamo-warned-once nil)
(make-variable-buffer-local 'mumamo-warned-once)
(put 'mumamo-warned-once 'permanent-local t)

; (append '(0 1) '(a b))
(defun mumamo-warn-once(type message &rest args)
  (let ((msgrec (append (list type message) args)))
    (unless (member msgrec mumamo-warned-once)
      (setq mumamo-warned-once
            (cons msgrec mumamo-warned-once))
      (apply 'lwarn type :warning message args))))

(defun mumamo-mode-from-modespec(major-spec)
  "Translate MAJOR-SPEC to a major mode.
See `mumamo-major-modes'."
  (let ((modes (cdr (assq major-spec mumamo-major-modes)))
        (mode 'fundamental-mode))
    (setq mode
          (catch 'mode
            (dolist (m modes)
              (when (functionp m)
                (let ((def (symbol-function m)))
                  (when (and (listp def)
                             (eq 'autoload (car def)))
                    (condition-case err
                        (load (nth 1 def))
                      (error (setq m nil)))))
                (throw 'mode m)))
            nil))
    (unless mode
      (if (functionp major-spec)
          ;; As a last resort allow spec to be a major mode too:
          (setq mode major-spec)
        (if modes
            (mumamo-warn-once '(mumamo-mode-from-modespec)
                              "\n  Couldn't find an available major mode for specification %s,\n  alternatives are:\n    %s"
                              major-spec modes)
          (lwarn '(mumamo-mode-from-modespec)
                 :error
                 "\n  Couldn't find an available major mode for specification %s" major-spec))
        (setq mode 'fundamental-mode)))
    (mumamo-msgfntfy " mumamo-mode-from-modespec %s => %s" major-spec mode)
    mode))
;(mumamo-mode-from-modespec 'eruby-mode)

(defcustom mumamo-chunk-family-by-mode
  '(
    (html-helper-mode "nXhtml Family")
    (php-mode "nXhtml Family")
    (nxhtml-mode "nXhtml Family")
    (ruby-mode "eRuby nXhtml Family")
    )
  "Chunk family to use when turning on `mumamo-mode'.
This overrides the search done in `mumamo-chunk-family-list' when
chunk family is selected automatically.

Each element in this list should have the form

  \(MAJOR-MODE chunk-family-NAME)

MAJOR-MODE is the major mode in effect when turning on
`mumamo-mode'. chunk-family-NAME should be one of the keys in
`mumamo-chunk-family-list'. (This is case sensitive.)"
  :type '(repeat (list function string))
  :group 'mumamo)

;; FIX-ME: Add some description to show during `mumamo-set-chunk-family'?
;; FIX-ME: Let second arg be a list to simplify?
(defconst mumamo-chunk-family-list
  '(
    ("nXhtml Family" nxhtml-mode
     (mumamo-chunk-xml-pi
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      ))
    ("HTML Family" html-mode
     (mumamo-chunk-xml-pi
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      ))
    ("eRuby Family" nil
     (mumamo-chunk-eruby
      ))
    ("eRuby nXhtml Family" nxhtml-mode
     (mumamo-chunk-eruby
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      ))
    ("eRuby Html Family" html-mode
     (mumamo-chunk-eruby
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      ))
    ("JSP nXhtml Family" nxhtml-mode
     (mumamo-chunk-jsp
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      ))
    ("JSP HTML Family" html-mode
     (mumamo-chunk-jsp
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      ))
    ("nXml Family" nxml-mode
     (mumamo-chunk-xml-pi
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      ))
    ("SGML Family" sgml-mode
     (mumamo-chunk-xml-pi
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      ))
    ("Perl Here Doc" perl-mode
     (mumamo-chunk-perl-here-html
      ))
    ("CSound orc/sco Modes" sgml-mode
     (mumamo-chunk-csound-sco
      mumamo-chunk-csound-orc
      ))
    ("OpenLaszlo Family" nxml-mode
     (mumamo-chunk-inlined-script
      mumamo-chunk-inlined-lzx-method
      mumamo-chunk-inlined-lzx-handler
     ))
;;     ;; Test by using just one submode:
;;     ("test script"
;;      html-mode
;;      (
;;       mumamo-chunk-inlined-script
;;       ))
;;     ("test style"
;;      html-mode
;;      (
;;       mumamo-chunk-inlined-style
;;       ))
;;     ("test style="
;;      html-mode
;;      (
;;       mumamo-chunk-style=
;;       ))
;;     ("test onjs="
;;      html-mode
;;      (
;;       mumamo-chunk-onjs=
;;       ))
    )
  "Alist with chunk definitions.
Each entry in this list is a \"chunk family\". When `mumamo-mode'
is on in a buffer one of this entries is used for that
buffer. That entry decides how the buffer is divided into chunks
and what major mode is chunk has.

Selection of the entry from this list to use is done by
`mumamo-select-chunk-family' or `mumamo-set-chunk-family'.

The dividing into chunks is done by `mumamo-get-chunk-at'.


The entries in this list has the format

  \(CHUNKS-DEF-NAME MAJOR-MODE SUBMODE-CHUNK-FUNCTIONS)

CHUNKS-DEF-NAME is the key name by which the entry is recognized.
MAJOR-MODE is the main major mode used by this entry.
SUBMODE-CHUNK-FUNCTIONS is a list of the functions that does the
chunk division of the buffer.

If you want to write new functions for chunk divisions then
please see `mumamo-chunk'.  You can perhaps also use
`mumamo-easy-make-chunk-fun' or `mumamo-chunk-attr=' which are
more easy-to-use alternatives.

When you write those new functions you may want to use some of
the functions for testing chunks:

 `mumamo-test-create-chunk-at'  `mumamo-test-create-chunks-at-all'
 `mumamo-test-easy-make'        `mumamo-test-fontify-region'

These are in the file mumamo-test.el.")

(defvar mumamo-current-chunk-family nil
  "The currently used chunk family.
See `mumamo-set-chunk-family' for more information.")
(make-variable-buffer-local 'mumamo-current-chunk-family)
(put 'mumamo-current-chunk-family 'permanent-local t)

(defvar mumamo-main-major-mode nil)
(make-variable-buffer-local 'mumamo-main-major-mode)
(put 'mumamo-main-major-mode 'permanent-local t)

(defun mumamo-main-major-mode()
  (let ((main (cadr mumamo-current-chunk-family)))
    (if main
        main
      mumamo-main-major-mode)))

(defun mumamo-unset-chunk-family()
  "Set chunk family to nil, ie undecided."
  (interactive)
  (setq mumamo-current-chunk-family nil))

(defun mumamo-describe-chunk-family(name)
  (let ((family (assoc name mumamo-chunk-family-list)))
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer (help-buffer)
        (help-setup-xref (list #'mumamo-describe-chunk-family name) (interactive-p))
        (let ((inhibit-read-only t)
              (point (point))
              s)
          ;;(insert (format "%S\n\n" family))
          (insert (format "%s - a MuMaMo chunk family" name))
          (insert "

A ")

          (mumamo-insert-describe-button 'mumamo-mode 'describe-function)
          (insert " chunk family is specifications for how to divide a
buffer into chunks for multiple major modes. The chunk familly is
normally selected automatically, see ")
          (mumamo-insert-describe-button 'mumamo-set-chunk-family 'describe-function)
          (insert ".\n\n")
          (insert "Main major mode for "
                  name
                  ": ")
          (let ((mm (nth 1 family)))
            (if mm
                (mumamo-insert-describe-button mm 'describe-function)
              (insert "Major mode in buffer without mumamo-mode.")))
          (insert "\nFunctions for dividing into submodes:\n")
          (dolist (divider (nth 2 family))
            (insert "  ")
            (mumamo-insert-describe-button divider 'describe-function)
            (insert "\n")
            (insert "    ")
            (let ((doc (documentation divider t)))
              (if (not doc)
                  (insert "(Not documented)")
                (insert (substring doc 0 (string-match "\n" doc)))))
            (insert "\n")
            )
          (print-help-return-message))))))

(defun mumamo-set-chunk-family(chunk-family-name)
  "Set chunk family for current buffer.
Set the chunk family to CHUNK-FAMILY-NAME.  When called
interactively ask the user for the chunk family name.  If
`mumamo-current-chunk-family' is set then use this as the
default. Otherwise call `mumamo-select-chunk-family' to try to
get a default.

This function is called when turning on `mumamo-mode' unless
`mumamo-current-chunk-family' is set.  You can call this function
yourself before or after turning on `mumamo-mode' to override the
automatic setting of chunk family."
  (interactive
   (list
    (let* ((prompt "Set chunk family: ")
          (minibuffer-local-must-match-map (copy-keymap minibuffer-local-must-match-map))
          (hist (mapcar (lambda(elt)
                          (car elt))
                        mumamo-chunk-family-list))
          (desc-fun (lambda()
                      (let ((s (minibuffer-contents-no-properties)))
                        (when (< 0 (length s))
                          (mumamo-describe-chunk-family s)))))
          (up-fun (lambda() (interactive)
                    (previous-history-element 1)
                    (funcall desc-fun)))
          (down-fun (lambda() (interactive)
                      (next-history-element 1)
                      (funcall desc-fun)))
          (default (if mumamo-current-chunk-family
                       (car mumamo-current-chunk-family)
                     (let ((d (mumamo-select-chunk-family-1 major-mode)))
                       (if d d
                         (car (car mumamo-chunk-family-list))))))
          (histpos (if (not default)
                       1
                     (catch 'pos
                       (let ((n 0))
                       (dolist (elt mumamo-chunk-family-list)
                         (setq n (1+ n))
                         (when (string= default (car elt))
                           (throw 'pos n)))))))
          (default-name (if default
                            default
                          (car (nth (1- histpos) mumamo-chunk-family-list))))
          )
      (define-key minibuffer-local-must-match-map [up] up-fun)
      (define-key minibuffer-local-must-match-map [down] down-fun)
      (save-window-excursion
        (mumamo-describe-chunk-family default-name)
        (completing-read prompt
                         mumamo-chunk-family-list
                         nil ; predicate
                         t   ; require-match
                         default-name ;; initial-input
                         (cons 'hist histpos) ;; hist
                         )))))
  (when (= 0 (length chunk-family-name))
    (error "No chunk family given"))
  (let ((chunk-family (assoc chunk-family-name mumamo-chunk-family-list))
        (old-chunk-family mumamo-current-chunk-family))
    (unless chunk-family
      (error "There is no chunk family named %s" chunk-family-name))
    (setq mumamo-current-chunk-family chunk-family)
    (when old-chunk-family (mumamo-refresh))
    (require 'rngalt nil t)
    (when (featurep 'rngalt)
      (when rngalt-validation-header
        (rngalt-update-validation-header-overlay)))))

(defun mumamo-refresh()
  "Restart `mumamo-mode' if already active."
  (interactive)
  ;;(mumamo-msgfntfy "mumamo-refresh")
  (when mumamo-mode
      (mumamo-mode 0)
      (mumamo-mode 1)))



(defvar mumamo-chunk-ovls nil
  "Internal. Unused overlays.")

(defvar mumamo-chunks-to-remove nil
  "Internal. Unused overlays.")

(defun mumamo-get-chunk-overlays-in (min max)
  (mumamo-msgfntfy "mumamo-remove-chunk-overlays %s %s" min max)
  (let ((ovls nil))
    (dolist (o (overlays-in min max))
      (when (overlay-get o 'mumamo-major-mode)
        (setq ovls (cons o ovls))))
    ovls))

(defun mumamo-remove-chunk-overlays (min max)
  "Remove chunk overlays and mark for refontification."
  (mumamo-msgfntfy "mumamo-remove-chunk-overlays %s %s" min max)
  (let ((min-min min)
        (max-max max))
    (dolist (o (overlays-in min max))
      (when (overlay-get o 'mumamo-major-mode)
        (when (< max (overlay-end o))
          (setq max-max (overlay-end o)))
        (when (> min (overlay-start o))
          (setq min-min (overlay-start o)))
        ;; Save the old major mode so that we can compare with it:
        (overlay-put o 'mumamo-old (overlay-get o 'mumamo-major-mode))
        (overlay-put o 'mumamo-major-mode nil)
        (setq mumamo-chunks-to-remove (cons o mumamo-chunks-to-remove))))
    (cons min-min max-max)))

(defun mumamo-mark-for-refontification(min max)
  (mumamo-msgfntfy "mumamo-mark-for-refontification %s %s" min max)
  (mumamo-save-buffer-state nil (put-text-property min max 'fontified nil)))

(defun mumamo-remove-all-chunk-overlays ()
  (save-restriction
    (widen)
    (mumamo-remove-chunk-overlays (point-min) (point-max))
    (mumamo-remove-old-overlays)))

(defun mumamo-extend-region-like-jit-lock(min max len)
  "Extend the region the same way jit-lock does it."
  (let ((jit-lock-start min)
        (jit-lock-end   max))
    (font-lock-extend-jit-lock-region-after-change min max len)
    (setq min jit-lock-start)
    (setq max jit-lock-end))
  (cons min max))

(defun mumamo-after-change (min max len)
  (mumamo-msgfntfy "mumamo-after-change %s %s %s" min max len)
  ;; FIX-ME: I do not really know how much to remove here.  This has
  ;; to be coordinated with the removal of chunk overlays somehow, but
  ;; how? -- Probably this must be done when removing the overlays. If
  ;; the new and old overlays differ then some parts should be
  ;; refontified. Maybe. If they have not been that already.  Hm. I
  ;; think this might be the best place to take chunks into
  ;; account. Not sure yet though.
  ;;
  ;; This should ideally check
  ;; `font-lock-extend-after-change-region-function' and
  ;; `font-lock-extend-region-wholelines' for min and max points.
  (let* ((r (mumamo-extend-region-like-jit-lock min max len))
         (new-min (1- (car r)))
         (new-max (1+ (cdr r)))
         chunks-min-max
         chunks-min
         chunks-max
         )
    (setq new-min (max new-min (point-min)))
    (setq new-max (min new-max (point-max)))
    ;; The current chunks could frame what should be refontified:
    (setq chunks-min-max (mumamo-remove-chunk-overlays new-min new-max))
    (setq chunks-min (car chunks-min-max))
    (setq chunks-max (cdr chunks-min-max))
    (when (< chunks-max new-max) (setq new-max chunks-max))
    (when (> chunks-min new-min) (setq new-min chunks-min))
    (mumamo-mark-for-refontification new-min new-max)
    ))

(defun mumamo-create-chunk-at(pos)
  (assert (not (mumamo-get-existing-chunk-at pos)))
  (mumamo-msgfntfy "mumamo-create-chunk-at %s" pos)
  ;; FIX-ME: Maybe remove the narrow support here? This seems to me to
  ;; be overriden now by the narrowing done by
  ;; `mumamo-fontify-region-1'.
  (let* ((chunk-info (cdr mumamo-current-chunk-family))
         (major-normal (mumamo-main-major-mode))
         (chunk-fns    (cadr chunk-info))
         r
         min
         max
         major-sub
         major-narrow
         )
    (dolist (fn chunk-fns)
      (setq r (funcall fn pos (point-min) (point-max)))
      (let ((rmin   (nth 0 r))
            (rmax   (nth 1 r))
            (is-exc (nth 2 r))
            (nrrow (nth 3 r)))
        (unless rmin (setq rmin (point-min)))
        (unless rmax (setq rmax (point-max)))
        ;; comparision have to be done differently if we are in an
        ;; exception part or not. since we are doing this from top to
        ;; bottom the rules are:
        ;;
        ;; - exception parts always outrules non-exception part.  when
        ;;   in exception part the min start point should be used.
        ;; - when in non-exception part the max start point and the
        ;;   min end point should be used.
        ;;
        ;; check if first run:
        (if (not min)
            (progn
              (setq min rmin)
              (setq max rmax)
              (setq major-sub is-exc)
              (setq major-narrow nrrow))
          (if is-exc
              (if major-sub
                  (when (or (not min)
                            (< rmin min))
                    (setq min rmin)
                    (setq max rmax)
                    (setq major-sub is-exc))
                (setq min rmin)
                (setq max rmax)
                (setq major-sub is-exc)
                (setq major-narrow nrrow))
            (unless major-sub
              (when (< min rmin) (setq min rmin))
              (when (< rmax max) (setq max rmax))
              )))))
    ;; check!
    (assert (<= min pos)) (assert (<= pos max))
    ;; remove all old chunk overlays between min and max
    ;; Fix-me: Must keep track of those to know how much to refontify:
    (mumamo-remove-chunk-overlays min max)
    (if mumamo-chunk-ovls
        (progn
          (setq chunk-ovl (car mumamo-chunk-ovls))
          (setq mumamo-chunk-ovls (cdr mumamo-chunk-ovls))
          (overlay-put chunk-ovl 'face nil)
          (overlay-put chunk-ovl 'mumamo-old nil)
          (overlay-put chunk-ovl 'mumamo-major-mode nil))
      (setq chunk-ovl (make-overlay (point-min) (point-min)))
      ;; Fix-me: Why should these overlays have a priority?
      ;;(overlay-put chunk-ovl 'priority 10)
      )
    (move-overlay chunk-ovl min max)
    (overlay-put chunk-ovl 'mumamo-narrow major-narrow)
    (when (and (= min (point-min))
               (= max (point-max)))
      ;;(setq major-sub mumamo-main-major-mode)
      (setq major-sub nil)
      )
    (if major-sub
        (let ((major-sub-to-use (mumamo-mode-from-modespec major-sub)))
          (overlay-put chunk-ovl 'mumamo-major-mode major-sub-to-use)
          (overlay-put chunk-ovl
                       'face
                       (when (memq mumamo-chunk-coloring '(submode-colored both-colored))
                         mumamo-background-chunk-submode)))
      (overlay-put chunk-ovl 'mumamo-major-mode major-normal)
      (overlay-put chunk-ovl
                   'face
                   (when (memq mumamo-chunk-coloring '(both-colored))
                     mumamo-background-chunk-major)))
    (unless (and (<= (overlay-start chunk-ovl) pos)
                 (<= pos (overlay-end chunk-ovl)))
      (mumamo-display-error 'mumamo-create-chunk-at "start=%s, pos=%s, end=%s"
             (overlay-start chunk-ovl) pos (overlay-end chunk-ovl)))
    (mumamo-msgfntfy "mumamo-create-chunk-at %s, chunk-ovl=%s, major=%s" pos chunk-ovl (overlay-get chunk-ovl 'mumamo-major-mode))
    chunk-ovl))

;; FIX-ME: probably usable colors only for light background now
(defface mumamo-background-chunk-submode
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "gray34")
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "azure")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "azure")
    (((class color) (min-colors 8))
     :background "blue")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Background colors for chunks in major mode.
You should only specify :background here, otherwise it will
interfere with syntax highlighting.

FIX-ME: background colors for dark cases."
  :group 'mumamo)

(defcustom mumamo-background-chunk-major 'mumamo-background-chunk-major
  "Background colors for chunks in major mode.
Pointer to face with background color.

If you do not want any special background color use the face named
default."
  :type 'face
  :group 'mumamo)

(defface mumamo-background-chunk-major
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "gray34")
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "cornsilk")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "cornsilk")
    (((class color) (min-colors 8))
     :background "blue")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Background colors for chunks in sub modes.
You should only specify :background here, otherwise it will
interfere with syntax highlighting.

FIX-ME: background colors for dark cases."
  :group 'mumamo)

(defcustom mumamo-background-chunk-submode 'mumamo-background-chunk-submode
  "Background colors for chunks in sub modes.
Pointer to face with background color.

If you do not want any special background color use the face named
default."
  :type 'face
  :group 'mumamo)

(defcustom mumamo-chunk-coloring 'both-colored
  "What chunks to color."
  :type '(choice (const :tag "Color only submode chunks" submode-colored)
                 (const :tag "No coloring of chunks" no-chunks-colored)
                 (const :tag "Color both submode and main major mode chunks" both-colored))
  :group 'mumamo)

(defun mumamo-get-existing-chunk-at(pos)
  (let ((chunk-ovl))
    (when (= pos (point-max))
      (setq pos (1- pos)))
    (dolist (o (overlays-at pos))
      (unless chunk-ovl
        (when (overlay-get o 'mumamo-major-mode)
          (setq chunk-ovl o))))
    chunk-ovl))

(defun mumamo-get-chunk-at(pos)
  "Return chunk overlay.
Create it if it does not exist. Do creation just from chunk at
point as returned by `mumamo-chunk-xml-pi'."
  (let ((chunk-ovl (mumamo-get-existing-chunk-at pos)))
    (if chunk-ovl
        ;;(mumamo-msgfntfy "existing %s %s" pos chunk-ovl)
        (unless (and (<= (overlay-start chunk-ovl) pos)
                     (<= pos (overlay-end chunk-ovl)))
          (error "mumamo-get-chunk-at: start=%s, pos=%s, end=%s"
                   (overlay-start chunk-ovl) pos (overlay-end chunk-ovl)))
      (setq chunk-ovl (mumamo-create-chunk-at pos)))
    chunk-ovl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chunk routines

(defun mumamo-forward-chunk()
  (interactive)
  (let* ((chunk (mumamo-get-chunk-at (point)))
         (end-pos (overlay-end chunk)))
    (goto-char (min end-pos
                    (point-max)))))

(defun mumamo-backward-chunk()
  (interactive)
  (let* ((chunk (mumamo-get-chunk-at (point)))
         (start-pos (overlay-start chunk)))
    (goto-char (max (1- start-pos)
                    (point-min)))))


;;; jsp

(defun mumamo-chunk-jsp(pos min max)
  "Find <% ... %>."
  (mumamo-chunk pos min max
                    'mumamo-search-bw-exc-start-jsp
                    'mumamo-search-bw-exc-end-jsp
                    'mumamo-search-fw-exc-start-jsp
                    'mumamo-search-fw-exc-end-jsp))

(defun mumamo-search-bw-exc-start-jsp(pos min)
  (let ((exc-start (mumamo-search-bw-exc-start-str pos min "<%")))
    (when (and exc-start
               (<= exc-start pos))
      (cons exc-start 'jsp-java-mode))))

(defun mumamo-search-bw-exc-end-jsp(pos min)
  (mumamo-search-bw-exc-end-str pos min "%>"))

(defun mumamo-search-fw-exc-start-jsp(pos max)
  (let ((end-out (mumamo-search-fw-exc-start-str pos max "<%")))
    end-out))

(defun mumamo-search-fw-exc-end-jsp(pos max)
  (mumamo-search-fw-exc-end-str pos max "%>"))

;;; eruby

(defun mumamo-chunk-eruby(pos min max)
  "Find <% ... %>."
  (mumamo-chunk pos min max
                    'mumamo-search-bw-exc-start-ruby
                    'mumamo-search-bw-exc-end-jsp
                    'mumamo-search-fw-exc-start-jsp
                    'mumamo-search-fw-exc-end-jsp))

(defun mumamo-search-bw-exc-start-ruby(pos min)
  (let ((exc-start (mumamo-search-bw-exc-start-str pos min "<%")))
    (when (and exc-start
               (<= exc-start pos))
      (cons exc-start 'eruby-mode))))


;;; <script ...>

(defconst mumamo-script-tag-start-regex
  (rx "<script"
      space
      (0+ (not (any ">")))
      "type"
      (0+ space)
      "="
      (0+ space)
      ?\"
      ;;(or "text" "application")
      ;;"/"
      ;;(or "javascript" "ecmascript")
      "text/javascript"
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-search-bw-exc-start-inlined-script(pos min)
  (goto-char (+ pos 7))
  (let ((marker-start (search-backward "<script" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-script-tag-start-regex)
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) 'inlined-javascript-mode))
        ))))
(defun mumamo-search-bw-exc-end-inlined-script(pos min)
  (mumamo-search-bw-exc-end-str pos min "</script>"))
(defun mumamo-search-fw-exc-start-inlined-script(pos max)
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<script" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-script-tag-start-regex)
        (goto-char (match-end 0))
        (point)
        ))))
(defun mumamo-search-fw-exc-end-inlined-script(pos max)
  (mumamo-search-fw-exc-end-str pos max "</script>"))

(defun mumamo-chunk-inlined-script(pos min max)
  "Find inlined script, <script>...</script>."
  (mumamo-chunk pos min max
                    'mumamo-search-bw-exc-start-inlined-script
                    'mumamo-search-bw-exc-end-inlined-script
                    'mumamo-search-fw-exc-start-inlined-script
                    'mumamo-search-fw-exc-end-inlined-script))


;;; OpenLaszlo

(defconst mumamo-lzx-method-tag-start-regex
  (rx "<method"
      (optional
       space
       (0+ (not (any ">"))))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-search-bw-exc-start-inlined-lzx-method(pos min)
  (goto-char (+ pos 7))
  (let ((marker-start (search-backward "<method" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-lzx-method-tag-start-regex)
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) 'inlined-javascript-mode))
        ))))
(defun mumamo-search-bw-exc-end-inlined-lzx-method(pos min)
  (mumamo-search-bw-exc-end-str pos min "</method>"))
(defun mumamo-search-fw-exc-start-inlined-lzx-method(pos max)
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<method" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-lzx-method-tag-start-regex)
        (goto-char (match-end 0))
        (point)
        ))))
(defun mumamo-search-fw-exc-end-inlined-lzx-method(pos max)
  (mumamo-search-fw-exc-end-str pos max "</method>"))

(defun mumamo-chunk-inlined-lzx-method(pos min max)
  "Find inlined script, <method>...</method>."
  (mumamo-chunk pos min max
                    'mumamo-search-bw-exc-start-inlined-lzx-method
                    'mumamo-search-bw-exc-end-inlined-lzx-method
                    'mumamo-search-fw-exc-start-inlined-lzx-method
                    'mumamo-search-fw-exc-end-inlined-lzx-method))

(defconst mumamo-lzx-handler-tag-start-regex
  (rx "<handler"
      (optional
       space
       (0+ (not (any ">"))))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-search-bw-exc-start-inlined-lzx-handler(pos min)
  (goto-char (+ pos 8))
  (let ((marker-start (search-backward "<handler" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-lzx-handler-tag-start-regex)
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) 'inlined-javascript-mode))
        ))))
(defun mumamo-search-bw-exc-end-inlined-lzx-handler(pos min)
  (mumamo-search-bw-exc-end-str pos min "</handler>"))
(defun mumamo-search-fw-exc-start-inlined-lzx-handler(pos max)
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<handler" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 8))
      (when (looking-at mumamo-lzx-handler-tag-start-regex)
        (goto-char (match-end 0))
        (point)
        ))))
(defun mumamo-search-fw-exc-end-inlined-lzx-handler(pos max)
  (mumamo-search-fw-exc-end-str pos max "</handler>"))

(defun mumamo-chunk-inlined-lzx-handler(pos min max)
  "Find inlined script, <handler>...</handler>."
  (mumamo-chunk pos min max
                    'mumamo-search-bw-exc-start-inlined-lzx-handler
                    'mumamo-search-bw-exc-end-inlined-lzx-handler
                    'mumamo-search-fw-exc-start-inlined-lzx-handler
                    'mumamo-search-fw-exc-end-inlined-lzx-handler))


;;; csound

;; FIX-ME: make a macro out of this?

(defun mumamo-search-bw-exc-start-csound-orc (pos min)
  (let ((exc-start (mumamo-search-bw-exc-start-str pos min "<csinstruments>")))
    (and exc-start
         (<= exc-start pos)
         (cons exc-start 'csound-orc-mode))))
(defun mumamo-search-bw-exc-end-csound-orc(pos min)
  (mumamo-search-bw-exc-end-str pos min "</csinstruments>"))
(defun mumamo-search-fw-exc-start-csound-orc(pos max)
  (mumamo-search-fw-exc-start-str pos max "<csinstruments>"))
(defun mumamo-search-fw-exc-end-csound-orc(pos max)
  (mumamo-search-fw-exc-end-str pos max "</csinstruments>"))
(defun mumamo-chunk-csound-orc(pos min max)
  "Find <csinstruments>...</csinstruments>."
  (mumamo-chunk pos min max
                    'mumamo-search-bw-exc-start-csound-orc
                    'mumamo-search-bw-exc-end-csound-orc
                    'mumamo-search-fw-exc-start-csound-orc
                    'mumamo-search-fw-exc-end-csound-orc))

(defun mumamo-search-bw-exc-start-csound-sco (pos min)
  (let ((exc-start (mumamo-search-bw-exc-start-str pos min "<csscore>")))
    (and exc-start
         (<= exc-start pos)
         (cons exc-start 'csound-sco-mode))))
(defun mumamo-search-bw-exc-end-csound-sco(pos min)
  (mumamo-search-bw-exc-end-str pos min "</csscore>"))
(defun mumamo-search-fw-exc-start-csound-sco(pos max)
  (mumamo-search-fw-exc-start-str pos max "<csscore>"))
(defun mumamo-search-fw-exc-end-csound-sco(pos max)
  (mumamo-search-fw-exc-end-str pos max "</csscore>"))
(defun mumamo-chunk-csound-sco(pos min max)
  "Found <csscore>...</csscore>."
  (mumamo-chunk pos min max
                    'mumamo-search-bw-exc-start-csound-sco
                    'mumamo-search-bw-exc-end-csound-sco
                    'mumamo-search-fw-exc-start-csound-sco
                    'mumamo-search-fw-exc-end-csound-sco))

;;; <style ...>

(defconst mumamo-style-tag-start-regex
  (rx "<style"
      space
      (0+ (not (any ">")))
      "type"
      (0+ space)
      "="
      (0+ space)
      ?\"
      "text/css"
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[")
      ))

(defun mumamo-search-bw-exc-start-inlined-style(pos min)
  (goto-char (+ pos 6))
  (let ((marker-start (search-backward "<style" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-style-tag-start-regex)
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) 'inlined-css-mode))
        ))))
(defun mumamo-search-bw-exc-end-inlined-style(pos min)
  (mumamo-search-bw-exc-end-str pos min "</style>"))
(defun mumamo-search-fw-exc-start-inlined-style(pos max)
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<style" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 6))
      (when (looking-at mumamo-style-tag-start-regex)
        (goto-char (match-end 0))
        (point)
        ))))
(defun mumamo-search-fw-exc-end-inlined-style(pos max)
  (mumamo-search-fw-exc-end-str pos max "</style>"))

(defun mumamo-chunk-inlined-style(pos min max)
  "Find inlined style, <style>...</style>."
  (mumamo-chunk pos min max
                    'mumamo-search-bw-exc-start-inlined-style
                    'mumamo-search-bw-exc-end-inlined-style
                    'mumamo-search-fw-exc-start-inlined-style
                    'mumamo-search-fw-exc-end-inlined-style))


;;; on[a-z]+=\"javascript:"

(defconst mumamo-onjs=start-regex
  (rx "<"
      (0+ (not (any ">")))
      space
      "on"
      (1+ (any "a-za-z"))
      "="
      (0+ space)
      ?\"
      (submatch
       "javascript:"
       (0+
        (not (any ">\""))))
      ))
(defun mumamo-chunk-onjs=(pos min max)
  "Find javascript attributes, on...=\"...\"."
  (mumamo-chunk-attr= pos min max "on[a-z]+=" t mumamo-onjs=start-regex
                      'inlined-javascript-mode))

;;; style=

(defconst mumamo-style=start-regex
  (rx "<"
      (0+ (not (any ">")))
      space
      "style="
      (0+ space)
      ?\"
      (submatch
       (0+
        (not (any ">\""))))
      ))
(defun mumamo-chunk-style=(pos min max)
  "Find style attributes, style=\"...\"."
  (mumamo-chunk-attr= pos min max "style=" nil mumamo-style=start-regex
                      'inlined-css-mode))

(defun mumamo-chunk-attr=(pos min max attr= attr=is-regex attr-regex submode)
  "This should work similar to `mumamo-chunk'.
See `mumamo-chunk-style=' for an example of use."
  ;; 1- for the case that pos is at the " before the attribute value.
  (condition-case err
      (progn
        (goto-char (1- pos))
        (let ((prev-attr= (if attr=is-regex
                              (re-search-backward attr= min t)
                            (search-backward attr= min t)))
              next-attr
              start
              end
              exc-mode
              exc-start-prev
              exc-end-prev
              exc-start-next
              exc-end-next
              )
          ;; find prev change and if inside style= the next change
          (when (and prev-attr=
                     (search-backward "<" min t))
            (when (looking-at attr-regex)
              (setq exc-start-prev (match-beginning 1))
              (setq exc-end-prev   (match-end 1))
              (when (<= exc-start-prev pos)
                (if (>= pos exc-end-prev)
                    (setq start exc-end-prev)
                  (setq exc-mode submode)
                  (setq start exc-start-prev)
                  (setq end exc-end-prev)))))
          ;; find next change
          (unless end
            (if start
                (goto-char start)
              (goto-char pos)
              (search-backward "<" min t))
            (setq next-attr= (if attr=is-regex
                                 (re-search-forward attr= max t)
                               (search-forward attr= max t)))
            (when (and next-attr=
                       (search-backward "<" min t))
              (when (looking-at attr-regex)
                (setq end (match-beginning 1)))))
          (when start (assert (<= start pos) t "s=%s p=%s" start pos))
          (when end   (assert (<= pos end) t "p=%s e=%s" pos end))
          (goto-char pos)
          (list start end exc-mode t nil pos)))
    (error
     (mumamo-display-error 'mumamo-chunk-attr= "%s"
                           (error-message-string err)))))



;;; xml pi

(defun mumamo-search-bw-exc-start-xml-pi(pos min)
  (let ((exc-start (mumamo-search-bw-exc-start-str pos min "<?"))
        spec
        exc-mode
        hit)
    (when exc-start
      (goto-char exc-start)
      (when (and (not (looking-at "xml"))
                 (looking-at (rx (0+ (any "a-z")))))
        (setq exc-start (match-end 0))
        (setq spec (match-string-no-properties 0))
        (setq exc-mode (assoc spec mumamo-xml-pi-mode-alist))
        (when exc-mode (setq exc-mode (cdr exc-mode)))
        (setq hit t)
        )
      (when hit
        (unless exc-mode
          (setq exc-mode 'fundamental-mode))
        (when (<= exc-start pos)
          (cons exc-start exc-mode))))))

(defun mumamo-search-bw-exc-start-str(pos min marker)
  (assert (stringp marker))
  (let (start-in)
    (goto-char pos)
    (setq start-in (search-backward marker min t))
    (when start-in
      ;; do not include the marker
      (setq start-in (+ start-in (length marker))))
    start-in))

(defun mumamo-search-bw-exc-end-xml-pi(pos min)
  (mumamo-search-bw-exc-end-str pos min "?>"))

(defun mumamo-search-bw-exc-end-str(pos min marker)
  (assert (stringp marker))
  (goto-char (+ pos (length marker)))
  (search-backward marker min t))

(defun mumamo-search-fw-exc-end-xml-pi(pos max)
  (mumamo-search-fw-exc-end-str pos max "?>"))

(defun mumamo-search-fw-exc-end-str(pos max marker)
  (assert (stringp marker))
  (goto-char (1+ pos)) ;; 1+ cause otherwise ?> is at point
  (let (end-in)
    (setq end-in (search-forward marker max t))
    (when end-in
      ;; do not include the marker
      (setq end-in (- end-in (length marker))))
    end-in))

(defun mumamo-search-fw-exc-start-xml-pi(pos max)
  (goto-char pos)
  (skip-chars-backward "a-zA-Z")
  (let ((end-out (mumamo-search-fw-exc-start-str (point) max "<?")))
    (when (looking-at "xml")
      (setq end-out nil))
    (when end-out
      ;; Get end-out:
      (when (looking-at (rx (0+ (any "a-z"))))
        (setq end-out (match-end 0))))
    end-out))

(defun mumamo-search-fw-exc-start-str(pos max marker)
  (assert (stringp marker))
  (goto-char (- pos (length marker)))
  (search-forward marker max t))

(defvar mumamo-xml-pi-mode-alist
  '(("php" . xml-pi-php))
  "Alist used by `mumamo-chunk-xml-pi' to get exception mode."
  )

(defun mumamo-chunk-xml-pi(pos min max)
  "Find process instruction chunks, <? ... ?>."
  (mumamo-chunk pos min max
                    'mumamo-search-bw-exc-start-xml-pi
                    'mumamo-search-bw-exc-end-xml-pi
                    'mumamo-search-fw-exc-start-xml-pi
                    'mumamo-search-fw-exc-end-xml-pi))


;;; Easy extensions:

(defun mumamo-easy-bw-exc-start(pos min)
  (let ((exc-start (mumamo-search-bw-exc-start-str pos min mumamo-easy-start-static))
        exc-mode)
    (when exc-start
      (goto-char (match-beginning 0))
      (when (looking-at mumamo-easy-start-regex)
        (setq exc-start (match-end 0))
        (setq exc-mode (intern-soft (match-string 1)))
        (when (<= exc-start pos)
          (goto-char exc-start) ;; for testing
          (cons exc-start exc-mode))))))
(defun mumamo-easy-bw-exc-end(pos min)
   (mumamo-search-bw-exc-end-str pos min mumamo-easy-end-static))
(defun mumamo-easy-fw-exc-start(pos min)
   (let ((exc-start (mumamo-search-fw-exc-start-str pos min mumamo-easy-start-static)))
     (when exc-start
       (goto-char (match-beginning 0))
       (when (looking-at mumamo-easy-start-regex)
         (setq exc-start (match-end 0))
         (when (>= exc-start pos)
           (goto-char exc-start) ;; for testing
           exc-start)))))
(defun mumamo-easy-fw-exc-end(pos min)
   (mumamo-search-fw-exc-end-str pos min mumamo-easy-end-static))

(defmacro mumamo-easy-make-chunk-fun(chunk-fun-symbol
                                     start-static start-regex
                                     end-static)
  "Create a new chunk function.
This is supposed to be an easy way to create new chunk functions.
"
  (declare (indent 1) (debug t))
  `(defun ,chunk-fun-symbol(pos min max)
     (let ((mumamo-easy-start-static ,(symbol-value start-static))
           (mumamo-easy-start-regex ,(symbol-value start-regex))
           (mumamo-easy-end-static ,(symbol-value end-static)))
       (mumamo-chunk pos min max
                     'mumamo-easy-bw-exc-start
                     'mumamo-easy-bw-exc-end
                     'mumamo-easy-fw-exc-start
                     'mumamo-easy-fw-exc-end))))

;;; perl here doc
(defun mumamo-chunk-perl-here-html(pos min max)
  "Find perl here docs, EXPERIMENTAL.
See `mumamo-perl-here-doc-modes' for how to customize the choice
of major mode in the perl here document."
  (mumamo-msgfntfy "perl-here start")
  (let ((r (mumamo-chunk-perl-here-doc pos min max)))
    (mumamo-msgfntfy "perl-here stop")
    r)
  )

(defcustom mumamo-perl-here-doc-modes
  '(
    ("_HTML_" html-mode)
    )
  "Matches for perl here doc modes.
The entries in this list have the form

  (REGEXP MAJOR-MODE)

where REGEXP is a regular expression that should match the perl
here document marker and MAJOR-MODE is the major mode to use in
the perl here document.

The entries are tried from the beginning until the first match."
  :type '(repeat
          (list
           regexp 
           (function :tag "Major mode")))
  :group 'mumamo)

(defun mumamo-perl-here-doc-mode(marker)
  (catch 'mode
    (dolist (rec mumamo-perl-here-doc-modes)
      (let ((regexp (nth 0 rec))
            (mode   (nth 1 rec)))
        (when (string-match regexp marker)
          (throw 'mode mode))))))

(defun mumamo-chunk-perl-here-doc(pos min max)
  "This should work similar to `mumamo-chunk'."
  (condition-case err
      (let ((old-point (point)))
        (goto-char pos)
        ;; Adjust for beeing inside an <<...;
        ;;(beginning-of-line)
        ;;(when (looking-at (rx (1+ (not (any "<"))) "<<" (submatch (1+ (not (any "^;")))) ";" line-end)
        (end-of-line)
        (beginning-of-line)
        (let ((prev-<< (search-backward "<<" min t))
              next-<<
              here-doc-end-mark-end
              here-doc-end-mark-start
              here-doc-start-mark-start
              here-doc-start-mark-end
              here-doc-mark
              start
              end
              exc-mode
              )
          ;; find prev change and end of that perl here doc
          (when prev-<<
            (when (looking-at (concat "<<[[:space:]]*\\([^;]*\\);"))
              (setq here-doc-start-mark-start (match-beginning 0))
              (setq here-doc-start-mark-end   (match-end 0))
              (setq here-doc-mark  (buffer-substring-no-properties
                                    (match-beginning 1)
                                    (match-end 1)))
              (end-of-line)
              (setq here-doc-end-mark-end (search-forward here-doc-mark max t))
              (when (and here-doc-end-mark-end
                         (eolp))
                (beginning-of-line)
                (if (looking-at here-doc-mark)
                    (setq here-doc-end-mark-start (point))
                  (setq here-doc-end-mark-end nil)))
              (if (and here-doc-end-mark-start
                       (<= here-doc-end-mark-start pos))
                  (progn
                    (setq start here-doc-end-mark-start)
                    )
                (setq exc-mode (mumamo-perl-here-doc-mode here-doc-mark))
                (setq start (1+ here-doc-start-mark-end))
                (when here-doc-end-mark-start
                  (setq end here-doc-end-mark-start))
                )))
          (unless end
            (goto-char pos)
            (beginning-of-line)
            (setq next-<< (search-forward "<<" max t))
            (when next-<<
              (when (looking-at (concat "[[:space:]]*\\([^;]*\\);"))
                (setq end (1+ (match-end 0))))))
          (when start (assert (<= start pos) t "s=%s p=%s" start pos))
          (when end   (assert (<= pos end) t "p=%s e=%s" pos end))
          (goto-char old-point)
          (list start end exc-mode t nil pos)))
    (error
     (mumamo-display-error 'mumamo-chunk-perl-here-doc "%s"
                           (error-message-string err)))))


;;; The main generic chunk routine
(defun mumamo-chunk(pos
                    min max
                    bw-exc-start-fun
                    bw-exc-end-fun
                    fw-exc-start-fun
                    fw-exc-end-fun)
  "Return list describing a chunk that includes POS.
No notice is taken about existing chunks and no chunks are
created. The description returned is for the smallest possible
chunk which is delimited by the function parameters.

POS must be between MIN and MAX.

The function BW-EXC-START-FUN takes two parameters, POS and
MIN. It should search backward from POS, bound by MIN, for
exception start and return a cons \(found-pos . exception-mode).

The functions BW-EXC-END-FUN, FW-EXC-START-FUN and FW-EXC-END-FUN
should search for exception start or end, forward resp
backward. Those three should return just the position found.

For all four functions the position returned should be nil if
search fails.

Return as a list with values

  \(START END EXCEPTION-MODE NARROW)

The bounds START and END are where the exception starts or stop.
Either of them may be nil, in which case this is equivalent to
`point-min' respectively `point-max'.

If EXCEPTION-MODE is non-nil that is the submode for this
range. Otherwise the main major mode should be used for this
chunk."
  (condition-case err
      (progn
        (assert (and (<= min pos) (<= pos max))
                t
                "mumamo-chunk: min=%s, pos=%s, max=%s, bt=%S" min pos max (with-output-to-string (backtrace)))
        (let (
              start-in-cons
              exc-mode
              start-in start-out
              end-in end-out
              start end
              end-of-exception
              wants-end-type
              )
          ;; ;; find start of range
          ;;
          ;; start normal
          (setq start-out (funcall bw-exc-end-fun pos min))
          (when start-out
            (assert (<= start-out pos))
            (assert (<= min start-out)))
          (when start-out (setq min start-out)) ;; minimize next search bw
          ;; start exception
          (setq start-in-cons (funcall bw-exc-start-fun pos min))
          (setq start-in (car start-in-cons))
          (when start-in
            (assert (<= start-in pos))
            (assert (<= min start-in)))
          ;; compare
          (cond
           ((and start-in start-out)
            (if (< start-in start-out)
                (setq start start-out)
              (setq exc-mode (cdr start-in-cons))
              (setq start start-in)))
           (start-in
            (setq exc-mode (cdr start-in-cons))
            (setq start start-in))
           (start-out
            (setq start start-out))
           )
          ;; ; find end of range
          ;;
          ;; what end type is acceptable?  three possible values: nil means
          ;; any end type, the other values are 'end-normal and
          ;; 'end-exception.
          (when start
            (if exc-mode
                (setq wants-end-type 'end-exception)
              (setq wants-end-type 'end-normal)))
          ;; end exception
          (when (or (not wants-end-type)
                    (eq wants-end-type 'end-exception))
            (setq max end-in) ;; minimize next search fw
            (setq end-in (funcall fw-exc-end-fun pos max)))
          ;; end normal
          (when (or (not wants-end-type)
                    (eq wants-end-type 'end-normal))
            (setq end-out (funcall fw-exc-start-fun pos max)))
          ;; compare
          (cond
           ((and end-in end-out)
            (if (> end-in end-out)
                (setq end end-out)
              (setq end-of-exception t)
              (setq end end-in)))
           (end-in
            (setq end-of-exception t)
            (setq end end-in))
           (end-out
            (setq end end-out))
           )
          ;; check
          (when start (assert (<= start pos)))
          (when end   (assert (<= pos end)))
          (goto-char pos)
          (list start end exc-mode nil end-of-exception pos)))
    (error
     (mumamo-display-error 'mumamo-chunk "%s"
                           (error-message-string err)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The minor mode


;;; Main
(defvar mumamo-unread-command-events-timer nil)
(make-variable-buffer-local 'mumamo-unread-command-events-timer)

(defun mumamo-unread-command-events(command-keys new-major old-last-command)
  (condition-case err
      (progn
        ;; last-command seems to be cleared by top-level:
        (unless last-command
          (setq last-command old-last-command))
        (when (< 0 (length command-keys))
          (setq unread-command-events (append command-keys nil)))
        (message "Switched to %s" new-major))
    (error (lwarn '(mumamo-unread-command-events)
                  :error "mumamo-unread-command-events, err=%s" err))))

(defvar mumamo-idle-set-major-mode-timer nil)
(make-variable-buffer-local 'mumamo-idle-set-major-mode-timer)
(put 'mumamo-idle-set-major-mode-timer 'permanent-local t)

(defun mumamo-idle-set-major-mode(chunk)
  ;;(mumamo-msgfntfy "mumamo-idle-set-major-mode c=%s" chunk)
  (condition-case err
      (let* ((ovl (mumamo-get-chunk-at (point)))
             (major (overlay-get ovl 'mumamo-major-mode)))
        (when (eq ovl chunk)
          (unless (eq major major-mode)
            (mumamo-set-major major)
            (when (eq ovl chunk)
              ;; sync keymap
              (let ((this-command-keys (this-command-keys)))
                (when (timerp mumamo-unread-command-events-timer)
                  (cancel-timer mumamo-unread-command-events-timer))
                (setq mumamo-unread-command-events-timer
                      (run-with-idle-timer 0 nil
                                           'mumamo-unread-command-events this-command-keys major last-command))
                (top-level)
                )))))
    (error (lwarn '(mumamo-idle-set-major) :error "mumamo-idle-set-major-mode, err=%s" err))))

(defun mumamo-request-idle-set-major-mode()
  (when (timerp mumamo-idle-set-major-mode-timer)
    (cancel-timer mumamo-idle-set-major-mode-timer))
  (setq mumamo-idle-set-major-mode-timer
        (run-with-idle-timer
         mumamo-set-major-mode-delay
         nil
         'mumamo-idle-set-major-mode (mumamo-get-chunk-at (point)))))

(defvar mumamo-done-first-set-major nil)
(make-variable-buffer-local 'mumamo-done-first-set-major)
(put 'mumamo-done-first-set-major 'permanent-local t)

;;(setq mumamo-set-major-mode-delay -1)
(defcustom mumamo-set-major-mode-delay idle-update-delay
  "Delay this number of seconds before setting major mode.
When point enters a region where the major mode should be
different than the current major mode, wait until emacs has been
idle this number of seconds before switching major mode.

If negative switch major mode immediately.

Ideally the switching of major mode should occur immediately when
entering a region. However this can make movements a bit unsmooth
for some major modes on a slow computer. Therefore on a slow
computer use a short delay.

If you have a fast computer and want to use mode specific
movement commands then set this variable to -1.

I tried to measure the time for switching major mode in
mumamo. For most major modes it took 0 ms, but for nxml-mode and
its derivate it took 20 ms on a 3GHz CPU."
  :type 'number
  :group 'mumamo)

(defun mumamo-set-major-from-overlay()
  (let* ((ovl (mumamo-get-chunk-at (point)))
         (major (overlay-get ovl 'mumamo-major-mode)))
    (unless major
      (lwarn '(mumamo-set-major-from-overlay)
             :error
             "major=%s" major))
    (unless (and mumamo-done-first-set-major
                 (eq major-mode major))
      (if mumamo-done-first-set-major
          (if (<= 0 mumamo-set-major-mode-delay)
              (progn
                (mumamo-request-idle-set-major-mode))
            (mumamo-set-major major)
            (message "Switched to %s" major-mode))
        (mumamo-set-major major)))))

(defun mumamo-post-command()
  (when mumamo-mode
    (condition-case err
        (mumamo-set-major-from-overlay)
      (error
       (lwarn 'mumamo-post-command :warning "%s"
                             (error-message-string err))))))

(defvar mumamo-just-changed-major nil
  "Avoid refontification when switching major mode.
Set to t by `mumamo-set-major'. Checked and reset to nil by
`mumamo-jit-lock-function'.")
(make-variable-buffer-local 'mumamo-just-changed-major)

(defun mumamo-jit-lock-function(start)
  (mumamo-msgfntfy "mumamo-jit-lock-function %s, ff=%s, just-changed=%s" start (get-text-property start 'fontified) mumamo-just-changed-major)
  (if mumamo-just-changed-major
      (setq mumamo-just-changed-major nil)
    (jit-lock-function start)))

(defvar mumamo-set-major-running nil
  "Internal use. Handling of `mumamo-mode' turn off.")

(defvar mumamo-changed-major-num 0)
(make-variable-buffer-local 'mumamo-changed-major-num)
(put 'mumamo-changed-major-num 'permanent-local t)

(defun mumamo-change-major-function()
  (unless (or mumamo-set-major-running
	      (and (boundp 'mumamo-global-mode-checking)
                   mumamo-global-mode-checking)
	      (and (boundp 'mumamo-from-global)
                   mumamo-from-global)
              )
    (setq mumamo-changed-major-num (1+ mumamo-changed-major-num))
    (when (< 1 mumamo-changed-major-num)
      (setq mumamo-explicitly-turned-on-off t)
      (mumamo-turn-off-actions1))))

(defun mumamo-derived-from-mode(major from-mode)
  (let ((major-mode major))
    (derived-mode-p from-mode)))

(defvar mumamo-indent-line-alist nil)

(defvar mumamo-survive
  '(
    ;;
    ;; Emulation modes
    ;;
    ;; These variables should have 'permanant-local t set in their
    ;; packages IMO, but now they do not have that.

    ;; -*- mode: grep; default-directory: "c:/emacs/p/070323/emacs/lisp/emulation/" -*-
    ;; Grep started at Thu Mar 29 22:37:11

    ;; grep -i -nH -e "make.*local" *.el
    cua-inhibit-cua-keys
    cua--explicit-region-start
    cua--status-string
    ;; This is buffer local, but not defined yet:
    cua--rectangle
    ;;cua--rectangle-overlays
    edt-select-mode
    tpu-newline-and-indent-p
    tpu-newline-and-indent-string
    tpu-saved-delete-func
    tpu-buffer-local-map
    tpu-mark-flag
    vi-add-to-mode-line
    vi-scroll-amount
    vi-shift-width
    vi-ins-point
    vi-ins-length
    vi-ins-repetition
    vi-ins-overwrt-p
    vi-ins-prefix-code
    vi-last-change-command
    vi-last-shell-command
    vi-last-find-char
    vi-mark-alist
    vi-insert-state
    vi-mode-old-local-map
    vi-mode-old-mode-name
    vi-mode-old-major-mode
    vi-mode-old-case-fold
    vip-emacs-local-map
    vip-insert-local-map
    vip-insert-point
    vip-com-point
    vip-current-mode
    vip-emacs-mode-line-buffer-identification
    vip-current-major-mode

    viper-after-change-functions
    viper-before-change-functions
    viper-post-command-hooks
    viper-pre-command-hooks
    ;;minor-mode-map-alist
    ;; viper-mode-string -- is already buffer local, globally void
    viper-d-com
    viper-last-insertion
    viper-command-ring
    ;;(put ',var 'permanent-local save)
    ;;(put 'make-variable-frame-local 'permanent-local save)
    ;;(put 'viper-replace-overlay-cursor-color 'permanent-local save)
    ;;(put 'make-variable-frame-local 'permanent-local save)
    ;;(put 'viper-insert-state-cursor-color 'permanent-local save)
    ;;(put 'make-variable-frame-local 'permanent-local save)
    ;;(put 'viper-emacs-state-cursor-color 'permanent-local save)
    ;;(put 'make-variable-frame-local 'permanent-local save)
    ;;(put 'viper-vi-state-cursor-color 'permanent-local save)
    ;;(put 'bar-cursor 'permanent-local save)
    viper-tut--part
    viper-syntax-preference

    ;;(put 'require-final-newline 'permanent-local save)
    ;; This does not help, at least not with Viper:
    ;;(put 'cursor-type 'permanent-local save)

    ;;Grep finished (matches found) at Thu Mar 29 22:37:11


    ;;-*- mode: grep; default-directory: "c:/emacs/p/070323/emacs/lisp/emulation/" -*-
    ;;Grep started at Fri Mar 30 23:34:00

    ;;grep -i -nH -e "viper-deflocalvar" *.el
    ;;(defmacro viper-deflocalvar (var default-value &optional documentation)
    viper-vi-intercept-minor-mode
    viper-vi-basic-minor-mode
    viper-vi-local-user-minor-mode
    viper-vi-global-user-minor-mode
    viper-vi-state-modifier-minor-mode
    viper-vi-diehard-minor-mode
    viper-vi-kbd-minor-mode
    viper-insert-intercept-minor-mode
    viper-insert-basic-minor-mode
    viper-insert-local-user-minor-mode
    viper-insert-global-user-minor-mode
    viper-insert-state-modifier-minor-mode
    viper-insert-diehard-minor-mode
    viper-insert-kbd-minor-mode
    viper-replace-minor-mode
    viper-emacs-intercept-minor-mode
    viper-emacs-local-user-minor-mode
    viper-emacs-global-user-minor-mode
    viper-emacs-kbd-minor-mode
    viper-emacs-state-modifier-minor-mode
    viper-vi-minibuffer-minor-mode
    viper-insert-minibuffer-minor-mode
    viper-automatic-iso-accents
    viper-special-input-method
    ;; already local: viper-undo-needs-adjustment
    viper-intermediate-command
    viper-began-as-replace
    ;; already local: viper-replace-overlay
    ;; already local: viper-last-posn-in-replace-region
    ;; already local: viper-last-posn-while-in-insert-state
    ;; already local: viper-sitting-in-replace
    viper-replace-chars-to-delete
    viper-replace-region-chars-deleted
    viper-current-state
    viper-cted
    viper-current-indent
    viper-preserve-indent
    viper-auto-indent
    viper-electric-mode
    ;; already local: viper-insert-point
    ;; already local: viper-pre-command-point
    viper-com-point
    viper-ex-style-motion
    viper-ex-style-editing
    viper-ESC-moves-cursor-back
    viper-delete-backwards-in-replace
    ;; already local: viper-related-files-and-buffers-ring
    viper-local-search-start-marker
    viper-search-overlay
    viper-last-jump
    viper-last-jump-ignore
    viper-minibuffer-current-face
    ;; already local: viper-minibuffer-overlay
    ;;viper-init.el:1045: ;; eval: (put 'viper-deflocalvar 'lisp-indent-hook 'defun)
    ;; already local: viper-vi-local-user-map
    ;; already local: viper-insert-local-user-map
    ;; already local: viper-emacs-local-user-map
    viper--key-maps
    viper--intercept-key-maps
    ;; already local: viper-need-new-vi-local-map
    ;; already local: viper-need-new-insert-local-map
    ;; already local: viper-need-new-emacs-local-map
    viper-mouse-click-search-noerror
    viper-mouse-click-search-limit
    ;;viper-mous.el:674: ;;; eval: (put 'viper-deflocalvar 'lisp-indent-hook 'defun)
    viper-non-word-characters
    viper-ALPHA-char-class
    ;;viper-util.el:1580: ;;; eval: (put 'viper-deflocalvar 'lisp-indent-hook 'defun)
    ;;viper.el:1365: ;; eval: (put 'viper-deflocalvar 'lisp-indent-hook 'defun)

    ;;Grep finished (matches found) at Fri Mar 30 23:34:01

    ;;
    ;; nxhtml etc:
    nxhtml-minor-mode
    rng-validate-up-to-date-end
    ;;
    ;; mlinks:
    mlinks-mode
    mlinks-hilighter-timer
    mlinks-mark-links-timer
    mlinks-link-update-pos-min
    mlinks-link-update-pos-max
    ;;
    ;;change-major-mode-hook
    ;;after-change-major-mode-hook
    )
  "Local variables to survive the change of major mode.")

(defvar mumamo-survive-a-bit-strange
  '(
    cua--rectangle
    viper-after-change-functions
    viper-before-change-functions
    viper-post-command-hooks
    viper-pre-command-hooks
    viper-command-ring
    viper-last-insertion
    viper-d-com
    mlinks-mode
    )
  "Some `mumamo-survive' variables have a bit strange presense.
Those are listed here. They are maybe not always local and when
they are they may already have 'permanent-local t.")

(defvar mumamo-mode-major-mode nil)
(make-variable-buffer-local 'mumamo-mode-major-mode)
(put 'mumamo-mode-major-mode 'permanent-local t)

;; FIX-ME: Clean up the different ways of surviving variables during
;; change of major mode.
(defun mumamo-set-major(major)
  (mumamo-msgfntfy "mumamo-set-major %s, %s" major (current-buffer))
  (let ((start-time (get-internal-run-time))
        end-time
        used-time
        ;; Tell `mumamo-change-major-function':
        (mumamo-set-major-running major)
        (old-font-lock-mode font-lock-mode)
        (font-lock-mode font-lock-mode)
        ;; Save cursor type:
        (old-cursor-type cursor-type)
        ;; Protect last-command:
        (old-last-command last-command)
        )
    (when (mumamo-derived-from-mode major 'nxml-mode)
      (require 'nxml-mode)
      (make-local-variable 'nxml-syntax-highlight-flag)
      (setq nxml-syntax-highlight-flag nil)
      ;;(assert (not nxml-syntax-highlight-flag) "first")
      (when nxml-syntax-highlight-flag
        (lwarn 'mumamo-set-major :warning "nxml-syntax-highlight-flag is not nil"))
      ;; To avoid removing 'fontified flag:
      (put 'nxml-syntax-highlight-flag 'permanent-local t)
      (put 'nxml-ns-state 'permanent-local t)
      ;;(put 'rng-open-elements 'permanent-local t)
      ;;(put 'rng-pending-contents 'permanent-local t)
      ;;(put 'rng-collecting-text 'permanent-local t)
      ;;(put 'rng-current-schema-file-name 'permanent-local t)
      (put 'rng-error-count 'permanent-local t)
      ;;(put 'rng-validate-up-to-date-end 'permanent-local t)
      )

    ;; We are not changing mode from font-lock's point of view, so
    ;; do not tell font-lock:
    (remove-hook 'change-major-mode-hook 'font-lock-change-mode t)
    (remove-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh)

    ;; We are not changing mode from hs-minor-mode's point of view:
    (remove-hook 'change-major-mode-hook 'turn-off-hideshow t)

    (dolist (sym (reverse mumamo-survive))
      (when (boundp sym)
        (unless (or (local-variable-if-set-p sym)
                    (memq sym mumamo-survive-a-bit-strange))
          (delq sym mumamo-survive)
          (lwarn 'mumamo-survive :warning "Not a local variable: %s" sym))
        (when (and (get sym 'permanent-local)
                   (not (memq sym mumamo-survive-a-bit-strange)))
          (delq sym mumamo-survive)
          (lwarn 'mumamo-survive :warning "Already 'permanent-local t: %s" sym))))
    (dolist (sym mumamo-survive)
      (put sym 'permanent-local t))

    (run-hooks 'mumamo-change-major-mode-hook)

    (setq mumamo-mode-major-mode major)

    (funcall major) ;; <-----------------

    (dolist (sym mumamo-survive)
      (when (boundp sym)
        (put sym 'permanent-local nil)))
    (when mlinks-mode
      (add-hook 'after-change-functions 'mlinks-after-change t t))

    (add-to-list 'mumamo-indent-line-alist (list major-mode indent-line-function))

    ;; Some major modes deactivates the mark, we do not want that:
    (setq deactivate-mark nil)

    (setq cursor-type old-cursor-type)
    (unless (eq last-command old-last-command)
      (lwarn 'mumamo-set-major :error "last-command 3=%s, old-last-command" last-command old-last-command)
      (setq last-command old-last-command))
    (run-hooks 'mumamo-after-change-major-mode-hook)

    (when (boundp 'nxml-syntax-highlight-flag)
      (put 'nxml-syntax-highlight-flag 'permanent-local nil)
      (put 'nxml-ns-state 'permanent-local nil)
      ;;(put 'rng-open-elements 'permanent-local t)
      ;;(put 'rng-pending-contents 'permanent-local t)
      ;;(put 'rng-collecting-text 'permanent-local t)
      ;;(put 'rng-current-schema-file-name 'permanent-local t)
      ;;
      ;; FIX-ME: I Do not understand this, but I get rng-error-count
      ;; reset to nil if it is not always permanent-local t.
      ;;
      ;;(put 'rng-error-count 'permanent-local nil)
      ;;
      ;;(put 'rng-validate-up-to-date-end 'permanent-local nil)
      )

    (when global-font-lock-mode
      (add-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh))
    (when old-font-lock-mode
      (add-hook 'change-major-mode-hook 'font-lock-change-mode nil t))

    (setq mumamo-mode t)
    (mumamo-set-fontification-functions)


    (if mumamo-done-first-set-major
        (setq mumamo-just-changed-major t)
      (mumamo-msgfntfy "----- removing 'fontified")
      (mumamo-save-buffer-state nil
        (remove-text-properties (point-min) (point-max) '(fontified)))
      (setq mumamo-done-first-set-major t))

    ;; Timing, on a 3ghz cpu:
    ;;
    ;;   used-time=(0 0 0), major-mode=css-mode
    ;;   used-time=(0 0 0), major-mode=ecmascript-mode
    ;;   used-time=(0 0 0), major-mode=html-mode
    ;;   used-time=(0 0 203000), major-mode=nxhtml-mode
    ;;
    ;; After some changes 2007-04-25:
    ;;
    ;;   used-time=(0 0 15000), major-mode=nxhtml-mode
    ;;
    ;; which is 15 ms. That seems acceptable though I am not sure
    ;; everything is correct when switching to nxhtml-mode yet.  I
    ;; will have to wait for bug reports ;-)
    ;;
    ;; The delay is clearly noticeable and disturbing IMO unless you
    ;; change major mode in an idle timer.
    ;;
    (setq end-time (get-internal-run-time))
    (setq used-time (time-subtract end-time start-time))
    ;;(message ";;   used-time=%s, major-mode=%s" used-time major-mode)
    ;;(lwarn 'switch-time :warning "used-time=%s, major-mode=%s" used-time major-mode)

    ))

(defun mumamo-setup-local-vars()
  ;; Fix-me: add to survive
  ;;
  ;; FIX-ME: Should not these variables always be buffer local? Is not
  ;; that a bug that they are not that?
  (make-local-variable 'font-lock-fontify-region-function)
  (setq font-lock-fontify-region-function 'mumamo-fontify-region)

  ;; FIX-ME: This should really be defined similar to fontify, and
  ;; this should be used when finishing mumamo-mode.
  (make-local-variable 'font-lock-unfontify-region-function)
  (setq font-lock-unfontify-region-function 'mumamo-unfontify-region)

  (if nil
      (font-lock-turn-on-thing-lock)
    (make-local-variable 'font-lock-fontify-buffer-function)
    (setq font-lock-fontify-buffer-function 'mumamo-fontify-buffer)
    (setq font-lock-fontify-buffer-function 'jit-lock-refontify))

  (make-local-variable 'font-lock-unfontify-buffer-function)
  (setq font-lock-unfontify-buffer-function 'mumamo-unfontify-buffer)


  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'mumamo-indent-line-function)

  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'mumamo-indent-region-function)

  ;; FIX-ME: Not sure about this one, but it looks like it must be
  ;; set:
  (make-local-variable 'jit-lock-contextually)
  (setq jit-lock-contextually t)
  )

(defun mumamo-jit-lock-register (fun &optional contextual)
  "Replacement for `jit-lock-register'.
Avoids refontification, otherwise same."
  (add-hook 'jit-lock-functions fun nil t)
  (when (and contextual jit-lock-contextually)
    (set (make-local-variable 'jit-lock-contextually) t))

  ;;(jit-lock-mode t)
  ;;
  ;; Replace this with the code below from jit-lock-mode t part:
  (setq jit-lock-mode t)

  ;; Mark the buffer for refontification.
  ;; This is what we want to avoid in mumamo:
  ;;(jit-lock-refontify)

  ;; Install an idle timer for stealth fontification.
  (when (and jit-lock-stealth-time (null jit-lock-stealth-timer))
    (setq jit-lock-stealth-timer
          (run-with-idle-timer jit-lock-stealth-time t
                               'jit-lock-stealth-fontify)))

  ;; Create, but do not activate, the idle timer for repeated
  ;; stealth fontification.
  (when (and jit-lock-stealth-time (null jit-lock-stealth-repeat-timer))
    (setq jit-lock-stealth-repeat-timer (timer-create))
    (timer-set-function jit-lock-stealth-repeat-timer
                        'jit-lock-stealth-fontify '(t)))

  ;; Init deferred fontification timer.
  (when (and jit-lock-defer-time (null jit-lock-defer-timer))
    (setq jit-lock-defer-timer
          (run-with-idle-timer jit-lock-defer-time t
                               'jit-lock-deferred-fontify)))

  ;; Initialize contextual fontification if requested.
  (when (eq jit-lock-contextually t)
    (unless jit-lock-context-timer
      (setq jit-lock-context-timer
            (run-with-idle-timer jit-lock-context-time t
                                 'jit-lock-context-fontify)))
    (setq jit-lock-context-unfontify-pos
          (or jit-lock-context-unfontify-pos (point-max))))

  ;; Setup our hooks.
  (add-hook 'after-change-functions 'jit-lock-after-change nil t)
  (add-hook 'fontification-functions 'mumamo-jit-lock-function))


(defun mumamo-set-fontification-functions()
  "Let `mumamo-mode' take over fontification."
  ;; Give the jit machinery a starting point:
  (mumamo-jit-lock-register 'font-lock-fontify-region t)
  ;; Set up fontification to call jit:
  (let ((ff (reverse fontification-functions)))
    (mapc (lambda(f)
            ;;(unless (eq f 'jit-lock-function)
            (remove-hook 'fontification-functions f t))
          ;;)
          ff))
  (add-hook 'fontification-functions 'mumamo-jit-lock-function nil t)
  ;; Set the functions that font-lock should use:
  (mumamo-setup-local-vars)
  ;; Need some hook modifications to keep things together too:
  (add-hook 'change-major-mode-hook 'mumamo-change-major-function nil t)
  (add-hook 'after-change-functions 'mumamo-after-change nil t)
  (add-hook 'after-change-functions 'jit-lock-after-change nil t)
  (add-hook 'post-command-hook 'mumamo-post-command nil t)
  (remove-hook 'change-major-mode-hook 'nxml-change-mode t)
  (remove-hook 'change-major-mode-hook 'nxhtml-change-mode t)
  )

(defun mumamo-initialize-state()
  (setq mumamo-done-first-set-major nil)
  (setq mumamo-just-changed-major nil))

(defun mumamo-turn-on-actions()
  (condition-case err
      (progn
        (mumamo-msgfntfy "mumamo-turn-on-actions")
        (unless mumamo-current-chunk-family
          (mumamo-select-chunk-family))
        (if (not mumamo-current-chunk-family)
            (progn
              (setq mumamo-mode nil)
              (lwarn '(mumamo) :warning
                     "Could not turn on mumamo because chunk family was not set\n\tin buffer %s."
                     (current-buffer))
              (with-current-buffer "*Warnings*"
                (insert "\tFor more information see `")
                (mumamo-insert-describe-button 'mumamo-mode 'describe-function)
                (insert "'.\n")))
          ;; Disabled this, there seem to be some bug in with-temp-buffer
          ;;
          ;; Load major mode:
          ;;(let ((main-major-mode (mumamo-main-major-mode)))
          ;;  (with-temp-buffer
          ;;    (funcall main-major-mode)
          ;;    ))
          (mumamo-initialize-state)
          (mumamo-set-fontification-functions)
          (mumamo-save-buffer-state nil
              (remove-list-of-text-properties (point-min) (point-max)
                                              (list 'fontified)))
          ;; For validation header etc:
          (require 'rngalt nil t)
          (when (featurep 'rngalt)
            (setq rngalt-major-mode (mumamo-main-major-mode))
            (rngalt-update-validation-header-overlay)))
        (mumamo-get-chunk-at (point))
        (mumamo-set-major-from-overlay)
        (add-hook 'change-major-mode-hook 'mumamo-change-major-function nil t)
        (setq mumamo-mode-major-mode major-mode))
    (quit (setq mumamo-mode nil))
    (error
     (setq mumamo-mode nil)
     (mumamo-display-error 'mumamo-turn-on-actions "%s"
                           (error-message-string err)))))

(defun mumamo-turn-off-actions()
  (mumamo-turn-off-actions1)
  (when (mumamo-derived-from-mode (nth 1 mumamo-current-chunk-family) 'nxml-mode)
    (when (fboundp 'nxml-change-mode)
      (nxml-change-mode)))
  (fundamental-mode)
  (if mumamo-main-major-mode
      (funcall mumamo-main-major-mode)
    (normal-mode)))

(defun mumamo-unfontify-chunks()
  (save-restriction
    (widen)
    (let ((ovls (overlays-in (point-min) (point-max))))
      (dolist (o ovls)
        (when (overlay-get o 'mumamo-major-mode)
          (mumamo-unfontify-chunk o))))))

(defun mumamo-turn-off-actions1()
  (mumamo-msgfntfy "mumamo-turn-off-actions1")
  (when (and (boundp 'rng-validate-mode)
             rng-validate-mode)
    (rng-validate-mode 0))
  ;; FIX-ME: This may be run from change-major-mode-hook, do we need
  ;; to remove it in a timer? It looks to me like it is needed when I
  ;; look at the code for `run-hooks'.
  (when (boundp 'rngalt-major-mode)
    (setq rngalt-major-mode nil))
  ;;(run-with-idle-timer 0 nil (lambda() (lwarn 't :warning "turn off removing change-maj") (remove-hook 'change-major-mode-hook 'mumamo-change-major-function t)))
  (remove-hook 'change-major-mode-hook 'mumamo-change-major-function t)
  ;;(font-lock-unfontify-buffer)
  (mumamo-unfontify-chunks)
  (remove-hook 'after-change-functions 'mumamo-after-change t)
  (remove-hook 'post-command-hook 'mumamo-post-command t)
  (mumamo-remove-all-chunk-overlays))

(defvar mumamo-mode-on-hook nil
  "Normal hook run after turning on `mumamo-mode'.")
(put 'mumamo-mode-on-hook 'permanent-local t)

(defvar mumamo-mode-off-hook nil
  "Normal hook run after turning off `mumamo-mode'.")
(put 'mumamo-mode-off-hook 'permanent-local t)

(defvar mumamo-change-major-mode-hook nil
  "Normal hook run before internal change of major mode.")
(put 'mumamo-change-major-mode-hook 'permanent-local t)

(defvar mumamo-after-change-major-mode-hook nil
  "Normal hook run after internal change of major mode.")
(put 'mumamo-after-change-major-mode-hook 'permanent-local t)


(defvar mumamo-explicitly-turned-on-off nil)
(make-variable-buffer-local 'mumamo-explicitly-turned-on-off)
(put 'mumamo-explicitly-turned-on-off 'permanent-local t)

(define-minor-mode mumamo-mode
  "Minor mode for maintaining major modes per chunk in a buffer.
When this minor mode is on in a buffer the buffer is divided into
chunk, where each chunk has a different major mode than the
previous chunk.

In each chunk the choosen major mode decide how syntax
highlighting and indentation should be done.

The dividing into chunks is normally done automatically when you
start `mumam-mode'.  For information about the automatic
selection and how to override it see `mumamo-set-chunk-family'."
  :lighter " MuMaMo"
  :group 'mumamo
  ;;(mumamo-msgfntfy "mumamo-mode %s" mumamo-mode)
  (unless (or
           ;; Avoid looping if user add something like (mumamo-mode 1)
           ;; to a major mode hook:
           (boundp 'mumamo-mode--called)
           mumamo-set-major-running
           ;; Avoid calling mumamo-mode when fetching major mode info:
           (boundp 'mumamo-fetching-major))
    (let (mumamo-mode--called)
      (unless (or (and (boundp 'mumamo-global-mode-checking)
                       mumamo-global-mode-checking)
                  (and (boundp 'mumamo-from-global)
                       mumamo-from-global))
        (setq mumamo-explicitly-turned-on-off t))
      (if mumamo-mode
          (progn
            (setq mumamo-main-major-mode major-mode)
            (mumamo-turn-on-actions)
            (run-hooks 'mumamo-mode-on-hook))
        (mumamo-turn-off-actions)
        (run-hooks 'mumamo-mode-off-hook)))))

(defcustom mumamo-filenames-list
  '(
    ("\.php$" "nXhtml Family")
    ("\.lzx$" "OpenLaszlo Family")
    ("\.jsp$" "JSP nXhtml Family")
    )
  "List of regexps for file names for turning on `mumamo-mode'.
If buffer file name matches the an entry in the list then
`mumamo-global-mode' will turn on `mumamo-mode' in buffer.

Each element is used as a regexp to match the file name.

See also `mumamo-chunk-family-by-mode'."
  :type '(repeat
          (list
           (string :tag "File name regexp")
           (string :tag "MuMaMo chunk family")
           ))
  :group 'mumamo)

(defun mumamo-turn-on-for-global()
  (unless (or mumamo-mode
              mumamo-set-major-running
              mumamo-explicitly-turned-on-off
              (boundp 'mumamo-from-global)
              (minibufferp (current-buffer))
              (let ((bn (buffer-name)))
                (when (< 0 (length bn))
                  (string= " " (substring bn 0 1))
                  (string= "*" (substring bn 0 1))))
              ;;(char-to-string (char-after 1))
              (and (buffer-file-name)
                   (string= "php" (file-name-extension (buffer-file-name)))
                   (< 0 (buffer-size))
                   (not (eq ?\< (char-after 1))))
              )
    (mumamo-turn-on-for-global-1)
    ))

(defun mumamo-turn-on-for-global-1()
  (let ((mumamo-from-global t))
    (unless mumamo-current-chunk-family
      (let ((chunk-family-name (mumamo-select-chunk-family-1 major-mode)))
        (if chunk-family-name
            (mumamo-set-chunk-family chunk-family-name))))
    (when mumamo-current-chunk-family
      (mumamo-msgfntfy "mumamo-turn-on-for-global")
      (mumamo-mode 1))))

(defun mumamo-turn-off-for-global()
  (unless (or mumamo-explicitly-turned-on-off
              (not mumamo-mode))
    (mumamo-mode 0)))

(defmacro define-globalized-mumamo-minor-mode (global-mode mode turn-on turn-off &rest keys)
  "Make a global mode GLOBAL-MODE corresponding to buffer-local minor MODE.
This is a special variant of `define-globalized-minor-mode' for
mumamo.  It let bounds the variable GLOBAL-MODE-checking before
calling TURN-ON or TURN-OFF.

TURN-ON is a function that will be called with no args in every buffer
  and that should try to turn MODE on if applicable for that buffer.
TURN-OFF is a function that turns off MODE in a buffer.
KEYS is a list of CL-style keyword arguments.  As the minor mode
  defined by this function is always global, any :global keyword is
  ignored.  Other keywords have the same meaning as in `define-minor-mode',
  which see.  In particular, :group specifies the custom group.
  The most useful keywords are those that are passed on to the
  `defcustom'.  It normally makes no sense to pass the :lighter
  or :keymap keywords to `define-globalized-minor-mode', since these
  are usually passed to the buffer-local version of the minor mode.

If MODE's set-up depends on the major mode in effect when it was
enabled, then disabling and reenabling MODE should make MODE work
correctly with the current major mode.  This is important to
prevent problems with derived modes, that is, major modes that
call another major mode in their body."

  (let* ((global-mode-name (symbol-name global-mode))
	 (pretty-name (easy-mmode-pretty-mode-name mode))
	 (pretty-global-name (easy-mmode-pretty-mode-name global-mode))
	 (group nil)
	 (extra-keywords nil)
	 (MODE-buffers (intern (concat global-mode-name "-buffers")))
	 (MODE-enable-in-buffers
	  (intern (concat global-mode-name "-enable-in-buffers")))
	 (MODE-check-buffers
	  (intern (concat global-mode-name "-check-buffers")))
	 (MODE-cmhh (intern (concat global-mode-name "-cmhh")))
	 (MODE-major-mode (intern (concat (symbol-name mode) "-major-mode")))
         (MODE-checking (intern (concat global-mode-name "-checking")))
	 keyw)

    ;; Check keys.
    (while (keywordp (setq keyw (car keys)))
      (setq keys (cdr keys))
      (case keyw
	(:group (setq group (nconc group (list :group (pop keys)))))
	(:global (setq keys (cdr keys)))
	(t (push keyw extra-keywords) (push (pop keys) extra-keywords))))

    (unless group
      ;; We might as well provide a best-guess default group.
      (setq group
	    `(:group ',(intern (replace-regexp-in-string
				"-mode\\'" "" (symbol-name mode))))))

    `(progn

       ;; Define functions for the global mode first so that it can be
       ;; turned on during load:

       ;; List of buffers left to process.
       (defvar ,MODE-buffers nil)

       ;; The function that calls TURN-ON in each buffer.
       (defun ,MODE-enable-in-buffers ()
         (let ((,MODE-checking nil))
           (dolist (buf ,MODE-buffers)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (if ,mode
                     (unless (eq ,MODE-major-mode major-mode)
                       (setq ,MODE-checking t)
                       (,mode -1)
                       (,turn-on)
                       (setq ,MODE-checking nil)
                       (setq ,MODE-major-mode major-mode))
                   (setq ,MODE-checking t)
                   (,turn-on)
                   (setq ,MODE-checking nil)
                   (setq ,MODE-major-mode major-mode)))))))
       (put ',MODE-enable-in-buffers 'definition-name ',global-mode)

       (defun ,MODE-check-buffers ()
	 (,MODE-enable-in-buffers)
	 (setq ,MODE-buffers nil)
	 (remove-hook 'post-command-hook ',MODE-check-buffers))
       (put ',MODE-check-buffers 'definition-name ',global-mode)

       ;; The function that catches kill-all-local-variables.
       (defun ,MODE-cmhh ()
	 (add-to-list ',MODE-buffers (current-buffer))
	 (add-hook 'post-command-hook ',MODE-check-buffers))
       (put ',MODE-cmhh 'definition-name ',global-mode)


       (defvar ,MODE-major-mode nil)
       (make-variable-buffer-local ',MODE-major-mode)

       ;; The actual global minor-mode
       (define-minor-mode ,global-mode
	 ,(format "Toggle %s in every possible buffer.
With prefix ARG, turn %s on if and only if ARG is positive.
%s is enabled in all buffers where `%s' would do it.
See `%s' for more information on %s."
		  pretty-name pretty-global-name pretty-name turn-on
		  mode pretty-name)
	 :global t ,@group ,@(nreverse extra-keywords)

	 ;; Setup hook to handle future mode changes and new buffers.
	 (if ,global-mode
	     (progn
	       (add-hook 'after-change-major-mode-hook
			 ',MODE-enable-in-buffers)
	       (add-hook 'find-file-hook ',MODE-check-buffers)
	       (add-hook 'change-major-mode-hook ',MODE-cmhh))
	   (remove-hook 'after-change-major-mode-hook ',MODE-enable-in-buffers)
	   (remove-hook 'find-file-hook ',MODE-check-buffers)
	   (remove-hook 'change-major-mode-hook ',MODE-cmhh))

	 ;; Go through existing buffers.
         (let ((,MODE-checking t))
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               ;;(if ,global-mode (,turn-on) (when ,mode (,mode -1)))
               (if ,global-mode (,turn-on) (,turn-off))
               ))))

       )))

(define-globalized-mumamo-minor-mode mumamo-global-mode mumamo-mode
  mumamo-turn-on-for-global
  mumamo-turn-off-for-global
  :group 'mumamo)
;; The problem with global minor modes:
;; (when (and mumamo-global-mode
;;            (not (boundp 'define-global-minor-mode-bug)))
;;   (mumamo-global-mode 1)
;;   )

(provide 'mumamo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo.el ends here
