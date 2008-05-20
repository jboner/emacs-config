;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VoiceCode, a programming-by-voice environment.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;;
;;; (C)2002, National Research Council of Canada
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (push (substitute-in-file-name "$VCODE_HOME/Environments/Emacs") load-path))
(load "wddx.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message packaging layer.
;;;
;;; Uses the WDDX protocol to exchange message description with VoiceCode.
;;;
;;; Emacs counterpart of VoiceCode Python class 
;;; messaging.MessPackager_FixedLenSeq
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar chunk-len 1024)
(defvar large-white-space "")
(let ((ii 0))
  (while (< ii chunk-len) 
    (setq large-white-space (concat large-white-space " "))
    (setq ii (1+ ii))
  )
)

(defun vcode-get-packed-mess (transporter)
   "Gets a complete message from a message transporter (typically a socket 
    connection)

   The message is split in fixed length chunks, each chunk prefixed with 
   a flag that indicates if this is the last chunk in the complete message.
   "

   (let ((last-chunk nil) (chunk-prefix nil) (packed-mess ""))
     (while (not last-chunk)
       (setq a-chunk (vcode-read-from-transporter transpoter chunk-len))
       (setq chunk-prefix (substring a-chunk 0 0))
       (setq packed-mess (concat packed-mess a-chunk))
       (if (string= "1" chunk-prefix) (setq last-chunk t))
     )
   )
)

(defun vcode-unpack-mess (mess)
   "Unpacks a message received as a series of chunks"
   


   (let ((bytes-read 0) (unpacked-mess "") (a-chunk) (chop-len) (chunk-prefix))

     (while (not (string= mess ""))
       ;;;
       ;;; Get a chunk of the message
       ;;;
       (setq a-chunk (substring mess 0 chunk-len))
       (setq mess (substring mess chunk-len))
       ;;;
       ;;; Ignore single character prefix (the one indicating if this is 
       ;;; the last chunk in the message)
       ;;;
       (setq unpacked-mess (concat unpacked-mess (substring a-chunk 1)))
       (setq bytes-read (+ bytes-read chunk-len))
     )
     (setq result (list unpacked-mess bytes-read))
     result
   )
)

(defun vcode-pack-mess (mess)
   "Packs a message into a series of fixed length chunks."

   (let ((packed-mess "") (len-this-chunk (1- chunk-len))
	 (prefix "0") (padding nil))

     ;;;
     ;;; Concatenate chunks, prefixing each chunk with 0 (except for the 
     ;;; last chunk where prefix is 1).
     ;;;
     (while (> (length mess) 0)
        (if (< (length mess) len-this-chunk) 
	    (setq len-this-chunk (length mess)))
	(setq this-chunk (substring mess 0 len-this-chunk))
        (if (= len-this-chunk (1- chunk-len))
	    (setq mess (substring mess len-this-chunk))
	  ;;;
	  ;;; Pad the last chunk
	  ;;;
	  (progn
	     (setq mess ""))
	     (setq padding (substring large-white-space 0 
				      (- chunk-len len-this-chunk 1)))
	     (setq this-chunk (concat this-chunk padding))
	  )
	(if (= 0 (length mess)) (setq prefix "1"))
	(setq packed-mess (concat packed-mess prefix this-chunk))
     )
     packed-mess
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message enconding/decoding layer.
;;;
;;; Uses the WDDX protocol to exchange message description with VoiceCode.
;;;
;;; Emacs counterpart of VoiceCode Python class messaging.MessEncoderWDDX.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vcode-decode-mess (mess)
    "Parses a WDDX \"document\" and converts it to a LISP data structure"

    (let ((mess-cont) (mess-name))
       ;;;
       ;;; Parse the message in a temporary buffer
       ;;;
; Using switch-to-buffer sometimes causes a different buffer to be
; displayed when we return (despite the save-excursion call).
; Instead, we use with-temp-buffer, which doesn't have that effect.
; -- DCF
;       (save-excursion
;         (switch-to-buffer "*VC WDDX Message*") 
;         (erase-buffer)
;         (insert mess)
;         (setq dom (xml-parse-region (point-min) (point-max) (current-buffer)))
; 	 (kill-buffer nil)
;       )
       (with-temp-buffer
          (insert mess)
          (setq dom (xml-parse-region (point-min) (point-max) (current-buffer)))
       )

   
       ;;;
       ;;; Convert to a Lisp hash table
       ;;;
       (setq mess-cont (wddx-deserialize dom))
       (setq mess-name (cl-gethash "message_name" mess-cont))
       (cl-remhash "message_name" mess-cont)
       (setq result (list mess-name mess-cont))
       result
    )
)

(defun vcode-encode-mess (mess-name mess-content)
   "Translates a LISP data structure into a WDDX \"document\"."
 

   ; Debuggin problem with reporting changes involving unicode characters
   ; AD 2005-11-03
   (if (string= "mess-name" "updates")
      (vcode-trace "vcode-encode-mess" "Message is of type updates. (cl-gethash \"value\" mess-content)=%S" (cl-gethash "value" mess-content))
   )

   (let ((wddx-mess))

     ;;; Generate header
     (setq wddx-mess "<?xml version=\"1.0\" encoding=\"UTF-8\" ?><!DOCTYPE wddxPacket SYSTEM \"wddx_0090.dtd\"><wddxPacket version=\"0.9\"><header/><data>")


     ;;; Generate data part
     (cl-puthash "message_name" mess-name mess-content)
     (setq wddx-mess (concat wddx-mess (wddx-serialize-data mess-content)))

     ;;; Generate closing of header
     (setq wddx-mess (concat wddx-mess "</data></wddxPacket>"))
     (vcode-trace "vcode-encode-mess" "returning message:\n%S\n" wddx-mess)
     wddx-mess
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Responding to VoiceCode messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Associate message names with the callback function to be invoked in 
;;; response to it
;;;
(defvar vcode-callbacks (make-hash-table))
(cl-puthash 'cur_pos' 'cur_pos_cbk vcode-callbacks)
(cl-puthash 'get_selection' get_selection_cbk vcode-callbacks)
(cl-puthash 'set_selection' set_selection_cbk vcode-callbacks)
(cl-puthash 'get_text' get_text_cbk vcode-callbacks)
(cl-puthash 'make_position_visible' make_position_visible_cbk vcode-callbacks)
(cl-puthash 'len' len_cbk vcode-callbacks)
(cl-puthash 'insert' insert_cbk vcode-callbacks)
(cl-puthash 'delete' delete_cbk vcode-callbacks)
(cl-puthash 'goto' goto_cbk vcode-callbacks)
(cl-puthash 'active_buffer_name' active_buffer_name_cbk vcode-callbacks)
(cl-puthash 'multiple_buffers' multiple_buffers_cbk vcode-callbacks)
(cl-puthash 'bidirectional_selection' bidirectional_selection_cbk vcode-callbacks)
(cl-puthash 'get_visible' get_visible_cbk vcode-callbacks)
(cl-puthash 'language_name' language_name_cbk vcode-callbacks)
(cl-puthash 'newline_conventions' newline_conventions_cbk vcode-callbacks)
(cl-puthash 'pref_newline_convention' pref_newline_convention_cbk vcode-callbacks)
(cl-puthash 'start_responding' start_responding_cbk vcode-callbacks)
(cl-puthash 'stop_responding' stop_responding_cbk vcode-callbacks)
(cl-puthash 'open_file' open_file_cbk vcode-callbacks)
(cl-puthash 'refresh_if_necessary' refresh_if_necessary_cbk vcode-callbacks)