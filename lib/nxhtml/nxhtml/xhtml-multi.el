;;; xhtml-multi.el --- Editing of embedded <?MODE...?> parts
;;
;; Description:
;; Author:
;; Maintainer:
;; Created: Tue Dec 19 15:39:09 2006
(defconst xhtml-multi:version "0.51") ;; Version:
;; Lxast-Updated: Sat Mar 03 22:53:40 2007 (3600 +0100)
;; Keywords:
;; Compatibility:
;;
;; Fxeatures that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   Handling of XHTML files with <?php ?> type subfields. See /gr
;;
;; Fix-me: It is kind of tricky handling the local variables here.
;; Fix-me: Does not currently exit to mode not handled correctly.
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

(defgroup xhtml-multi nil
  "Customization group for `xhtml-multi-mode'."
  :group 'nxhtml
  :group 'html)

(defcustom xhtml-multi-xhtml-mode 'nxhtml-part-mode
  "Mode to use for XHTML part of buffer."
  :type 'function
  :group 'xhtml-multi)

(defcustom xhtml-multi-major-modes
  '(php-mode
    nxml-part-mode
    nxhtml-part-mode)
  "List of major modes that should use `xhtml-multi-mode'."
  :type '(repeat function)
  :group 'xhtml-multi)

(defcustom xhtml-multi-submodes
  '(("php" . php-mode))
  "Modes to use for <?LANG ?> parts of buffer.
Association list where the first part is matched to LANG and the
second part is the major mode function to use."
  :type '(repeat (cons string function))
  :group 'xhtml-multi)

(defface xhtml-multi-outside-submode
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "gray34")
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "gray93")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "lightgoldenrod2")
    (((class color) (min-colors 8))
     :background "blue" :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face for parts outside a submode."
  :group 'xhtml-multi)


(defvar xhtml-multi-update-timer nil)
(make-variable-buffer-local 'xhtml-multi-update-timer)


(defvar xhtml-multi-mode-in-buffer nil)
(make-variable-buffer-local 'xhtml-multi-mode-in-buffer)

(defun xhtml-multi-update (buffer)
  "Update major mode for buffer in selected window."
  (with-current-buffer buffer
    (when xhtml-multi-mode
      (when xhtml-multi-mode-in-buffer
        (let (errmsg)
          (condition-case info
              (xhtml-multi-set-major)
            (error (setq errmsg (error-message-string info))
                   (message "In xhtml-multi-update: %s" errmsg)
                   (setq xhtml-multi-mode-in-buffer nil))))))))

(defvar xhtml-multi-internal-change-major nil)

(defun xhtml-multi-after-change-major()
  ;; For after-change-major-mode-hook
  (unless xhtml-multi-internal-change-major
    (when (memq major-mode xhtml-multi-major-modes)
      (setq xhtml-multi-mode-in-buffer t)))
  (when (memq major-mode '(nxhtml-part-mode nxml-part-mode))
    (setq nxml-part-keep-me t))
  t)

(defvar xhtml-multi-ovl-begin nil)
(make-variable-buffer-local 'xhtml-multi-ovl-begin)
(put 'xhtml-multi-ovl-begin 'permanent-local t)
(defvar xhtml-multi-ovl-end nil)
(make-variable-buffer-local 'xhtml-multi-ovl-end)
(put 'xhtml-multi-ovl-end 'permanent-local t)

(defun xhtml-multi-set-major()
  (let* ((here (point))
         php-end
         php-beg
         php-name
         php-cons
         top-ovl-end
         btm-ovl-beg
         want-major)
    ;; Check backward for  <? type changes:
    (save-excursion
      (save-match-data
        (setq php-end   (search-backward "?>" nil t))
        (goto-char here)
        (setq php-beg (search-backward "<?" nil t))
        (when (looking-at "<\\?\\([a-zA-Z]+\\)")
          (setq php-name (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))
    ;; Get the major mode to change to and top overlay end:
    (if (not (or php-end php-beg))
        (setq want-major xhtml-multi-xhtml-mode)
      (if (and php-end php-beg)
          (if (> php-beg php-end)
              (setq top-ovl-end php-beg)
            (setq top-ovl-end php-end)
            (setq want-major xhtml-multi-xhtml-mode))
        (if php-beg
            (setq top-ovl-end php-beg)
          (setq top-ovl-end php-end)
          (setq want-major xhtml-multi-xhtml-mode))))
    (and (not want-major)
         php-name
         (when (setq php-cons (assoc php-name xhtml-multi-submodes))
           (setq want-major (cdr php-cons))))
    ;; Delete top overlay if nowhere to place it:
    (and xhtml-multi-ovl-begin
         (not top-ovl-end)
         (delete-overlay xhtml-multi-ovl-begin))
    ;; Move the top overlay:
    (when top-ovl-end
      (if xhtml-multi-ovl-begin
          (unless (= (overlay-end xhtml-multi-ovl-begin)
                     top-ovl-end)
            (move-overlay xhtml-multi-ovl-begin (point-min) top-ovl-end))
        (setq xhtml-multi-ovl-begin (make-overlay (point-min) top-ovl-end))
        (overlay-put xhtml-multi-ovl-begin 'face 'xhtml-multi-outside-submode)))
    ;; Find where to place beginning of bottom overlay:
    (save-excursion
      (save-match-data
        (goto-char here)
        (setq btm-ovl-beg (search-forward
                           (if (and top-ovl-end
                                    php-end
                                    (= top-ovl-end php-end))
                               "<?"
                             "?>")
                           nil t))))
    ;; Delete bottom overlay if nowhere to place it:
    (and xhtml-multi-ovl-end
         (not btm-ovl-beg)
         (delete-overlay xhtml-multi-ovl-end))
    ;; Move the bottom overlay:
    (when btm-ovl-beg
      (if xhtml-multi-ovl-end
          (unless (= (overlay-start xhtml-multi-ovl-end)
                     btm-ovl-beg)
            (move-overlay xhtml-multi-ovl-end btm-ovl-beg (point-max)))
        (setq xhtml-multi-ovl-end (make-overlay btm-ovl-beg (point-max)))
        (overlay-put xhtml-multi-ovl-end 'face 'xhtml-multi-outside-submode)))
    ;; If we have a major mode to switch to then do it:
    (and want-major
         (not (eq major-mode want-major))
      (let ((xhtml-multi-internal-change-major t))
        (let ((nxhtml-part-invisible-hf t))
          (funcall want-major)
          ;;(run-hooks 'post-command-hook)
          ;;(clear-this-command-keys)
          )
        (setq xhtml-multi-mode-in-buffer t)
        (run-with-idle-timer 0.02 nil 'message "Mode switched to %s" major-mode)
        (top-level)
        ))))

(define-minor-mode xhtml-multi-mode
  "Switch major mode automatically for different parts of buffer.
The buffer should contain XHTML style code mixed with some
embedded mode, for example <?php ?>. When point is outside the
embedding a HTML style major mode is used (see
`xhtml-multi-xhtml-moe'). When point is inside the embedding a
major mode based on the language choosen there is selected (see
`xhtml-multi-submodes')."
  :global t
  :group 'xhtml-multi
  (if xhtml-multi-mode
      ;;Turn it on
      (progn
        (when (boundp 'nxml-part-keep-me) (setq nxml-part-keep-me t))
        ;; Local, last so that moving hooks get their chance first:
        (add-hook 'after-change-major-mode-hook 'xhtml-multi-after-change-major t)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (xhtml-multi-after-change-major)))
        (setq xhtml-multi-update-timer
              (run-with-idle-timer idle-update-delay t 'xhtml-multi-update (current-buffer))))
    ;; Turn it off
    (dolist (buf (buffer-list))
      (when (boundp 'nxml-part-keep-me) (setq nxml-part-keep-me nil))
      (with-current-buffer buf
        (when (overlayp xhtml-multi-ovl-begin)
          (delete-overlay xhtml-multi-ovl-begin))
        (when (overlayp xhtml-multi-ovl-end)
          (delete-overlay xhtml-multi-ovl-end))))
    (remove-hook 'after-change-major-mode-hook 'xhtml-multi-after-change-major)
    (when (timerp xhtml-multi-update-timer)
      (cancel-timer xhtml-multi-update-timer))
    (setq xhtml-multi-update-timer nil)))
(when (and xhtml-multi-mode
           (not (boundp 'define-globa-minor-mode-bug)))
  (xhtml-multi-mode 1))


(provide 'xhtml-multi)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xhtml-multi.el ends here
