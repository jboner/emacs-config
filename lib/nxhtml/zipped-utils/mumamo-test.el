;;; mumamo-test.el --- Test routines for mumamo
;;
;; Author: Lennart Borgman
;; Created: Sat Mar 31 03:59:26 2007
;; Version: 0.1
;; Last-Updated:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file defines some test for mumamo.el and a the minor mode
;; `mumamu-test-mode' to binde the test functions to some keys for
;; convenient use. This will define F2 to run
;; `mumamo-test-create-chunk-at' and Shift-F2 to
;; `mumamo-test-create-chunks-at-all-points'.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;;;;;;; TESTS, run in fundamental-mode buffer


(defun mumamo-test-create-chunk-at-point()
  (interactive)
  (unless mumamo-current-chunk-family
    (mumamo-select-chunk-family))
  (when mumamo-mode
    (mumamo-mode 0))
  (save-excursion
    (mumamo-remove-all-chunk-overlays)
    (let ((chunk (mumamo-create-chunk-at (point)))
          (chunk2 (mumamo-get-chunk-at (point))))
      ;;(lwarn 'test-create-chunk-at :warning "chunk=%s, chunk2=%s" chunk chunk2)
      (assert (eq chunk chunk2))
      (mumamo-fontify-region-1 (overlay-start chunk) (overlay-end chunk) nil)
      (unless mumamo-test-mode (mumamo-test-mode 1))
      chunk)))
;;(global-set-key [f2] 'mumamo-test-create-chunk-at)

(defun mumamo-test-create-chunks-at-all-points()
  (interactive)
  ;;(goto-char (point-min))
  (let (last-ovl
        this-ovl)
    (while (< (point) (point-max))
      (setq this-ovl (mumamo-test-create-chunk-at-point))
      (sit-for 0.03)
      (when last-ovl
        (if (= (point) (overlay-end last-ovl))
            (assert (= (overlay-end last-ovl) (overlay-start this-ovl)))
          (assert (= (overlay-start last-ovl) (overlay-start this-ovl)))
          (assert (= (overlay-end last-ovl) (overlay-end this-ovl)))
          ))
      (if last-ovl
          (move-overlay last-ovl (overlay-start this-ovl) (overlay-end this-ovl))
        (setq last-ovl (make-overlay (overlay-start this-ovl) (overlay-end this-ovl))))
      (forward-char 1)
      )
    (message "No problems found")))
;;(global-set-key [(shift f2)] 'mumamo-test-create-chunks-at-all)

(defun mumamo-test-fontify-region()
  (interactive)
  (let ((font-lock-mode t))
    ;;(mumamo-fontify-region-with (point-min) (point-max) nil 'php-mode nil)
    (mumamo-fontify-region (point-min) (point-max) t)))
;;(global-set-key [(meta f2)] 'mumamo-test-fontify-region)

(defun mumamo-test-easy-make()
  (interactive)
  (let* ((start-str "--Start Submode:")
         (start-reg (rx (eval start-str)
                        (0+ space)
                        (submatch
                         (0+ (any "a-z-")))
                        (0+ space)
                        "--"
                        ))
         (end-str "--End Submode--"))
    (mumamo-easy-make-chunk-fun testchunk
      start-str
      start-reg
      end-str))
  (setq mumamo-current-chunk-family
        (list "testing"
              'text-mode
              (list
               'testchunk
               ))))

(defvar mumamo-test-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [f11] 'goto-char)
    (define-key map [(meta f2)] 'mumamo-test-fontify-region)
    (define-key map [(shift f2)] 'mumamo-test-create-chunks-at-all-points)
    (define-key map [f2] 'mumamo-test-create-chunk-at-point)
    map))

(define-minor-mode mumamo-test-mode
  "For testing creating mumamo-mode chunks.
When this mode is on the following keys are defined:

  \\{mumamo-test-mode-keymap}

"
  nil
  " MuMaMo-TEST"
  :keymap mumamo-test-mode-keymap
  )

(defun mumamo-test-tell-bindings()
  (let ((s "mumamo-test-mode is on, use F2/shift-F2 for simple testing"))
    (put-text-property 0 (length s)
                       'face 'font-lock-warning-face
                       s)
    (message "%s" s)))
(mumamo-test-mode 1)
(run-with-idle-timer 0 nil 'mumamo-test-tell-bindings)

(provide 'mumamo-test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo-test.el ends here
