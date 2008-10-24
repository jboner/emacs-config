;;; SCALA ------------------------------
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/scala/"))

;; ----------------------
;; Load deps
(require 'scala-mode-auto)
(require 'compile)
(require 'flymake)
(require 'font-lock)
 
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

;; ----------------------
;; Scala abbrev table setup
;; define scala abbrev table and scala mode map (for key bindings)
;(defvar scala-mode-abbrev-table nil)
;(define-abbrev-table 'scala-mode-abbrev-table nil)
;(defvar scala-mode-map ())
;(setq scala-mode-map java-mode-map)
;(define-key scala-mode-map (kbd "<tab>") 'scala-indent-line)

;(defvar scala-build-command nil)
;(make-variable-buffer-local 'scala-build-command)

;; ----------------------
;; Flymake for scala compiler
;; Flymake integration copied and heavily modified from:
;; http://snippets.dzone.com/posts/show/5013
(defun my-flymake-show-next-error() 
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-err-menu-for-current-line))

;(flymake-mode-on)
;(add-hook 'scala-mode-hook
;  (lambda () (flymake-mode-on)))

;; From the current directory, traverse down until you find a Makefile
(defun find-makefile ()
  (let ((fn buffer-file-name))
    (let ((dir (file-name-directory fn)))
      (while (and (not (file-exists-p (concat dir "/Makefile")))
	          (not (equal dir (file-truename (concat dir "/..")))))
        (setf dir (file-truename (concat dir "/.."))))
      (if (not (file-exists-p (concat dir "/Makefile")))
	  (message "No Makefile found")
	dir))))

(defun flymake-scala-init ()
  (interactive)
  (progn
    (remove-hook 'after-save-hook 'flymake-after-save-hook t)
    (save-buffer)
    (add-hook 'after-save-hook 'flymake-after-save-hook nil t)
    (list "make" (list "-C" (find-makefile) "install"))))

;(push '(".+\\.scala$" flymake-scala-init) flymake-allowed-file-name-masks)

;; ----------------------
;; Scala indentation
(set (make-local-variable 'indent-line-function) 'scala-indent-line)

(defun scala-indent-line ()
  "Indent current line of Scala code."
  (interactive)
  (indent-line-to (max 0 (scala-calculate-indentation))))

(defun scala-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (save-excursion
    (scala-maybe-skip-leading-close-delim)
    (let ((pos (point)))
      (beginning-of-line)
      (if (not (search-backward-regexp "[^\n\t\r ]" 1 0))
	  0
	(progn
	  (scala-maybe-skip-leading-close-delim)
	  (+ (current-indentation) (* 2 (scala-count-scope-depth (point) pos))))))))

(defun scala-maybe-skip-leading-close-delim ()
  (beginning-of-line)
  (forward-to-indentation 0)
  (if (looking-at "\\s)")
      (forward-char)
    (beginning-of-line)))

(defun scala-face-at-point (pos)
  "Return face descriptor for char at point."
  (plist-get (text-properties-at pos) 'face))

(defun scala-count-scope-depth (rstart rend)
  "Return difference between open and close scope delimeters."
  (save-excursion
    (goto-char rstart)
    (let ((open-count 0)
	  (close-count 0)
	  opoint)
      (while (and (< (point) rend)
		  (progn (setq opoint (point))
			 (re-search-forward "\\s)\\|\\s(" rend t)))
	(if (= opoint (point))
	    (forward-char 1)
	  (cond

            ;; Use font-lock-mode to ignore strings and comments
	   ((scala-face-at-point (- (point) 1))) 

	   ((looking-back "\\s)")
	    (incf close-count))
	   ((looking-back "\\s(")
	    (incf open-count))
	   )))
      (- open-count close-count))))



