;;;; modal-functions.el -- make a function which despatches on current major mode
;;; Time-stamp: <2006-08-02 12:18:07 john>
;;
;; Copyright (C) 2004, 2006  John C. G. Sturdy
;;
;; This file is part of emacs-versor.
;;
;; emacs-versor is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; emacs-versor is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with emacs-versor; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA



(defun safe-funcall (fn &rest args)
  "Call FN with remaining ARGS if not nil, else return first of ARGS"
  ;; (message "(safe-funcall %S %S)" fn args)
  (if fn
      (apply fn args)
    (car args)))

(defun remove-optional (args)
  "Remove any occurrence of &optional from ARGS."
  (let ((result nil))
    (while args
      (let ((arg (car args)))
	(if (not (eq arg '&optional))
	    (setq result (cons arg result)))
	(setq args (cdr args))))
    (nreverse result)))

(defmacro defmodel (fun args doc &optional interactive before-form after-form)
  "Define a caller for contextual FUN with ARGS and optional INTERACTIVE.
For each major mode, a function of that name should be defined using defmodal,
which see. When the function named as FUN is called, the specific function for
the current major mode is called to implement it. If no such function is defined,
the function definition for fundamental-mode is tried. If that is not defined,
the first of the arguments is returned.
If FUN has an interactive definition, each of the modal functions implementing it
should also have one.
The implementing functions are stored as a property of that name on the symbol
naming each major mode for which this function is implemented."
  (if (and interactive
	   (not (eq (car interactive) 'interactive)))
      (progn
	(setq after-form before-form
	      before-form interactive
	      interactive nil)))
  (append
   (list 'defun fun args doc)
   (if interactive
       (list interactive)
     nil)
   (if before-form
       (list before-form))
   (list (append (list 'safe-funcall
		       (list 'or 
			     (list 'get 'major-mode (list 'quote fun))
			     (list 'get ''fundamental-mode (list 'quote fun))))
		 (remove-optional args)))
   (if after-form
       (list after-form))))

(defun modal-function-name (function mode)
  "Make a name for the implementation of FUNCTION for MODE."
  (intern (concat (symbol-name mode) "-_-" (symbol-name function))))

(defun find-modal-function (function mode)
  "Find FUNCTION as defined in MODE."
  (interactive
   (let* ((function (completing-read "Function: "
				     obarray
				     'functionp
				     t))
	  (mode (completing-read "Mode: "
				 obarray
				 (function
				  (lambda (sym)
				    (and (commandp sym)
					 (string-match "-mode" (symbol-name sym))))))))
     (list function mode)))
  (find-file (symbol-file (modal-function-name (intern function) (intern mode))))
  (goto-char (point-min))
  (re-search-forward (format find-function-regexp function)))

(defun defmodal0 (fun mode args body)
  "Define FUNCTION, for MODE with ARGS and BODY.
This is for use inside defmodal."
  (let* ((this-name (modal-function-name fun mode)))
    ;; (message "Defining %S to be %S for %S with args %S and body %S" this-name fun mode args body)
    (list 'progn
	  (append (list 'defun this-name args)
		  body)
	  (list 'put (list 'quote mode) (list 'quote fun) (list 'quote this-name)))))

(defmacro defmodal (fun mode args &rest body)
  "Define FUNCTION, for MODE with ARGS and BODY.
MODE may be a symbol naming a mode, or a list of such symbols."
  (if (consp mode)
      (append
       '(progn)
       (mapcar (lambda (this-mode)
		 (defmodal0 fun this-mode args body))
	       mode))
    (defmodal0 fun mode args body)))

(defun defmodalalias0 (fun mode def)
  "Define the implementation of symbol FUNCTION for MODE to be DEF.
For use inside defmodalalias."
  ;; todo: change this to use defmodal0, so we get the before and after forms
  (put mode fun (symbol-function def)))

(defmacro defmodalalias (fun mode def)
  "Define the implementation of FUNCTION for MODE to be DEF.
MODE may be a symbol naming a mode, or a list of such symbols."
  (if (consp mode)
      (append
       '(progn)
       (mapcar (lambda (this-mode)
		 (defmodalalias0 fun this-mode def))
	       mode))
    (defmodalalias0 fun mode def)))

(provide 'modal-functions)

;;; end of modal-functions.el
