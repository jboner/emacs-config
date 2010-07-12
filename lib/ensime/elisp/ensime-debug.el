;;; ensime-debug.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This file includes code from slime.el of the SLIME project
;;     (also licensend under the GNU General Public License.) The
;;     following copyrights therefore apply:
;;     
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


(provide 'ensime-debug)


(defcustom ensime-debug-cmd-template 
  '("jdb" "-classpath" :classpath "-sourcepath" :sourcepath :debug-class :debug-args)
  "The command to launch the debugger. Keywords will be replaced
with data loaded from server."
  :type 'string
  :group 'ensime-debug)


(defcustom ensime-debug-default-cmd-line '("jdb")
  "Default command to launch the debugger, used when not connected to an ENSIME
server."
  :type 'string
  :group 'ensime-debug)


(defun ensime-debug-start ()
  "Run a Scala interpreter in an Emacs buffer"
  (interactive)
  (let* ((root-path (or (ensime-configured-project-root) "."))
	 (cmd-line (ensime-debug-get-cmd-line))
	 (command-str (ensime-debug-make-cmd-string cmd-line)))
    (jdb command-str)
    ))


(defun ensime-debug-make-cmd-string (cmd-line)
  "Concatenate the elements of cmd-line to create the command line.
-classpath and -sourcepath must be concatenated without intervening spaces."
  (let ((str "")
	(prev nil))
    (dolist (ea cmd-line)
      (cond
       ((equal prev "-classpath") (setq str (concat str (or ea "."))))
       ((equal prev "-sourcepath") (setq str (concat str (or ea "."))))
       (t (setq str (concat str " " (or ea "")))))
      (setq prev ea))
    str
    ))


(defun ensime-debug-get-cmd-line ()
  "Get the command needed to launch a debugger, including all
the current project's dependencies. Returns list of form (cmd [arg]*)"
  (if (ensime-connected-p)
      (let* ((conf (ensime-rpc-debug-config)))
	(ensime-replace-keywords ensime-debug-cmd-template conf))
    ensime-debug-default-cmd-line))


(provide 'ensime-debug)