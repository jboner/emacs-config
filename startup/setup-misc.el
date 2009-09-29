;;; ------------------------------------------------
(require 'linum)
(require 'htmlize)

;;; ------------------------------------------------
(set-buffer-file-coding-system 'utf-8-unix)

;;; ------------------------------------------------
;;; TextMate-like Command-T
;(load-file "$EMACS_LIB/lib/misc/command_t.el")
;(global-set-key "\M-t" 'find-tag-file)

;;; ------------------------------------------------
;;; GNU Server
;(load-file "$EMACS_LIB/lib/misc/gnuserv.el")
;(load-library "gnuserv")
;(gnuserv-start)
;(setq gnuserv-frame (selected-frame))

;;; ------------------------------------------------
;;; CUA mode
;(if (string-equal "21" (substring (emacs-version) 10 12))
;  (progn 
;    (load-file "$EMACS_LIB/lib/misc/cua.el")
;    (require 'cua)
;(if (string-equal "22" (substring (emacs-version) 10 12))
;  (progn 
;    (cua-mode t)))
(cua-mode t)

;;; ------------------------------------------------
(global-font-lock-mode 1)
(setq c-basic-offset 2)
(setq tab-width 4)
(show-paren-mode t)
(setq-default abbrev-mode t)
(column-number-mode t)
(line-number-mode t)
(setq visible-bell t)

;;; ------------------------------------------------
(setq max-lisp-eval-depth 1000)
(setq max-specpdl-size 1000)

;;; ------------------------------------------------
;; no TABs allowed
(setq-default indent-tabs-mode nil)
;(highlight-tabs)
;(highlight-trailing-whitespace)

;;; ------------------------------------------------
;; Make all yes-or-no questions as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;;; ------------------------------------------------
;; turn off menu and tool bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; ------------------------------------------------
(load "completion")
(initialize-completions)

;;; ------------------------------------------------
;;; inserts current date and time
(defun insert-current-time (arg)
  "Insert current time string at current position"
  (interactive "p")
  (insert (current-time-string)))

;;; ------------------------------------------------
;;; Very useful function allowing to jump to matching parent
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

;;; ------------------------------------------------
;;; Indent whole buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;; ------------------------------------------------
;;; IDO
;(if (string-equal "21" (substring (emacs-version) 10 12))
;  (progn ; only load cua for 21.x
;    (load-file "$EMACS_LIB/lib/misc/ido.el")))
;	(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq 
 ido-ignore-buffers              ; ignore these guys
 '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido") 
 ido-everywhere t                ; use for many file dialogs
 ido-case-fold  t                ; be case-insensitive
 ido-use-filename-at-point t     ; try to use filename...
 ido-use-url-at-point t          ; ... or url at point
 ido-max-prospects 10            ; don't spam my minibuffer
 ido-confirm-unique-completion t ; wait for RET, even with unique completion
)
(add-hook 'ido-setup-hook 
          (lambda () 
            (define-key ido-completion-map [tab] 'ido-complete)))									   

;; ido-mode for M-x
;(defun ido-execute ()
;  (interactive)
;  (call-interactively
;   (intern
;    (ido-completing-read
;     "M-x "
;     (let (cmd-list) 
;       (mapatoms (lambda (S) (when (commandp S) (setq cmd-list (cons (format "%S" S) cmd-list)))))
;       cmd-list)))))
;(global-set-key "\M-x" 'ido-execute)

;;; ------------------------------------------------
;;; IDO - FILE CACHE
(defun file-cache-ido-find-file (file)
  "Using ido, interactively open file from file cache'. 
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive (list (file-cache-ido-read "File: "
                                          (mapcar
                                           (lambda (x)
                                             (car x))
                                           file-cache-alist))))
  (let* ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (car (cdr record))
        (file-cache-ido-read
         (format "Find %s in dir: " file) (cdr record)))))))
 
(defun file-cache-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
         (lambda ()
           (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))
 
;;; ------------------------------------------------
;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;;; ------------------------------------------------
;; compilation; if compilation is successful, autoclose the compilation win
;; http://www.emacswiki.org/cgi-bin/wiki/ModeCompile
;; TODO: don't hide when there are warnings either (not just errors)
;(setq compilation-window-height 12)
;(setq compilation-finish-functions 'compile-autoclose)
;(defun compile-autoclose (buffer string)
;  (cond ((and (string-match "finished" string)
;	   (not (string-match "warning" string)))
;	  (message "Build maybe successful: closing window.")
;          (run-with-timer 2 nil                      
;	    'delete-window              
;	    (get-buffer-window buffer t)))
;    (t                                                                    
;      (message "Compilation exited abnormally: %s" string))))

;;; ------------------------------------------------
;;; TextMate-like Command-T
;(load-file "$EMACS_LIB/lib/misc/command_t.el")
;(add-hook 'emacs-lisp-mode-hook (lambda (setl ffip-regexp ".*\\.scala")))
;(load-file "$EMACS_LIB/lib/misc/project-local-variables.el")
;(load-file "$EMACS_LIB/lib/misc/find-file-in-project.el")
;(global-set-key "\M-t" 'find-file-in-project)

;;; ------------------------------------------------
;; full-screen mode
;; based on http://www.emacswiki.org/cgi-bin/wiki/WriteRoom
;; toggle full screen with F3; require 'wmctrl'
(when (executable-find "wmctrl") ;sudo apt-get install wmctrl
  (defun full-screen-toggle ()
    (interactive)
    (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen")))

;;; ------------------------------------------------
;; jump out from a pair(like quote, parenthesis, etc.)
(defun jump-out-of-pair ()
  (interactive)
  (let ((pair-regexp "[^])}\"'>]*[])}\"'>]"))
    (if (looking-at pair-regexp)
        (progn
          ;; be sure we can use C-u C-@ to jump back
          ;; if we goto the wrong place
          (push-mark)
          (goto-char (match-end 0)))
      (c-indent-command))))

;;; ------------------------------------------------
;;; viper and vimpulse modes
;(setq viper-mode t)
;(require 'viper)
;(load-file "$EMACS_LIB/startup/vimpulse.el")
;(require 'vimpulse)

;;; ------------------------------------------------
;;; icicles
;(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/icicles"))
;(load-file "$EMACS_LIB/lib/icicles/icicles.el")
;(icicle-mode t)

;;; ------------------------------------------------
;;; Remote Emacs buffer using Erlang 
;(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/shbuf"))
;(require 'shbuf)

;;; ------------------------------------------------
;; turn off anti-aliasing for mac
;(setq mac-allow-anti-aliasing nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation; if compilation is successful, autoclose the compilation win
;; http://www.emacswiki.org/cgi-bin/wiki/ModeCompile
;; TODO: don't hide when there are warnings either (not just errors)
;(setq compilation-window-height 12)
;(setq compilation-finish-functions 'compile-autoclose)
;(defun compile-autoclose (buffer string)
;  (cond ((and (string-match "finished" string)
;	   (not (string-match "warning" string)))
;	  (message "Build maybe successful: closing window.")
;          (run-with-timer 2 nil                      
;	    'delete-window              
;	    (get-buffer-window buffer t)))
;    (t                                                                    
;      (message "Compilation exited abnormally: %s" string))))

(require 'filecache)
;(require 'ido) 
;(ido-mode t) 
(global-set-key (kbd "ESC ESC f") 'file-cache-ido-find-file)

;;; FILE CACHE FOR JDE MODE
;; Prevent subversion files form polluting the cache
;(add-to-list 'file-cache-filter-regexps "\\.svn-base$")
;; global variable to keep track of current project
;(defvar credmp/current-jde-project nil)
 
;; small function to re-create the cache when the project changes
;(defun credmp/update-cache ()
;  (if (not (string= jde-current-project credmp/current-jde-project))
;      (progn
;        (file-cache-clear-cache)
;        (file-cache-add-directory-using-find (substring jde-current-project 0 (- (length jde-current-project) 6))))
;    )
;  (setq credmp/current-jde-project jde-current-project)
;  )
 
;; add the hook...
;(add-hook 'jde-project-hooks 'credmp/update-cache)


