;;; ERLANG ------------------------------
;; erlang mode
(add-to-list 'load-path (substitute-in-file-name "c://bin/erl5.5.5/lib/tools-2.5.5/emacs"))

;; set the location of the man page hierarchy
(setq erlang-root-dir "c://bin/erl5.5.5/")

;; add the home of the erlang binaries to the exec-path
(setq exec-path (cons "c://bin/erl5.5.5/bin" exec-path))

;; load and eval the erlang-start package to set up everything else 
(require 'erlang-start)

;; Distel
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/distel/elisp"))
(require 'distel)
(distel-setup)

;; Some Erlang customizations
(add-hook 'erlang-mode-hook
	  (lambda ()
	    ;; when starting an Erlang shell in Emacs, default in the node name
	    (setq inferior-erlang-machine-options '("-sname" "emacs"))
	    ;; add Erlang functions to an imenu menu
	    (imenu-add-to-menubar "imenu")))

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("C-M-i"    erl-complete)
    ("M-?"      erl-complete)	
    ("M-."      erl-find-source-under-point)
    ("M-,"      erl-find-source-unwind) 
    ("M-*"      erl-find-source-unwind) 
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    ;; add some Distel bindings to the Erlang shell
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

