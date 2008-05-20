;;; VM MAIL -------------------------------------------------
;; Some file locations are relative to my HOME or BIN directories
;(defvar home-dir)
;(setq home-dir (concat (expand-file-name "~") "/"))
;(defvar bin-dir (concat home-dir "bin/"))

;; Basic VM setup
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/vm-7.19"))
(autoload 'vm "vm" "Start VM on your primary inbox." t)
(autoload 'vm-other-frame "vm" "Like `vm' but starts in another frame." t)
(autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
(autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
(autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
(autoload 'vm-mail "vm" "Send a mail message using VM." t)
(autoload 'vm-submit-bug-report "vm" "Send a bug report about VM." t)

(setq vm-toolbar-pixmap-directory (concat (expand-file-name "~") "$EMACS_LIB/lib/vm-7.19/pixmaps"))
(setq vm-image-directory (concat (expand-file-name "~") "$EMACS_LIB/lib/vm-7.19/pixmaps"))
(setenv "PATH" (concat (concat (expand-file-name "~") "$EMACS_LIB/lib/vm-7.19/bin") ":" (getenv "PATH")))

;; Configure inbound mail (POP and IMAP)
(setq vm-spool-files
      '(
        ("~/Mail/INBOX" "pop-ssl:pop.gmail.com:995:pass:jonas@jonasboner.com:*" "~/Mail/INBOX.CRASH")
        ("~/Mail/TC" "imap-ssl:mail.terracottatech.com:993:inbox:login:jboner:*" "~/Mail/TC.CRASH")
       ))

;; Use W3M to read HTML email
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/emacs-w3m-1.4.4"))
(require 'w3m-load)
(setq vm-mime-use-w3-for-text/html nil)
(setq vm-url-browser 'w3m)

(autoload 'vm-w3m "vm" t)
(setq w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8)

;; Configure outbound mail (SMTP)
(load-library "smtpmail")
(setq smtpmail-local-domain "jonasboner.com")
(setq user-full-name "Jonas Bonér")
(setq user-mail-address "jboner@gmail.com")
(setq mail-archive-file-name "~/Mail/SENT")
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("smtp.gmail.com"
				   587
				   "jboner@gmail.com"
				   nil)))

