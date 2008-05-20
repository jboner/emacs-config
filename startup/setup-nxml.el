;;; nxml ------------------------------
(add-to-list 'load-path (substitute-in-file-name "$EMACS_LIB/lib/nxml-mode-20041004"))
(load-file (substitute-in-file-name "$EMACS_LIB/lib/nxml-mode-20041004/rng-auto.el"))
(require 'nxml-mode)

(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|html\\|xhtml\\)\\'" . nxml-mode)
      auto-mode-alist))

(unify-8859-on-decoding-mode)

(defun my-nxml-mode-keys ()
  (local-set-key [f1] 'nxml-complete)
  (local-set-key [C-f1] 'nxml-finish-element))

(add-hook 'nxml-mode-hook 'my-nxml-mode-keys)

;(load-file (substitute-in-file-name "$EMACS_LIB/lib/nxhtml/autostart.el"))
