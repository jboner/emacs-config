;;; ORG MODE ---------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-font-lock-mode 1)                     ; for all buffers
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only

;; for remember intergration
(setq org-directory "~/.org/")
(setq org-default-notes-file "~/.notes")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(setq org-remember-templates
 '((?t "* TODO %?\n  %i\n  %a" "~/.org/TODO.org")
   (?j "* %U %?\n\n  %i\n  %a" "~/.org/JOURNAL.org")
   (?i "* %^{Title}\n  %i\n  %a" "~/.org/JOURNAL.org" "New Ideas")))

