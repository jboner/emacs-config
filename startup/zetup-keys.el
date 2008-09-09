;;; ------------------------------------------------
;;; Key bindings

;;; Inserts pairs of characters + around selected text
(global-set-key "\"" 'skeleton-pair-insert-maybe)
(global-set-key "\'" 'skeleton-pair-insert-maybe)
(global-set-key "\`" 'skeleton-pair-insert-maybe)
(global-set-key "\{" 'skeleton-pair-insert-maybe)
(global-set-key "\[" 'skeleton-pair-insert-maybe)
(global-set-key "\(" 'skeleton-pair-insert-maybe)
(global-set-key "\<" 'tagify-region-or-insert-self)
(setq skeleton-pair 1)

;;; ------------------------------------------------
;;; hippie expand config
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand 
        try-expand-dabbrev-visible
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name))	

;;; ------------------------------------------------
(global-set-key [(meta up)]    'hippie-expand)
(global-set-key [(meta down)]  'set-mark-command)
(global-set-key [(meta right)] 'copy-region-as-kill)
(global-set-key [(meta left)]  'kill-region)

;;; ------------------------------------------------
(global-set-key [(control backspace)] 'backward-kill-word)

(global-set-key [(f1)]          'hippie-expand)
(global-set-key [(meta f1)]     'complete-tag)
(global-set-key [(control f1)]  'dabbrev-expand)

(global-set-key [(f2)]          'jump-out-of-pair)

(global-set-key [(f3)]          'full-screen-toggle)
;(global-set-key [(f4)]          'yas/next-field-group)
;(global-set-key [(meta f4)]     'yas/prev-field-group)

(global-set-key [(f5)]          'undo)
;(global-set-key [(f5)]          'versor-mode)

(global-set-key [(f6)]          'save-buffer)

(global-set-key [(meta f7)]     'theme-next)
(global-set-key [(control f7)]  'theme-prev)

(global-set-key [(f8)]          'bm-toggle)
(global-set-key [(control f8)]  'bm-previous)
(global-set-key [(meta f8)]     'bm-next)

(global-set-key [(f9)]          'find-tag)
;(global-set-key [(shift f9)]    ') ; next one: C-u M-.
;(global-set-key [(meta f9)]     ') ; go back: M-*

(global-set-key [(f10)]         'tags-search)
(global-set-key [(shift f10)]   'tags-loop-continue)
(global-set-key [(meta f10)]    'visit-tags-table)
(global-set-key [(control f10)] 'tags-query-replace)

;(global-set-key [(f11)]         'my-flymake-show-next-error)
(global-set-key [(f11)]         'next-error)
(global-set-key [(shift f11)]   'previous-error)

(global-set-key [(f12)]         'ido-switch-buffer)

(global-set-key "%"             'match-paren)
(global-set-key [(meta enter)]  'save-buffer)

;;; -----------------------------------------------
;;; local to specific modes
;  (local-set-key [f1] 'nxml-complete)
;  (local-set-key [C-f1] 'nxml-finish-element))
