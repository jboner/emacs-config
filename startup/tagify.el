(defun wrap-region (left right)
  "Wrap the region in arbitrary text, LEFT goes to the left and RIGHT goes to the right."
  (interactive)
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char beg)
    (insert left)
    (goto-char (+ end (length left)))
    (insert right)
    (goto-char (+ end (length left) (length right)))))

(defun tagify-region-or-insert-self (arg)
  "If there is a visible region, call `tagify-region-or-insert', otherwise
call `self-insert-command' passing it any prefix arg given."
  (interactive "*P")
  (if (and mark-active transient-mark-mode)
      (call-interactively 'tagify-region-or-insert-tag)
    (self-insert-command (prefix-numeric-value arg))))

(defun tagify-region-or-insert-tag (tag)
  "If there is a visible region, wrap it in the given HTML/XML tag using
`wrap-region'. If any attributes are specified then they are only included
in the opening tag.

Otherwise insert the opening and closing tags and position point between the two."
  (interactive "*sTag (including attributes): \n")
  (let* ((open     (concat "<" tag ">"))
         (close    (concat "</" (car (split-string tag " ")) ">")))
    (if (and mark-active transient-mark-mode)
        (wrap-region open close)
      (insert (concat open close))
      (backward-char (length close)))))

(provide 'tagify)