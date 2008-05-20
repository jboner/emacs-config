;;; smart-snippet.el --- snippet with conditional expansion

;; Copyright 2007 pluskid
;;
;; Author: pluskid.zju@gmail.com
;; Version: $Id: smart-snippet.el,v 0.0 2007/05/05 23:06:37 kid Exp $
;; Keywords: snippet smart condition
;; X-URL: http://code.google.com/p/smart-snippet/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Do you feel that the emace abbrev expansion is not smart enough?
;; Expansion to code snippet should not happen in comment. And
;; sometimes the same abbrev have different meaning. e.g. in ruby,
;; <--------------------
;; if
;; -------------------->
;; should expand into
;; <--------------------
;; if cond
;;
;; end
;; -------------------->
;; but the "if" at the follow statement
;; <--------------------
;; puts "foo" if predicate
;; -------------------->
;; should not expand to anything.
;;
;; So I write this code trying to solve this problem. It require the
;; wonderful snippet.el.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'smart-snippet)
;; On how to define a conditional snippet, refer to the document of
;; `smart-snippet-abbrev' and `smart-snippet-with-abbrev-table'

;;; Tips:

;; If the indentation is incorrect, e.g: the following snippet for c++
;; will not indent correctly
;;
;; (smart-snippet-with-abbrev-table 'c++-mode-abbrev-table
;;   ("class"
;;    "class $${name}
;; {$>
;; public:$>
;;     $${name}()$>
;;     {$>
;;        $>$.
;;     }$>
;; };$>"
;;    'bol?))
;;
;; when expanded, it will become something like this:
;;
;; class foo
;; {
;; public:
;;     foo()
;;             {
;;
;;                     }
;; };
;;
;; This is because indentation is done when there are still
;; "garbage" text like "$" and "{" which can make Emacs confused.
;; The solution is to reassign the identifiers to something
;; else(it is buffer-local, so you can set it to different value
;; in a mode-hook according to different major-mode):
;;
;;  (add-hook 'c++-mode-hook
;;    (lambda ()
;;      (setq snippet-field-default-beg-char ?\()
;;      (setq snippet-field-default-end-char ?\))
;;      (setq snippet-exit-identifier "$;")))
;;
;;  (smart-snippet-with-abbrev-table 'c++-mode-abbrev-table
;;    ("class"
;;     "class $$(name)
;;  {$>
;;  public:$>
;;      $$(name)()$>
;;      {$>
;;         $>$;
;;      }$>
;;  };$>"
;;     'bol?))
;;
;; This will work fine. And you can choose appropriate characters
;; for different major-mode(language).
;; ***NOTE*** I'm modifying `snippet-insert' in snippet.el since
;; there're some bugs in it. The current version already don't need
;; this tricky things any more(but you can still use it).

;;; Implementation Notes:

;; `snippet-insert' from snippet.el is used to insert snippet.  In
;; order to allow multiple meaning of abbrev(i.e. the same abbrev
;; might expand to different thing under different condition), I
;; maintain a list of (condition . template) in a distinct variable
;; for distincting mode(e.g. smart-snippet-ruby-mode-snippets) , those
;; added later will come to the first of the list. When an abbrev is
;; triggered, the list is searched, the first snippet whose condition
;; is satisfied is then expanded. If no snippet is satisfied, expand
;; nothing(just insert a space).

;; In order to do some enhancement and bugfix, I decide to copy the
;; code of snippet.el and paste it here -- and do some modification
;; of courses. Since snippet.el is distributed under GPL, I can
;; modify and re-distribute it (smart-snippet.el is also GPLed).
;; Thanks to GPL! And thanks to Pete Kazmier(the author of snippet.el)
;; smart-snippet.el will be a fork of snippet.el. I think the
;; modification will be easier to do and the user will no longer
;; to download two separate files to use smart-snippet


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The belowing is the description from the original snippet.el
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copyright (C) 2005 Pete Kazmier

;; Version: 0.2
;; Author: Pete Kazmier

;; A quick stab at providing a simple template facility like the one
;; present in TextMate (an OSX editor).  The general idea is that a
;; snippet of text (called a template) is inserted into a buffer
;; (perhaps triggered by an abbrev), and while the point is within the
;; snippet, a special keymap is active to permit the user to cycle the
;; point to any of the defined fields (placeholders) within the
;; template via `snippet-next-field' and `snippet-prev-field'.

;; For example, the following template might be a useful while editing
;; HTML:

;;   <a href="$$">$$</a>

;; This template might be useful for python developers.  In this
;; example, reasonable defaults have been supplied:

;;   for $${element} in $${sequence}:
;;       match = $${regexp}.search($${element})

;; When a template is inserted into a buffer (could be triggered by an
;; abbrev expansion, or simply bound to some key), point is moved to
;; the first field denoted by the "$$" characters (configurable via
;; `snippet-field-identifier').  The optional default for a field is
;; specified by the "{default}" (the delimiters are configurable via
;; `snippet-field-default-beg-char' and `snippet-field-defaul-end-char'.

;; If present, the default will be inserted and highlighted.  The user
;; then has the option of accepting the default by simply tabbing over
;; to the next field (any other key bound to `snippet-next-field' in
;; `snippet-map' can be used).  Alternatively, the user can start
;; typing their own value for the field which will cause the default
;; to be immediately replaced with the user's own input.  If two or
;; more fields have the same default value, they are linked together
;; (changing one will change the other dynamically as you type).

;; `snippet-next-field' (bound to <tab> by default) moves the point to
;; the next field.  `snippet-prev-field' (bound to <S-tab> by default)
;; moves the point to the previous field.  When the snippet has been
;; completed, the user simply tabs past the last field which causes
;; the snippet to revert to plain text in a buffer.  The idea is that
;; snippets should get out of a user's way as soon as they have been
;; filled and completed.

;; After tabbing past all of the fields, point is moved to the end of
;; the snippet, unless the user has specified a place within the
;; template with the `snippet-exit-identifier' ("$." by default).  For
;; example:

;;   if ($${test} {
;;       $.
;;   }

;; Indentation can be controlled on a per line basis by including the
;; `snippet-indent' string within the template.  Most often one would
;; include this at the beginning of a line; however, there are times
;; when indentation is better performed in other parts of the line.
;; The following shows how to use the functionality:

;;   if ($${test}) {
;;   $>this line would be indented
;;   this line will be indented after being inserted$>
;;   }

;;; Usage:

;; Snippets are inserted with the `snippet-insert' function.  This
;; function inserts the snippet into the current buffer.  It expects a
;; single argument which is the template that is to be inserted.  For
;; example:

;;   (snippet-insert "for $${element} in $${sequence}:")

;; `snippet-insert' can be called interactively in which case the user
;; is prompted for the template to insert.  This is hardly useful at
;; all unless you are testing the functionality of this code.

;; Snippets are much more effective when they are bound to expansions
;; for abbreviations.  When binding a snippet to an abbreviation, it
;; is important that you disable the insertion of the character that
;; triggered the expansion (typically some form of whitespace).  For
;; example, this is what you should NOT do:

;;   (define-abbrev python-mode-abbrev-table  ; abbrev table
;;                  "for"                     ; name
;;                  ""                        ; expansion
;;                  '(lambda ()               ; expansion hook
;;                     (snippet-insert
;;                      "for $${element} in $${sequence}:")))

;; The above example does not work as expected because after the
;; expansion hook is called, the snippet is inserted, and the point is
;; moved to the first field.  The problem occurs because when the user
;; typed "f o r <Spc>", the "<Spc>" character is inserted after the
;; snippet has been inserted.  The point happens to be located at the
;; first field and thus the "<Spc>" will delete any field default that
;; was present.

;; Fortunately, this is easy to fix.  According to the documentation
;; for `define-abbrev', if the hook function is a symbol whose
;; `no-self-insert' property is non-nil, then hook can control whether
;; or not the insertion of the character that triggered the abbrev
;; expansion is inserted.  `insert-snippet' returns non-nil and thus
;; the proper way of defining the abbrev is as follows:

;;   (defun python-foo-expansion ()
;;     (snippet-insert "for $${element} in $${sequence}:"))

;;   (put 'python-foo-expansion 'no-self-insert t)

;;   (define-abbrev python-mode-abbrev-table    ; abbrev table
;;                  "for"                       ; name
;;                  ""                          ; expansion
;;                  'python-foo-expansion)      ; expansion hook

;; Unfortunately, this is a lot of work to define what should be a
;; simple abbrev.  For convenience, this package provides a macro
;; `snippet-abbrev' that can be used with much less effort:

;;   (snippet-abbrev 'python-mode-abbrev-table            ; table
;;                   "for"                               ; name
;;                   "for $${element} in $${sequence}:") ; template

;; For even more convevience, when defining a lot of abbrevs in a
;; particular abbrev table, the package provides another macro
;; `snippet-with-abbrev-table':

;;   (snippet-with-abbrev-table 'python-mode-abbrev-table
;;     ("for" .  "for $${element} in $${sequence}:")
;;     ("im"  .  "import $$")
;;     ("if"  .  "if $${True}:")
;;     ("wh"  .  "while $${True}:"))

;; Be sure that the appropriate abbrev-table is loaded before using
;; the above otherwise you'll get an error.  I use the above in my
;; python-mode-hook.

;; Finally, for those running a recent version of Emacs, you can
;; disable snippet expansion in various parts of the buffer.  I use
;; this to disable the above "for" expansion while typing comments in
;; my python code.  Add the following line to your python-mode hook:

;;   (add-hook 'pre-abbrev-expand-hook
;;             (lambda ()
;;               (setq local-abbrev-table
;;                     (if (inside-comment-p)
;;                         text-mode-abbrev-table
;;                       python-mode-abbrev-table)))
;;             nil t)))

;;; Implementation Notes:

;; This is my first significant chunk of elisp code.  I have very
;; little experience coding with elisp; however, I have tried to
;; document the code for anyone trying to follow along.  Here are some
;; brief notes on the implementation.

;; When a snippet is inserted, the entire template of text has an
;; overlay applied.  This overlay is referred to as the "bound"
;; overlay in the code.  It is used to bold-face the snippet as well
;; as provide the keymap that is used while the point is located
;; within the snippet (so users can tab between fields).  This overlay
;; is actually one character longer than the template.  The reason is
;; to make sure that our local keymap is still in effect when a user
;; is typing in a field that happens to be at the end of the
;; template.

;; In addition, for each field (denoted by snippet-field-identifier),
;; an overlay is created.  These overlays are used to provide the
;; highlighting of the field values, the location of where the point
;; should move when tab is pressed (the start of the overlay is used
;; for this purpose), as well as the hooks to delete the default value
;; if a user starts to type their own value (the modification hooks of
;; the overlay are used for this purpose).

;; Once the user has tabbed out of the snippet, all overlays are
;; deleted and the snippet then becomes normal text.  Moving the
;; cursor back into the snippet has no affect (the snippet is not
;; activated again).  The idea is that the snippet concept should get
;; out of the users way as quickly as possible.

;;; Comparisons to Other Packages

;; tempo.el
;;  - Template definition is very lispy (although powerful).  In
;;    contrast, snippets are simple strings with minimal syntax.
;;  - Template parameters can be prompted via minibuffer.  In
;;    contrast, snippets use overlays to visually cue the user for
;;    parameters.
;;  + Templates can be wrapped around regions of text.
;;

;;; Known Limitations:

;; - When one uses something like `dabbrev-expand', when the text is
;;   inserted, it blows away a lot of the snippet.  Not sure why yet.
;; - Using 'indent-according-to-mode' does not seem to behave well
;;   with Python mode.  I have no idea why, the overlays end up
;;   getting shifted around incorrectly.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The description above is from the original snippet.el
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(provide 'smart-snippet)
(require 'cl)

(defgroup snippet nil
  "Insert a template with fields that con contain optional defaults."
  :prefix "snippet-"
  :group 'abbrev
  :group 'convenience)

(defcustom snippet-bound-face 'bold
  "*Face used for the body of the snippet."
  :type 'face
  :group 'snippet)

(defcustom snippet-field-face 'highlight
  "*Face used for the fields' default values."
  :type 'face
  :group 'snippet)

(defcustom snippet-field-identifier "$$"
  "*String used to identify field placeholders."
  :type 'string
  :group 'snippet)

(defcustom snippet-exit-identifier "$."
  "*String used to identify the exit point of the snippet."
  :type 'string
  :group 'snippet)

(defcustom snippet-field-default-beg-char ?{
  "*Character used to identify the start of a field's default value."
  :type 'character
  :group 'snippet)

(defcustom snippet-field-default-end-char ?}
  "*Character used to identify the stop of a field's default value."
  :type 'character
  :group 'snippet)

(defcustom snippet-indent "$>"
  "*String used to indicate that a line is to be indented."
  :type 'string
  :group 'snippet)

(defcustom snippet-hard-indent "$]"
  "*String used to indicate that a line is to be indented.
The difference between this and `snippet-indent' is that
`snippet-indent' is indented using `indent-according-to-mode'
while this is indented by manually inserting number of spaces
before the pos where the snippet is being expanded."
  :type 'string
  :group 'snippet)

(defcustom snippet-line-terminator "\n"
  "*String used to indicate the end of line in a snippet template."
  :type 'string
  :group 'snippet)

(defcustom snippet-escape-char-guard "PLUSKIDSMARTSNIPPETESCAPEGUARD"
  "*String used to guard the escaped character."
  :type 'string
  :group 'snippet)

(defcustom snippet-skip-same-field nil
  "*Boolean used to indicate whether to skip the next field if it
has the same name with current one. For example, if this variable
is t , for the belowing snippet template for python:

  for $${var} in $${vars}:
      print \"value of $${var} is %s\" % $${var}

At first you're at 1st $${var} field, pressing TAB you can go to
$${vars} field. But if you are in the 2nd $${var} field, pressing
TAB won't take you to the 3rd $${var} field, since they have the
same name. Instead, you'll exit the snippet in this case. For the
same reason, when you are in the 3rd $${var} field currently, by
pressing <S-tab> you'll be brought to the $${vars} field instead
of the 2nd $${var} field.

If you like this behavior, just turn on this variable. Or you can
have different behavior in different type of buffers, since it's
a buffer-local variable.")

(defvar snippet-orig-buffer-undo-list t
  "Original buffer undo list.
Buffer undo list will be disabled during constructing of a snippet.
This variable is then used to reference the original buffer undo
list.")

(make-variable-buffer-local 'snippet-field-default-beg-char)
(make-variable-buffer-local 'snippet-field-default-end-char)
(make-variable-buffer-local 'snippet-indent)
(make-variable-buffer-local 'snippet-hard-indent)
(make-variable-buffer-local 'snippet-exit-identifier)
(make-variable-buffer-local 'snippet-field-identifier)
(make-variable-buffer-local 'snippet-escape-char-guard)
(make-variable-buffer-local 'snippet-skip-same-field)
(make-variable-buffer-local 'snippet-orig-buffer-undo-list)

(defvar snippet-map (make-sparse-keymap)
  "Keymap used while the point is located within a snippet.")

;; Default key bindings
(define-key snippet-map (kbd "TAB")             'snippet-next-field)
(define-key snippet-map (kbd "<S-tab>")         'snippet-prev-field)
(define-key snippet-map (kbd "<S-iso-lefttab>") 'snippet-prev-field)

(defstruct snippet
  "Structure containing the overlays used to display a snippet.

The BOUND slot contains an overlay to bound the entire text of the
template.  This overlay is used to provide a different face
configurable via `snippet-bound-face' as well as the keymap that
enables tabbing between fields.

The FIELDS slot contains a list of overlays used to indicate the
position of each field.  In addition, if a field has a default, the
field overlay is used to provide a different face configurable via
`snippet-field-face'.

The EXIT-MARKER slot contains a marker where point should be placed
after the user has cycled through all available fields."
  bound fields exit-marker)

(defvar snippet nil
  "Snippet in the current buffer.
There is no more than one snippet per buffer.  This variable is buffer
local.")

(make-variable-buffer-local 'snippet)

(defun snippet-make-bound-overlay ()
  "Create an overlay to bound a snippet.
Add the appropriate properties for the overlay to provide: a face used
to display the snippet, the keymap to use while within the snippet,
and the modification hooks to clean up the overlay in the event it is
deleted."
  (let ((bound (make-overlay (point) (point) (current-buffer) nil nil)))
    (overlay-put bound 'keymap snippet-map)
    (overlay-put bound 'face snippet-bound-face)
    (overlay-put bound 'modification-hooks '(snippet-bound-modified))
    bound))

(defun snippet-make-field-overlay (&optional name)
  "Create an overlay for a field in a snippet.
Add the appropriate properties for the overlay to provide: a face used
to display a field's default value, and modification hooks to remove
the default text if the user starts typing."
  (let ((field (make-overlay (point) (point) (current-buffer) nil t)))
    (overlay-put field 'face snippet-field-face)
    (overlay-put field 'insert-in-front-hooks '(snippet-field-insert
                                                snippet-field-update))
    (overlay-put field 'insert-behind-hooks '(snippet-field-modified
                                              snippet-field-update))
    (overlay-put field 'modification-hooks '(snippet-field-modified
                                             snippet-field-update))
    (overlay-put field 'name (when name (intern name)))
    field))

(defun snippet-fields-with-name (name)
  "Return a list of fields whose name property is equal to NAME."
  (loop for field in (snippet-fields snippet)
        when (eq name (overlay-get field 'name))
        collect field))

(defun snippet-bound-modified (bound after beg end &optional change)
  "Ensure the overlay that bounds a snippet is cleaned up.
This modification hook is triggered when the overlay that bounds the
snippet is modified.  It runs after the change has been made and
ensures that if the snippet has been deleted by the user, the
appropriate cleanup occurs."
  (when (and after (> 2 (- (overlay-end bound) (overlay-start bound))))
    (snippet-cleanup)))

(defun snippet-field-insert (field after beg end &optional change)
  "Delete the default field value.
This insertion hook is triggered when a user starts to type when the
point is positioned at the beginning of a field (this occurs when the
user chooses to replace the field default).  In this case, the hook
deletes the field default."
  (let ((inhibit-modification-hooks t))
    (when (not after)
      (delete-region (overlay-start field) (overlay-end field)))))

(defun snippet-field-modified (field after beg end &optional change)
  "Shrink the field overlay.
This modification hook is triggered when a user starts to type when
the point is positioned in the middle or at the end of a field (this
occurs when the user chooses to edit the field default).  It is used
to ensure that the bound overlay always covers the entirety of all
field overlays, if not, its extends the bound overlay appropriately."
  (let ((bound (snippet-bound snippet)))
    (when (and after bound (> (overlay-end field) (overlay-end bound)))
      (move-overlay bound (overlay-start bound) (overlay-end field)))))

(defun snippet-field-update (field after beg end &optional change)
  "Update all fields that have the same name.
This modificition hook is triggered when a user edits any field and is
responsible for updating all other fields that share a common name."
  (let ((name (overlay-get field 'name))
        (value (buffer-substring (overlay-start field) (overlay-end field)))
        (inhibit-modification-hooks t))
    (when (and name after)
      (save-excursion
        (dolist (like-field (set-difference (snippet-fields-with-name name)
                                            (list field)))
          (goto-char (overlay-start like-field))
          (delete-region (overlay-start like-field)
                         (overlay-end like-field))
          (insert value))))))

(defun snippet-exit-snippet ()
  "Move point to `snippet-exit-identifier' or end of bound.
If the snippet has defined `snippet-exit-identifier' in the template,
move the point to that location.  Otherwise, move it to the end of the
snippet."
  (goto-char (snippet-exit-marker snippet))
  (snippet-cleanup))

(defun snippet-undo-snippet (abbrev begin end)
  "Undo inserting this snippet."
  (let ((buffer-undo-list t))           ; disable undo
    (snippet-cleanup)
    (delete-region begin end)
    (insert abbrev)))

(defun snippet-field-same-name-p (a b)
  (string= (overlay-get a 'name)
           (overlay-get b 'name)))
(defun snippet-current-field ()
  (loop for field in fields
        when (and (>= (point)
                      (overlay-start field))
                  (<= (point)
                      (overlay-end field)))
        return field))

(defun snippet-next-field ()
  "Move point forward to the next field in the `snippet'.
If there are no more fields in the snippet, point is moved to the end
of the snippet or the location specified by `snippet-exit-identifier',
and the snippet reverts to normal text."
  (interactive)
  (let* ((bound (snippet-bound snippet))
         (fields (snippet-fields snippet))
         (current-field (snippet-current-field))
         (exit (snippet-exit-marker snippet))
         (next-pos (loop for field in fields
                         for start = (overlay-start field)
                         when (and (< (point) start)
                                   (or (not snippet-skip-same-field)
                                       (null current-field)
                                       (not (snippet-field-same-name-p
                                             field
                                             current-field))))
                         return start)))
    (if (not (null next-pos))
        (goto-char next-pos)
      (goto-char exit)
      (snippet-cleanup))))

(defun snippet-prev-field ()
  "Move point backward to the previous field in the `snippet'.
If there are no more fields in the snippet, point is moved to the end
of the snippet or the location specified by `snippet-exit-identifier',
and the snippet reverts to normal text."
  (interactive)
  (let* ((bound (snippet-bound snippet))
         (fields (snippet-fields snippet))
         (current-field (snippet-current-field))
         (exit (snippet-exit-marker snippet))
         (prev-pos (loop for field in (reverse fields)
                         for end = (overlay-end field)
                         when (and (> (point) end)
                                   (or (not snippet-skip-same-field)
                                       (null current-field)
                                       (not (snippet-field-same-name-p
                                             field
                                             current-field))))
                         return (overlay-start field))))
    (if (not (null prev-pos))
        (goto-char prev-pos)
      (goto-char exit)
      (snippet-cleanup))))

(defun snippet-cleanup ()
  "Delete all overlays associated with `snippet'.
This effectively reverts the snippet to normal text in the buffer."
  (when snippet
    (when (snippet-bound snippet)
      (delete-overlay (snippet-bound snippet)))
    (dolist (field (snippet-fields snippet))
      (delete-overlay field))
    (setq snippet nil)))

(defun snippet-field-regexp ()
  "Return a regexp that is used to search for fields within a template."
  (let ((beg (char-to-string snippet-field-default-beg-char))
        (end (char-to-string snippet-field-default-end-char)))
    (concat (regexp-quote snippet-field-identifier)
            "\\("
            (regexp-quote beg)
            "\\([^"
            (regexp-quote end)
            "]+\\)"
            (regexp-quote end)
            "\\)?")))

(defun snippet-split-string (string &optional separators include-separators-p)
  "Split STRING into substrings and separators at SEPARATORS.
Return a list of substrings and optional include the separators in the
list if INCLUDE-SEPARATORS-P is non-nil."
  (let ((start 0) (list '()))
    (while (string-match (or separators snippet-line-terminator) string start)
      (when (< start (match-beginning 0))
        (push (substring string start (match-beginning 0)) list))
      (when include-separators-p
        (push (substring string (match-beginning 0) (match-end 0)) list))
      (setq start (match-end 0)))
    (when (< start (length string))
      (push (substring string start) list))
    (nreverse list)))

;; (setq snippet-exit-identifier "$;")
;;
;; This is a triky. The default identifier is "$."
;; When you write a snippet(for c/c++) like this:
;;
;;  if ($${condition})
;;  {$>
;;  $.$>
;;  }$>
;;
;; The last "}" won't indent correctly. since there is a "$." at the
;; previous line which is not a complete sentence. So I use "$;" which
;; has a ";" character at the end. This is exactly the character for
;; terminating a sentence in c/c++. Thus the "}" can indent correctly.
;; But this is only a solution for c/c++. If other languages have
;; similar problems, it won't be easy to fix it. So a better way is
;; to rewrite the `snippet-insert' function in snippet.el so that it
;; removes the "$." character before it begins to do indentation (or
;; it just never insert it).

;; Solution:

(defun snippet-split-regexp ()
  "Return a regexp to split the template into component parts."
  (concat (regexp-quote snippet-line-terminator)
          "\\|"
          (regexp-quote snippet-indent)
	  "\\|"
	  (regexp-quote snippet-hard-indent)
          "\\|"
          (regexp-quote snippet-exit-identifier)))

(defun smart-snippet-insert (abbrev template)
  "Insert a snippet into the current buffer at point.

ABBREV is the abbrev text that is triggering this snippet, recorded
for undo information.

TEMPLATE is a string that may optionally contain fields which are
specified by `snippet-field-identifier'.  Fields may optionally also
include default values delimited by `snippet-field-default-beg-char'
and `snippet-field-default-end-char'.

For example, the following template specifies two fields which have
the default values of \"element\" and \"sequence\":

  \"for $${element} in $${sequence}:\"

In the next example, only one field is specified and no default has
been provided:

  \"import $$\"

This function may be called interactively, in which case, the TEMPLATE
is prompted for.  However, users do not typically invoke this function
interactively, rather it is most often called as part of an abbrev
expansion.  See `snippet-abbrev' and `snippet-with-abbrev-table' for
more information."
  (interactive "sSnippet template: ")

  ;; Step 1: Ensure only one snippet exists at a time
  (snippet-cleanup)

  ;; Step 2: Create a new snippet and add the overlay to bound the
  ;; template body.  It should be noted that the bounded overlay is
  ;; sized to be one character larger than the template body text.
  ;; This enables our keymap to be active when a field happens to be
  ;; the last item in a template.
  (let ((start (point))
	(hard-indent (make-string (current-column) ?\ ))
        (field-markers nil))
    (flet ((end () (min (point-max)
                        (overlay-end (snippet-bound snippet)))))
      (setq snippet (make-snippet :bound (snippet-make-bound-overlay)))
      (insert template)
      (move-overlay (snippet-bound snippet) start (1+ (point)))

      ;; Step 3: Protect escape chars
      (goto-char (overlay-start (snippet-bound snippet)))
      (while (re-search-forward "\\\\\\(.\\)"
                                (end)
                                t)
        (replace-match (concat snippet-escape-char-guard
                               "\\1"
                               snippet-escape-char-guard)))

      ;; Step 3: Find and record each field's markers
      (goto-char (overlay-start (snippet-bound snippet)))
      (while (re-search-forward (snippet-field-regexp)
                                (end)
                                t)
        (let ((start (copy-marker (match-beginning 0) nil)))
          (replace-match (if (match-beginning 2) "\\2" ""))
          (push (cons start (copy-marker (point) t)) field-markers)))

      ;; Step 4: Find exit marker
      (goto-char (overlay-start (snippet-bound snippet)))
      (while (search-forward snippet-exit-identifier
                             (end)
                             t)
        (replace-match "")
        (setf (snippet-exit-marker snippet) (copy-marker (point) t)))

      ;; step 5: Do necessary indentation
      (goto-char (overlay-start (snippet-bound snippet)))
      (while (search-forward snippet-indent
                             (end)
                             t)
        (replace-match "")
        (indent-according-to-mode))
      (goto-char (overlay-start (snippet-bound snippet)))
      (while (search-forward snippet-hard-indent
			     (end)
			     t)
	(replace-match hard-indent))

      ;; Step 6: Recover escape characters
      (goto-char (overlay-start (snippet-bound snippet)))
      (while (re-search-forward (concat snippet-escape-char-guard
                                        "\\(.\\)"
                                        snippet-escape-char-guard)
                                (end)
                                t)
        (replace-match "\\1"))

      ;; Step 6: Insert the exit marker so we know where to move point
      ;; to when user is done with snippet.  If they did not specify
      ;; where point should land, set the exit marker to the end of the
      ;; snippet.
      (goto-char (overlay-start (snippet-bound snippet)))

      (unless (snippet-exit-marker snippet)
        (let ((end (overlay-end (snippet-bound snippet))))
          (goto-char (if (= end (point-max)) end (1- end))))
        (setf (snippet-exit-marker snippet) (point-marker)))

      (set-marker-insertion-type (snippet-exit-marker snippet) t)

      ;; Step 7: Create field overlays for each field and insert any
      ;; default values for the field.
      (dolist (marker-pair field-markers)
        (let ((field (snippet-make-field-overlay
                      (buffer-substring (car marker-pair)
                                        (cdr marker-pair)))))
          (push field (snippet-fields snippet))
          (move-overlay field
                        (car marker-pair)
                        (cdr marker-pair)))))

    ;; Step 7.5: Construct undo information
    (unless (eq snippet-orig-buffer-undo-list t)
      (setq snippet-orig-buffer-undo-list
            (cons (list 'apply 'snippet-undo-snippet
                        abbrev
                        (overlay-start (snippet-bound snippet))
                        (overlay-end (snippet-bound snippet)))
                  snippet-orig-buffer-undo-list))
      (setq snippet-orig-buffer-undo-list
            (cons nil
                  snippet-orig-buffer-undo-list)))

    ;; Step 8: Position the point at the first field or the end of the
    ;; template body if no fields are present.  We need to take into
    ;; consideration the special case where the first field is at the
    ;; start of the snippet (otherwise the call to snippet-next-field
    ;; will go past it).
    (let ((bound (snippet-bound snippet))
          (first (car (snippet-fields snippet))))
      (if (and first (= (overlay-start bound) (overlay-start first)))
          (goto-char (overlay-start first))
        (goto-char (overlay-start (snippet-bound snippet)))
        (snippet-next-field)))))

(defun snippet-strip-abbrev-table-suffix (str)
  "Strip a suffix of \"-abbrev-table\" if one is present."
  (if (string-match "^\\(.*\\)-abbrev-table$" str)
      (match-string 1 str)
      str))

(defun snippet-make-abbrev-expansion-hook (abbrev-table abbrev-name template)
  "Define a function with the `no-self-insert' property set non-nil.
The function name is composed of \"snippet-abbrev-\", the abbrev table
name, and the name of the abbrev.  If the abbrev table name ends in
\"-abbrev-table\", it is stripped."
  (let ((abbrev-expansion (intern
                           (concat "snippet-abbrev-"
                                   (snippet-strip-abbrev-table-suffix
                                    (symbol-name abbrev-table))
                                   "-"
                                   abbrev-name))))
    (fset abbrev-expansion
          `(lambda ()
             ,(format (concat "Abbrev expansion hook for \"%s\".\n"
                              "Expands to the following snippet:\n\n%s")
                      abbrev-name
                      template)
             (smart-snippet-insert ,abbrev-name ,template)))
    (put abbrev-expansion 'no-self-insert t)
    abbrev-expansion))

(defmacro snippet-abbrev (abbrev-table abbrev-name template)
  "Establish an abbrev for a snippet template.
Set up an abbreviation called ABBREV-NAME in the ABBREV-TABLE (note
that ABBREV-TABLE must be quoted) that expands into a snippet using
the specified TEMPLATE string.

This macro facilitates the creation of a function for the expansion
hook to be used in `define-abbrev'.  In addition, it also sets the
`no-self-insert' property on the function to prevent `abbrev-mode'
from inserting the character that triggered the expansion (typically
whitespace) which would otherwise interfere with the first field of a
snippet."
  (let ((name (gensym))
        (table (gensym)))
    `(let ((,name ,abbrev-name)
           (,table ,abbrev-table))
       (define-abbrev (symbol-value ,table) ,name ""
         (snippet-make-abbrev-expansion-hook ,table ,name ,template)))))

(defmacro snippet-with-abbrev-table (abbrev-table &rest snippet-alist)
  "Establish a set of abbrevs for snippet templates.
Set up a series of snippet abbreviations in the ABBREV-TABLE (note
that ABBREV-TABLE must be quoted.  The abbrevs are specified in
SNIPPET-ALIST.  For example:

  (snippet-with-abbrev-table 'python-mode-abbrev-table
    (\"for\" . \"for $${element} in $${sequence}:\")
    (\"im\"  . \"import $$\"))

See also `snippet-abbrev."
  (let ((table (gensym)))
    `(let ((,table ,abbrev-table))
       (progn
         ,@(loop for (name . template) in snippet-alist
              collect (list 'snippet-abbrev table name template))))))


(defun smart-snippet-expand
  (abbrev &optional abbrev-table call-directly)
  "Search the snippets related to ABBREV in TABLE(if supplied)
or the major-mode's default smart-snippet table. Expand the first
snippet whose condition is satisfied. Expand to one space if no
snippet's condition can be satisfied."
  (let* ((table (or abbrev-table
                    (smart-snippet-abbrev-table
                     (format "%s-abbrev-table"
                             major-mode))))
         (snippet-list (gethash abbrev table))
         ;; since the default abbrev expansion is case
         ;; sensitive, if I type 'If' in the comment, I
         ;; should get 'If', not 'if'. Smart-snippet
         ;; don't know what the user has typed, so just
         ;; let the default abbrev mechanism to expand
         ;; and then we get the expansion.
         (default-expansion
           (if call-directly
               abbrev
             (buffer-substring-no-properties (- (point)
                                                (length abbrev))
                                             (point)))))

    (setq snippet-orig-buffer-undo-list buffer-undo-list)
    (setq buffer-undo-list t)

    ;; if not from expanding abbrev(i.e. triggered directly
    ;; by binding keys), don't backward delete(since there
    ;; is now default expanded text)
    (unless call-directly
      (backward-delete-char (length abbrev)))
    (if (not snippet-list)              ; no abbrev found
        (progn (insert default-expansion)
               nil)                     ; permit the self-insert-command
      (while (and snippet-list
                  (not (apply
                        'smart-snippet-try-expand
                        abbrev
                        (car snippet-list))))
        (setq snippet-list (cdr snippet-list)))
      (setq buffer-undo-list snippet-orig-buffer-undo-list)
      (setq snippet-orig-buffer-undo-list t)
      (if (not snippet-list)
          (progn (insert default-expansion)
                 nil)                   ; let abbrev insert extra space
        t))))

(defun smart-snippet-try-expand (abbrev template condition)
  "Test CONDITION, if it satisfied, expand ABBREV with TEMPLATE
using `smart-snippet-insert'. Returning t to indicate that this expansion
didn't take place, should try the successive one."
  (let ((abbrev-name abbrev)
        (in-comment? (smart-snippet-inside-comment-p))
        (bol? (looking-back "^[[:blank:]]*")))
    (cond ((and (functionp condition) (funcall condition))
           (smart-snippet-insert abbrev template)
           t)
          ((and (or (symbolp condition) (listp condition))
                (eval condition))
           (smart-snippet-insert abbrev template)
           t)
          (t nil))))

;; code from http://www.mathcs.emory.edu/~mic/ftp/emacs/lisp-mic.el
(defun smart-snippet-inside-comment-p (&optional on)
  "Is the point inside a comment?
Optional ON means to also count being on a comment start."
  ;; Note: this only handles single-character commenting, as in lisp.
  (or (and on (looking-at "\\s<"))
      (save-excursion
        (skip-syntax-backward "^><")
        (and (not (bobp))
             (eq (char-syntax (preceding-char)) ?\<)))))

(defun smart-snippet-make-snippet-function-symbol
  (abbrev-name abbrev-table)
  (intern
   (concat "smart-snippet-abbrev-"
           (snippet-strip-abbrev-table-suffix
            (symbol-name abbrev-table))
           "-"
           abbrev-name)))

(defun smart-snippet-make-abbrev-expansion-hook
  (abbrev-table abbrev-name)
  "Define a function with the `no-self-insert' property set non-nil.
The function name is composed of \"smart-snippet-abbrev-\", the
abbrev table name, and the name of the abbrev.  If the abbrev
table name ends in \"-abbrev-table\", it is stripped."
  (let ((abbrev-expansion
         (smart-snippet-make-snippet-function-symbol abbrev-name
                                                     abbrev-table)))
    (if (functionp abbrev-expansion)
        abbrev-expansion
      (fset abbrev-expansion
            `(lambda (&optional call-directly)
               (interactive)
               (smart-snippet-expand ,abbrev-name
                                     ,table
                                     call-directly)))
      (put abbrev-expansion 'no-self-insert t)
      abbrev-expansion)))

(defun smart-snippet-abbrev-table (abbrev-table-name)
  (let ((table-symbol (intern
                      (concat (snippet-strip-abbrev-table-suffix
                               abbrev-table-name)
                              "-smart-snippet-abbrev-table"))))
    (unless (and (boundp table-symbol)
                 (hash-table-p (symbol-value table-symbol)))
      (set table-symbol (make-hash-table :test 'equal)))
    (symbol-value table-symbol)))

(defun smart-snippet-abbrev (abbrev-table abbrev-name template condition)
  "Establish an abbrev for a snippet template.
Set up an abbreviation called ABBREV-NAME in the
ABBREV-TABLE(must be quoted) that expands into a snippet using
the specified TEMPLATE string when CONDITION is satisfied.

if CONDITION is a function, it must accept no argument. It is
called to determine whether to expand the current abbrev.

if CONDITION is a list, it is then evaluated to determine
whether to expand the current abbrev.

if CONDITION is a symbol, it is then evaluated to determine
whether to expand the current abbrev.

All evaluation will be done under a circumstance where those
variables are available:

 abbrev-name : the current abbrev-name being expanded
 in-comment? : t if currently position is inside comment
 bol? : beginning of line(whitespaces are ignored)
"
  ;; first store the snippet template
  (let* ((table (smart-snippet-abbrev-table (symbol-name abbrev-table)))
         (snippet-list (gethash abbrev-name table))
         (snippet (assoc template snippet-list)))
    (puthash abbrev-name
             (cond ((null snippet)
                    (cons (list template condition)
                          snippet-list))
                   ;; overwrite the snippet of the same template
                   (t (setcdr snippet (cons condition nil))
                      snippet-list))
             table)

    ;; then setup the abbrev-table hook
    (define-abbrev (symbol-value abbrev-table) abbrev-name abbrev-name
      (smart-snippet-make-abbrev-expansion-hook
       abbrev-table abbrev-name))))

(defun smart-snippet-set-snippet-key
  (keymap abbrev-table abbrev-name key)
  "Some snippet can't be triggered by the default abbrev expansion.
E.g. you can't have a snippet '{' to be expand into '{ }' because
the default abbrev expansion ignore punctuations. So we must bind
them to the snippet explicitly.

KEYMAP is the keymap to define key.
ABBREV-NAME is the abbrev you used to define the snippet.
ABBREV-TABLE is the table in which you define this snippet(must be
quoted).
KEY is which key you want to bind to.

There is an example:
  (smart-snippet-set-snippet-key
    c++-mode-map 'c++-mode-abbrev-table \"{\" \"{\")"
  (define-key
    keymap
    key
    `(lambda ()
       (interactive)
       (funcall
        ',(smart-snippet-make-snippet-function-symbol abbrev-name
                                                      abbrev-table)
        t))))

(defun smart-snippet-flatten-1 (list)
  (cond ((atom list) list)
        ((listp (car list))
         (append (car list)
                 (smart-snippet-flatten-1 (cdr list))))
        (t (append (list (car list))
                   (smart-snippet-flatten-1 (cdr list))))))
(defun smart-snippet-quote-element (list)
  (loop for item in list
        collect (list 'quote item)))
(defmacro smart-snippet-with-abbrev-tables
  (abbrev-tables &rest snippets)
  (let ((tables (smart-snippet-quote-element abbrev-tables)))
    `(progn
       ,@(smart-snippet-flatten-1
          (loop for table in tables
                collect (loop for snippet in snippets
                              collect (append
                                       (list
                                        'smart-snippet-abbrev
                                        table)
                                       snippet)))))))
(defmacro smart-snippet-with-keymaps
  (keymap-and-abbrev-tables &rest map-list)
  (let ((kaymap-and-abbrev-tables
         (smart-snippet-quote-element keymap-and-abbrev-tables)))
    `(progn
       ,@(smart-snippet-flatten-1
          (loop for map-and-table in keymap-and-abbrev-tables
                collect (loop for key-mapping in map-list
                              collect (list
                                       'smart-snippet-set-snippet-key
                                       (car map-and-table)
                                       (list 'quote
                                             (cadr map-and-table))
                                       (car key-mapping)
                                       (cadr key-mapping))))))))



;;; smart-snippet.el ends here
