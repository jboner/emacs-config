;;; nxhtml.el --- Edit XHTML files

;; Copyright (C) 2005 by Lennart Borgman
;; Parts are from Peter Heslin (see below)

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2005-08-05
(defconst nxhtml:version "1.03") ;;Version:
;; Lxast-Updated: Wed Apr 11 02:42:30 2007 (7200 +0200)
;; Keywords: languages
;; Fxeatures that might be required by this library:
;;
;;   None
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  The purpose of nxhtml.el is to add some features that are useful
;;  when editing XHTML files to nxml-mode.  For more information see
;;  `nxhtml-mode'.
;;
;;
;;  Usage:
;;
;;  See the file readme.txt in the directory above this file. Or, if
;;  you do not have that follow the instructions below.
;;
;;  Put this file in `load-path'. In your .emacs:
;;
;;     ;; Load nxml according to the instructions, ie something like:
;;     (load "your-path/nxml-mode-20041004/rng-auto.el")
;;
;;     ;; Then autoload nxhtml-mode:
;;     (autoload 'nxhtml-mode "nxhtml" "Mode for editing XHTML files - based on nxml-mode." t)
;;
;;     ;; For file associations you can use:
;;     (require 'fmode)
;;     (fmode-replace-default-mode 'html-mode 'nxhtml-mode)
;;     (fmode-replace-default-mode 'xml-mode 'nxml-mode)
;;
;;
;;  Tip: Why not put all these in a .nxml file that you load in your
;;  .emacs?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 2006-04-25: Added completion for href, src etc. Removed xhtmlin.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'cl)
  (unless (featurep 'nxml-nxhtml-autostart)
    (let ((efn (expand-file-name "../autostart.el"))) (load efn))
    (require 'rng-valid)
    (require 'rng-nxml)
    (require 'html-toc nil t)
    (require 'html-pagetoc nil t)))

(require 'button)
(require 'loadhist)
(require 'nxml-mode)
(require 'url-parse)
(require 'url-expand)
(require 'popcmp)
(require 'rngalt)
(require 'nxhtml-menu)

(defun nxhtml-version()
  "Show nxthml version."
  (interactive)
  (message "nXhtml mode version %s" nxhtml-menu:version))

(defgroup nxhtml nil
  "Customization of nxhtml-mode."
  :group 'nxml)

(defvar nxhtml-req-features
  (let ((req-features
         '(
           (html-site    "Web sites you define"
                         "html-site.el" "0.1")
           (html-chklnk  "Checking links in site"
                         "html-chklnk.el" "0.2")
           (html-move    "Moving files in web sites"
                         "html-move.el" "0.31")
           (html-pagetoc "Page TOC"
                         "html-pagetoc.el" "0.84")
           (html-toc     "Web site TOC"
                         "html-toc.el" "0.4")
           (html-wtoc    "Merge pages and web Site TOC"
                         "html-wtoc.el" "0.2")
           (html-upl     "Upload web sites"
                         "html-upl.el" "0.2")
;;            (html-inlined "Editing of inlined code"
;;                          "html-inlined.el" "2.2")
;;            (xhtml-multi  "Editing of embedded <?MODE...?>"
;;                          "xhtml-multi.el" "0.50")
           (mumamo       "Multiple major modes in buffer"
                         "mumamo.el" "0.5")
;;            (xmlpe        "Editing of XHTML fragments"
;;                          "xmlpe.el" "0.56")
           (tidy-xhtml   "Run HTML tidy program"
                         "tidy-xhtml.el")
           (xhtml-help   "HTML+CSS help"
                         "xhtml-help.el" "0.56")
           (hexcolor     "Hex color help functions"
                         "hexcolor.el" "0.5")
           (fold-dwim    "Folding on headers and tags"
                         "fold-dwim.el")
           (appmenu      "Popup menu"
                         "appmenu.el" "0.51")
           (appmenu-fold "Popup menu entries for folding"
                         "appmenu-fold.el" "0.51" appmenu fold-dwim)
           (nxml-where   "Shows XML path"
                         "nxml-where.el" "0.1")
           (mlinks       "Live XHTML links"
                         "mlinks.el" "0.25")
           )
         ))
    (dolist (extf req-features)
      (require (car extf) nil t))
    (when (featurep 'html-inlined)
      (html-inlined-add-key-to-modes))
    req-features))


(defun nxhtml-make-library-link(beg end)
  (let ((library (buffer-substring-no-properties beg end)))
    (make-text-button beg end
                      'action (lambda (button)
                                (find-library
                                 (button-get button 'lib-name)))
                      'lib-name library
                      'face 'button)))

(defun nxhtml-feature-insert(ok msg)
  (put-text-property 0 (length msg)
                     'face (list
                            (cons 'foreground-color
                                  (if ok "RGB:00/cc/00"
                                    "RGB:cc/00/00")))
                     msg)
  (insert msg))

(defun nxhtml-feature-check(feat-entry silent)
  (let ((feature     (nth 0 feat-entry))
        (description (nth 1 feat-entry))
        (file        (nth 2 feat-entry))
        (need-ver    (nth 3 feat-entry))
        (need-list   (cddddr feat-entry))
        (ok))
    (if (featurep feature)
        (let* (
               (feat-versym (read (format "%s:version" feature)))
               (feat-ver (condition-case err
                             (symbol-value feat-versym)
                           (error nil)))
               (feat-vok (or (not need-ver)
                             (and feat-ver
                                  (version<= need-ver feat-ver))))
               (need-ok (or (not need-list)
                            (let ((has t))
                              (dolist (n need-list)
                                (unless (featurep n)
                                  (setq has nil)))
                              has))))
          (setq ok (and feat-vok need-ok))
          (unless silent
            (nxhtml-feature-insert
             ok
             (concat (format "%31s -- " description)
                     (if ok
                         (format "supported by %s\n" file)
                       (concat "found " file
                               " but needs"
                               (if feat-vok ""
                                 (format " version %s" need-ver))
                               (if (or feat-vok need-ok) "" " and")
                               (if need-ok ""
                                 (format " also %s" need-list))
                               "\n"))))
            (unless (string= file
                             (file-name-nondirectory (feature-file feature)))
              (insert (make-string (+ 31 4) ?\ ) "** Bad file name: " file "\n"))))
      (unless silent
        (nxhtml-feature-insert
         nil (format "%31s -- support missing, can't find %s\n"
                     description file))))
    ok))

(defun nxhtml-features-check()
  "Check if external modules used by `nxhtml-mode' are found.
See this function for more information."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*nXhtml Optional Features Check*") t)
  (help-mode)
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((s (concat "Elisp modules used by Nxhtml Mode version " nxhtml-menu:version ":")))
      (put-text-property 0 (length s)
                         'face '( :weight bold :height 1.4)
                         s)
      (insert s "\n\n"))
    (dolist (feat-entry nxhtml-req-features)
      (nxhtml-feature-check feat-entry nil))
    (goto-char (point-min))
    (while (search-forward-regexp "[-a-zA-Z]+\\.el" nil t)
      (nxhtml-make-library-link
       (match-beginning 0)
       (match-end 0)))
    (goto-char (point-min)))
  (set-buffer-modified-p nil))

(defun nxhtml-all-features-found()
  (let ((all t))
    (dolist (feat-entry nxhtml-req-features)
      ;;(unless (featurep (car extf))
      (unless (nxhtml-feature-check feat-entry t)
        (setq all nil)))
    all))

;;(defun nxhtml-nxml-fontify-attribute (att &optional namespace-declaration)
;;"Holds the original `nxml-fontify-attribute' function.")
;;(fset 'nxhtml-nxml-fontify-attribute (symbol-function 'nxml-fontify-attribute))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Folding etc. This part is taken from
;; http://www.emacswiki.org/cgi-bin/wiki/NxmlModeForXHTML and was
;; originally written by Peter Heslin. It requires fold-dwim.el.

(when (featurep 'fold-dwim)

  (defun nxhtml-setup-for-fold-dwim ()
    (make-local-variable 'outline-regexp)
    (setq outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")
    (make-local-variable 'outline-level)
    (setq outline-level 'nxhtml-outline-level)
    (outline-minor-mode 1)
    (hs-minor-mode 1)
    (add-to-list 'hs-special-modes-alist
                 '(nxhtml-mode
                   "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
                   "</\\|-->"
                   "<!--" ;; won't work on its own; uses syntax table
                   (lambda (arg) (nxhtml-hs-forward-element))
                   nil))
    (when (featurep 'appmenu-fold)
      (appmenu-fold-setup))
    )

  (defun nxhtml-outline-level ()
    ;;(message "nxhtml-outline-level=%s" (buffer-substring (match-beginning 0) (match-end 0)))(sit-for 2)
    ;; Fix-me: What did I intend to do???
    (let ((tag (buffer-substring (match-beginning 1) (match-end 1))))
      (if (eq (length tag) 2)
          (- (aref tag 1) ?0)
        0))
    8)


  (defun nxhtml-hs-forward-element ()
    (let ((nxml-sexp-element-flag))
      (setq nxml-sexp-element-flag (not (looking-at "<!--")))
      (unless (looking-at outline-regexp)
        (condition-case nil
            (nxml-forward-balanced-item 1)
          (error nil)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun nxhtml-find-base-href()
  "Return base href found in the current file."
  (let ((base-href))
    (save-excursion
      (goto-char (point-min))
      (while (and (not base-href)
		  (search-forward-regexp "<!--[^!]*-->\\|<base[[:space:]]" nil t))
	(when (equal " " (char-to-string (char-before)))
	  (backward-char 6)
	  (when (looking-at "<base [^>]*href *= *\"\\(.*?\\)\"")
	    (setq base-href (match-string-no-properties 1))))))
    base-href))


(defvar nxhtml-saved-link-file nil
  "Saved buffer file name for use in `nxhtml-paste-link'.")
(defvar nxhtml-saved-link-anchor nil
  "Saved anchor name for use in `nxhtml-paste-link'.")

;; Fix-me: same line???
(defun nxhtml-save-link-to-here()
  "Save buffer file name+anchor for `nxhtml-paste-link'."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer has no file name")
    (setq nxhtml-saved-link-file (buffer-file-name))
    (setq nxhtml-saved-link-anchor nil)
    (save-excursion
      (let ((here (point)))
        (while (not (or (bolp) (looking-at "\\(?:id\\|name\\)[[:space:]]*=[[:space:]]*\".*?\"")))
          (backward-char))
        (when (and (looking-at "\\(?:id\\|name\\)[[:space:]]*=[[:space:]]*\"\\(.*?\\)\"")
                   (<= (match-beginning 0) here)
                   (< here (match-end 0)))
          (setq nxhtml-saved-link-anchor (match-string-no-properties 1)))))
    (message "Saved link: %s%s" nxhtml-saved-link-file
             (if nxhtml-saved-link-anchor
                 (concat "#" nxhtml-saved-link-anchor)
               ""))))

(defun nxhtml-paste-link-as-a-tag()
  "Paste link saved by `nxhtml-save-link-to-here' as an <a> tag.
Takes into account the relative position of the saved link."
  (interactive)
  (let ((paste-text (nxhtml-get-saved-link)))
    (when paste-text
      (let ((link-text (read-string "Link text: ")))
        (insert "<a href=\"" paste-text "\">" link-text "</a>")))))

(defun nxhtml-paste-link()
  "Paste link saved by `nxhtml-save-link-to-here'.
Takes into account the relative position of the saved link."
  (interactive)
  (let ((paste-text (nxhtml-get-saved-link)))
    (when paste-text
      (insert paste-text))))

(defun nxhtml-get-saved-link()
  (if nxhtml-saved-link-file
      (let* (
	     (base-href (nxhtml-find-base-href))
	     (rel (file-relative-name nxhtml-saved-link-file
				      (if base-href
					  base-href
					(file-name-directory (buffer-file-name)))))
	     (to-file (file-name-nondirectory (buffer-file-name)))
	     (anchor nxhtml-saved-link-anchor)
	     )
	(when (equal to-file rel) (setq rel ""))
	(when anchor (setq rel (concat rel "#" anchor)))
	rel)
    (message "There is no saved link")
    nil))




(defcustom nxhtml-default-encoding 'iso-8859-1
  "Default encoding."
  :type 'coding-system
  :group 'nxhtml)

(defun nxhtml-insert-empty-frames-page()
  "Insert an empty frames page."
  (interactive)
  (unless (= 0 (buffer-size))
    (error "Buffer is not empty"))
  (insert
   "<?xml version=\"1.0\" encoding=\""
   (symbol-name nxhtml-default-encoding)
   "\"?>
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
\"http://www.w3.org/xhtml1/DTD/xhtml1-frameset.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title></title>
  </head>
  <frameset cols=\"50%, 50%\">
    <frame src=\"about:blank\" />
    <frame src=\"about:blank\" />
  </frameset>
</html>")
  (search-backward "</title>"))

(defun nxhtml-insert-empty-page()
  "Insert an empty XHTML page."
  (interactive)
  (unless (= 0 (buffer-size))
    (error "Buffer is not empty"))
  (insert
   "<?xml version=\"1.0\" encoding=\""
   (symbol-name nxhtml-default-encoding)
   "\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title></title>
  </head>
  <body>
  </body>
</html>")
  (search-backward "</title>"))

(defun nxhtml-empty-page-completion()
  (unless (= 0 (buffer-size)) (error "Buffer is not empty"))
  (let* ((frames "Frameset page")
         (normal "Normal page")
         (hist (list normal frames))
         res)
    (setq res (completing-read "Insert empty page: " hist nil t normal (cons 'hist 1)))
    (cond ((string= res frames)
           (nxhtml-insert-empty-frames-page))
          ((string= res normal)
           (nxhtml-insert-empty-page))
          (t
           (error "Bad res=%s" res))))
  (rng-auto-set-schema))



(defvar nxhtml-mode-hook nil)
;;(add-hook 'nxhtml-mode-hook 'nxml-fontify-buffer)

(defun nxhtml-help()
  (interactive)
  (describe-function 'nxhtml-mode))


(defun nxhtml-view-file()
  "View file in web browser."
  (interactive)
  (if (eq major-mode 'nxhtml-part-mode)
      (nxhtml-part-view)
    (browse-url-of-file)))

(defun nxhtml-buffer-file-possibly-viewable()
  (and buffer-file-name
       (eq major-mode 'nxhtml-mode)))

(defun nxhtml-customize()
  "Customize nXhtml."
  (interactive)
  (customize-group 'nxhtml))

;; FIX-ME: When should this be done? Get tidy-menu-symbol:
(when (featurep 'tidy-xhtml)
  (tidy-build-menu))


(defvar nxhtml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?h) ?c]    'nxhtml-save-link-to-here)
    (define-key map [(control ?c) (control ?h) ?v]    'nxhtml-paste-link-as-a-tag)
    (define-key map [(control ?c) (control ?h) ?b ?f] 'nxhtml-view-file)
    (define-key map [(control ?c) (control ?h) ?b ?b] 'browse-url-of-buffer)
    (define-key map [(control ?c) tab]         'nxhtml-next-href)
    (define-key map [(control ?c) (shift tab)] 'nxhtml-prev-href)
    ;;(define-key map [C-return] 'nxhtml-complete-and-insert)
    ;;(define-key map [M-return] 'nxhtml-complete-and-insert)
    ;;(define-key map [(meta tab)] 'nxhtml-complete-and-insert)
    (define-key map [(meta tab)] 'nxml-complete)
;;    (when (featurep 'xhtmlin)
;;       (define-key map [(control ?c) (control ?h) ?i ?j] 'nxhtml-insert-javascript)
;;       (define-key map [(control ?c) (control ?h) ?i ?i] 'nxhtml-insert-img)
;;       (define-key map [(control ?c) (control ?h) ?i ?c] 'nxhtml-insert-css)
;;       (define-key map [(control ?c) (control ?h) ?i ?a] 'nxhtml-insert-a)
;;      )
    (when (featurep 'html-pagetoc)
      (define-key map [(control ?c) (control ?h) ?i ?t ?t] 'html-pagetoc-insert-toc)
      (define-key map [(control ?c) (control ?h) ?i ?t ?r] 'html-pagetoc-rebuild-toc)
      (define-key map [(control ?c) (control ?h) ?i ?t ?s] 'html-pagetoc-insert-style-guide)
      )
    (when (featurep 'xhtml-help)
      (define-key map [(control ?c) (f1) ?x] 'nxhtml-short-tag-help)
      (define-key map [(control ?c) (f1) ?c] 'xhtml-help-show-css-ref)
      )
;;     (define-key map [menu-bar nxhtml-mode]
;;       (list 'menu-item "nXhtml" nxhtml-mode-menu-map))
    map))

;; (eval-after-load 'css-mode
;;   '(when (featurep 'xhtml-help)
;;     (define-key css-mode-map [(control ?c) (f1) ?c] 'xhtml-help-show-css-ref)
;;     ))
(add-hook 'css-mode-hook
          (lambda ()
            (and (featurep 'xhtml-help)
                 (boundp 'css-mode-map)
                 (define-key css-mode-map [(control ?c) (f1) ?c]
                   'xhtml-help-show-css-ref))))

;; This should be run in `change-major-mode-hook'."
(defun nxhtml-change-mode()
  (when (featurep 'mlinks)
    (mlinks-mode 0)))

;; This should be run in `change-major-mode-hook'."
;; Should be part of nxml.el IMO
(defun nxml-change-mode()
  ;; Remove overlays used by nxml-mode.
  (save-excursion
    (unless (and (= (point-min) 1)
                 (= (point-max) (1+ (buffer-size))))
      (widen))
;;     (let ((overlays (overlays-in (point-min) (point-max))))
;;       (mapc (lambda(ovl)
;;               (let ((cat (overlay-get ovl 'category)))
;;                 (when (memq cat '(nxml-dependent rng-dependent rng-error))
;;                   (delete-overlay ovl)
;;                   )))
;;             overlays))
    (rng-validate-mode 0)
    (let ((inhibit-read-only t)
          (buffer-undo-list t)
          (modified (buffer-modified-p)))
      (nxml-with-invisible-motion
        (remove-text-properties (point-min) (point-max) '(face nil)))
      (set-buffer-modified-p modified))))

(define-derived-mode nxhtml-mode nxml-mode "nXhtml"
  "Major mode for editing XHTML documents.
It is based on `nxml-mode' and adds some features that are useful
when editing XHTML files. \\<nxhtml-mode-map>

To see an overview do \\[nxhtml-overview].

Some of the features supported by this mode are optional and
available only if other Emacs modules are found.  Use
\\[nxhtml-features-check] to get a list of these optional
features and modules needed.

The most important feature is probably completion, which is
inherited from `nxml-mode' with some small addtions.  In very
many situation you can use completion. To access it type
\\[nxml-complete]. Completion has been enhanced in the following way:

- If region is active and visible then completion will surround the
  region with the choosen tag's start and end tag.  However only the
  starting point is checked for validity. If something is wrong after
  insertion you will however immediately see it if you have validation
  on.
- It can in some cases give assistance with attribute values.
- Completion can be customized, see the menus XHTML - Completion:
  * You can use a menu popup style completion.
  * You can have alternatives grouped.
  * You can get a short help text shown for each alternative.
- There does not have to be a '<' before point for tag name
  completion. (`nxml-mode' requires a '<' before point for tag name
  completion.) CURRENTLY OUT OF FUNCTION.
- Completes xml version and encoding.
- Completes an empty file, ie inserts a skeleton.

Another feature you may find useful is the handling of links.
The href and src attribute names are underlined and a special
keymap is bound to them:

  Tab, S-Tab        Move between underlined href/src attributes
  C-c RET, Mouse-1  Follow link inside Emacs (if possible)

If the link is not into a file you can edit (a mailto link for
example) you will be prompted for an alternative action.

If you are on an id or on a name attribute C-c c will copy the id
or name and remember the file name.  C-c v will paste this as an
a-tag.

The XHTML menu is added by this mode and gives quick access and
an overview of some important features.  There is also a popup
menu added to the \[apps] key.

See also the XML menu which is added by `nxml-mode'.

Note that Tidy has its own menu."
  (add-hook 'change-major-mode-hook 'nxml-change-mode nil t)
  (add-hook 'change-major-mode-hook 'nxhtml-change-mode nil t)
  (when (featurep 'rngalt)
    (add-hook 'nxml-completion-hook 'rngalt-complete nil t))
  ;; Fails. why? Maybe because of the autoload? Use a timer instead:
  ;;(run-with-idle-timer 1 nil 'nxhtml-add-appmenu)
  ;;(define-key nxhtml-mode-map [(meta tab)] 'nxml-complete)
  (nxhtml-minor-mode 1)
  (when (featurep 'mlinks)
    (mlinks-mode 1))
  (when (featurep 'fold-dwim)
    (nxhtml-setup-for-fold-dwim)))


(defvar nxhtml-single-tags
  '("base"
    "meta"
    "link"
    "br"
    "hr"
    "frame"
    "img"
    "input"
    "option"
    "param"))

(defun nxthml-is-single-tag(tag)
  (member tag nxhtml-single-tags))

(defvar nxhtml-help-attribute-name
  '(("title"    "Element title")
    ("class"   "Style class of element")
    ("charset"  "Encoding of target")
    ("coords"   "Defining shape")
    ("href"   "Target URL")
    ("hreflang"   "Language of target")
    ("name"   "(DEPRECEATED)")
    ("rel"   "Target's relation to document")
    ("rev"   "Document's relation to target")
    ("shape"   "Area shape")
    ("target"   "Where to open target")
    ("type"   "MIME type of target")

    ("id"   "Unique id of element")
    ("lang"   "Language code")
    ("dir"   "Text direction")
    ("accesskey"   "Keyboard shortcut")
    ("tabindex"   "Tab order of element")

    ("style"   "Inline style")
    ("disabled"   "Tag initially disabled")
    ("readonly"   "User can not modify")
    ;;(""   "")

    ("alink" "(DEPRECEATED)")
    ("background" "(DEPRECEATED)")
    ("bgcolor" "(DEPRECEATED)")
    ("link" "(DEPRECEATED)")
    ("text" "(DEPRECEATED)")
    ("vlink" "(DEPRECEATED)")
    ("xml:lang" "Tag content language")
    ("cite" "URL with more info")
    ("method" "HTTP method for sending")
    ("accept" "Content types")
    ("accept-charset" "Character sets")
    ("enctype" "Encoding")
    ))
(defvar nxhtml-help-attribute-name-tag
  '(("textarea"
     ("name" "Name for textarea")
     )
    ))

(defvar nxhtml-help-tag
  (let ((h (make-hash-table :test 'equal)))
    (puthash "html"     "Document" h)
    (puthash "head"     "Document head" h)
    (puthash "title"    "Document title" h)
    (puthash "base"     "Base URL/target" h)
    (puthash "meta"     "Meta information" h)
    (puthash "style"    "Inline style sheet" h)
    (puthash "link"     "Style sheet etc" h)
    (puthash "script"   "(Java)Script code" h)
    (puthash "noscript" "Script disabled part" h)
    (puthash "isindex"  "(DEPRECEATED)" h)

    (puthash "iframe"   "Inline frame" h)
    (puthash "frameset" "Organize frames" h)
    (puthash "frame"    "Sub window" h)
    (puthash "noframes" "Substitute for frames" h)

    (puthash "bdo"      "Text direction" h)

    (puthash "body"     "Document body" h)
    (puthash "a"        "Link" h)
    (puthash "p"        "Paragraph" h)
    (puthash "span"     "Group inline elements" h)
    (puthash "br"       "Line break" h)
    (puthash "hr"       "Horizontal rule" h)
    (puthash "div"      "Division/section" h)
    (puthash "img"      "Image" h)
    (puthash "h1"       "Header 1" h)
    (puthash "del"      "Deleted text" h)
    (puthash "strike"   "(DEPRECEATED)" h)
    (puthash "u"        "(DEPRECEATED)" h)
    (puthash "s"        "(DEPRECEATED)" h)
    (puthash "ins"      "Inserted text" h)
    (puthash "sup"      "Superscript text" h)
    (puthash "center"   "(DEPRECEATED)" h)
    (puthash "dir"      "(DEPRECEATED)" h)

    (puthash "blockquote" "Long quotation" h)
    (puthash "q"          "Short quotation" h)
    (puthash "pre"      "Preformatted text" h)
    (puthash "applet"   "(DEPRECEATED)" h)
    (puthash "basefont" "(DEPRECEATED)" h)
    (puthash "font"     "(DEPRECEATED)" h)

    ;; The following elements are all font style elements. They are
    ;; not deprecated, but it is possible to achieve richer effects
    ;; using style sheets.
    (puthash "tt"       "Renders as teletype or mono spaced text" h)
    (puthash "i"        "Renders as italic text" h)
    (puthash "b"        "Renders as bold text" h)
    (puthash "big"      "Renders as bigger text" h)
    (puthash "small"    "Renders as smaller text" h)


    ;; The following tags are not deprecated, but it is possible to
    ;; achieve a much richer effect using style sheets:
    (puthash "em"       "Renders as emphasized text" h)
    (puthash "strong"   "Renders as strong emphasized text" h)
    (puthash "dfn"      "Defines a definition term" h)
    (puthash "code"     "Defines computer code text" h)
    (puthash "samp"     "Defines sample computer code" h)
    (puthash "kbd"      "Defines keyboard text" h)
    (puthash "var"      "Defines a variable" h)
    (puthash "cite"     "Defines a citation" h)

    (puthash "ul"       "Unordered list" h)
    (puthash "ol"       "Ordered list" h)
    (puthash "li"       "List element" h)
    (puthash "dl"       "Definition list" h)
    (puthash "dt"       "Definition term" h)
    (puthash "dd"       "Definition description" h)


    (puthash "fieldset" "Draw box around" h)
    (puthash "form"     "User input form" h)
    (puthash "input"    "Input field/checkbox etc" h)
    (puthash "textarea" "Input multiline field" h)
    (puthash "button"   "Push button" h)
    (puthash "label"    "Label for control" h)
    (puthash "map"      "Client side image map" h)
    (puthash "select"   "Drop down list" h)
    (puthash "option"   "Option in drop down list" h)
    (puthash "menu"     "(DEPRECEATED)" h)

    (puthash "object"   "Embedded object" h)
    (puthash "param"    "Object settings" h)

    (puthash "abbr"     "Abbreviation" h)
    (puthash "address"  "For addresses etc" h)
    (puthash "acronym"  "May be used for lookup etc" h)

    (puthash "table"    "Table" h)
    (puthash "caption"  "Table caption" h)
    (puthash "col"      "Table column attributes" h)
    (puthash "colgroup"  "Table column group" h)
    (puthash "thead"    "Table header" h)
    (puthash "tbody"    "Table body" h)
    (puthash "tfoot"    "Table footer" h)
    (puthash "tr"       "Table row" h)
    (puthash "td"       "Table cell" h)

    h))

(defun nxhtml-short-tag-help (tag)
  "Display description of tag TAG.  If TAG is omitted, try tag at point."
  (interactive
   (let ((tag (xhtml-help-tag-at-point)))
     (unless (stringp tag)
       (setq tag (read-string "No tag at point. Give tag name: ")))
     (list tag)))
  (setq tag (downcase tag))
  (let ((desc (gethash tag nxhtml-help-tag))
        (use-dialog-box nil))
    (unless desc
      (setq desc (concat tag " -- No short description available")))
    (when (y-or-n-p (concat desc ". Fetch more information from the Internet? "))
      (xhtml-help-browse-tag tag))))

(defvar nxhtml-no-single-tags nil)
(defvar nxhtml-no-end-tags nil)

(defadvice rng-complete-qname-function (around nxhtml-rng-complete-qname-function-ad
                                               (string predicate flag)
                                               disable)
  ;;(if (not (eq major-mode 'nxhtml-mode))
  (if (not nxhtml-completing-with-help)
      ad-do-it
    (setq ad-return-value
          (let ((alist (mapcar (lambda (name) (cons name nil))
                               (nxhtml-rng-generate-qname-list string))))
            (cond ((not flag)
                   (try-completion string alist predicate))
                  ((eq flag t)
                   (all-completions string alist predicate))
                  ((eq flag 'lambda)
                   (and (assoc string alist) t)))))))




(defvar nxhtml-predicate-error nil)

(defun nxhtml-find-ids(file)
  (let ((buf (find-file-noselect file)))
    (when buf
      (with-current-buffer buf
        (when (eq major-mode 'nxhtml-mode)
          (save-excursion
            (let ((ids nil)
                  (id-ptrn
                   (rx space
                       "id"
                       (0+ space)
                       ?=
                       (0+ space)
                       ?\"
                       (submatch
                        (1+ (not (any ?\")))
                        )
                       ?\"
                       )))
              (goto-char (point-min))
              (while (re-search-forward id-ptrn nil t)
                (add-to-list 'ids (match-string-no-properties 1)))
              ids)))))))

(defun nxhtml-read-url(&optional allowed-types initial-contents extra-predicate prompt-prefix)
  (popcmp-mark-completing initial-contents)
  (let ((local-ovl popcmp-mark-completing-ovl))
    (setq popcmp-mark-completing-ovl nil)
    (unwind-protect
        (let* ((url-type (nxhtml-read-url-type allowed-types initial-contents))
               (base-prompt (cond ((eq url-type 'local-file-url)
                                   "File: ")
                                  ((eq url-type 'id-url)
                                   "Id: ")
                                  ((eq url-type 'web-url)
                                   "Web URL: ")
                                  ((eq url-type 'mail-url)
                                   "e-Mail address: ")
                                  ((eq url-type 'any-url)
                                   "Any URL-type: ")
                                  (t
                                   (error "Internal error: bad url-type=%s" url-type))))
               prompt
               type-predicate
               url
               (bad-url initial-contents)
               (default-directory (if buffer-file-name
                                      (file-name-directory buffer-file-name)
                                    default-directory)))
          (when prompt-prefix
            (setq base-prompt (concat prompt-prefix " " base-prompt)))
          (setq nxhtml-predicate-error "")
          (cond ((eq url-type 'local-file-url)
                 )
                ((eq url-type 'web-url)
                 )
                ((eq url-type 'mail-url)
                 (setq type-predicate 'nxhtml-mailto-predicate)
                 (when (and (stringp bad-url)
                            (<= 7 (length bad-url))
                            (string= "mailto:" (substring bad-url 0 7)))
                   (setq bad-url (substring bad-url 7)))))
          (while (not url)
            (setq prompt (concat nxhtml-predicate-error " " base-prompt))
            (cond ((eq url-type 'local-file-url)
                   (setq url (read-file-name prompt nil "" nil bad-url extra-predicate))
                   (when (< 0 (length url))
                     ;; Fix-me: prompt for id here
                     (setq url (file-relative-name
                                (expand-file-name url)))))
                  ((eq url-type 'id-url)
                   (setq url (completing-read prompt (nxhtml-find-ids buffer-file-name)))
                   (when url
                     (setq url (concat "#" url))))
                  ((eq url-type 'web-url)
                   (setq url (nxhtml-read-from-minibuffer prompt bad-url nil nil
                                                          'nxhtml-read-web-url-history
                                                          t)))
                  ((eq url-type 'mail-url)
                   (setq url (nxhtml-read-from-minibuffer prompt bad-url nil nil
                                                          'nxhtml-read-mail-url-history
                                                          t)))
                  (t
                   (setq url (nxhtml-read-from-minibuffer prompt bad-url nil nil
                                                          'nxhtml-read-url-history
                                                          t))))
            (when (or (and type-predicate
                           (not (funcall type-predicate url)))
                      (and extra-predicate
                           (not (funcall extra-predicate url))))
              (setq bad-url url)
              (setq url)))
          (when (eq url-type 'mail-url)
            (setq url (concat "mailto:" url)))
          url)
      (delete-overlay local-ovl)
      )))

(defun nxhtml-read-url-type(allowed url-beginning)
  (let ((prompt "URL-type: ")
        (beg-type (elt (url-generic-parse-url url-beginning) 0))
        choices
        choice)
    (cond ((string= "mailto" beg-type)
           (setq allowed '(?m)))
          ((or (string= "http"  beg-type)
               (string= "https" beg-type)
               (string= "ftp"   beg-type))
           (setq allowed '(?w)))
          ((= 1 (length beg-type)) ;; w32
           (setq allowed '(?f)))
          )
    (if allowed
        (when (eq allowed t)
          (setq allowed '(?f ?i ?w ?m)))
      (setq allowed '(?f ?w)))
    (dolist (a allowed)
      (cond
       ((= a ?f)
        (setq choices (cons "File" choices)))
       ((= a ?i)
        (setq choices (cons "Id" choices)))
       ((= a ?w) (setq choices (cons "Url" choices)))
       ((= a ?m) (setq choices (cons "Mail" choices)))
       ))
    (setq choice (popcmp-completing-read prompt choices nil t
                                  "" nil nil t))
    (cond ((string= choice "Id")
           'id-url)
          ((string= choice "File")
           'local-file-url)
          ((string= choice "Url")
           'web-url)
          ((string= choice "Mail")
           'mail-url)
          )))

(defvar nxhtml-read-url-history nil)
(defvar nxhtml-read-web-url-history nil)
(defvar nxhtml-read-mail-url-history nil)

(defconst nxhtml-in-xml-attribute-value-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   ;;"<w\\(?::w\\)?\
   "<\\?xml\
\\(?:[ \t\r\n]+w\\(?::w\\)?[ \t\r\n]*=\
\[ \t\r\n]*\\(?:\"[^\"]*\"\\|'[^']*'\\)\\)*\
\[ \t\r\n]+\\(w\\(:w\\)?\\)[ \t\r\n]*=[ \t\r\n]*\
\\(\"[^\"]*\\|'[^']*\\)\\="
   t
   t))

(defun nxhtml-mailto-predicate(url)
  "Tries to match a mailto url.
This is not supposed to be entirely correct."
  (setq nxhtml-predicate-error nil)
  ;; Local pattern copied from gnus.
  (let ((r (concat "^"
                   ;;"mailto:"
                   "[a-z0-9$%(*-=?[_][^<>\")!;:,{}]*"
                   "\@"
                   "\\(?:[a-z0-9\-]+\.\\)+[a-z0-9]\\{2,4\\}$"))
        (case-fold-search t))
    ;;(message "mailpred") (sit-for 1)
    (if (string-match r url)
        t
      (setq nxhtml-predicate-error "Malformed email address.")
      nil)))

(defcustom nxhtml-image-completion-pattern
  "\\.\\(?:png\\|jpg\\|jpeg\\|gif\\)$"
  "Pattern for matching image URLs in completion."
  :type 'regexp
  :group 'nxhtml)

(defun nxhtml-image-url-predicate(url)
  (setq nxhtml-predicate-error nil)
  (if (or (file-directory-p url)
          (string-match nxhtml-image-completion-pattern url))
      t
    (setq nxhtml-predicate-error "Does not match image file name pattern.")
    nil
    ))

(defcustom nxhtml-script-completion-pattern
  "\\.\\(?:js\\)$"
  "Pattern for matching src URLs in completion in script tags."
  :type 'regexp
  :group 'nxhtml)

(defun nxhtml-script-url-predicate(url)
  (setq nxhtml-predicate-error nil)
  (if (or (file-directory-p url)
          (string-match nxhtml-script-completion-pattern url))
      t
    (setq nxhtml-predicate-error "Does not match script file name pattern.")
    nil
    ))

(defun nxhtml-coding-systems-complete(init default)
  (let (coding-systems
        hist-num
        (n 0)
        hist)
    (unless (and init (< 0 (length init)))
      (setq init default))
    (mapc (lambda(coding-system)
            (let ((mime-charset (coding-system-get coding-system 'mime-charset)))
              (when mime-charset
                (setq coding-systems (cons
                                      (symbol-name mime-charset)
                                      coding-systems)))))
          (coding-system-list t))
    (setq coding-systems (sort coding-systems 'string=))
    (mapc (lambda(coding-system)
            (unless (< 0 (length coding-system))
              (error "len=0"))
            (setq n (1+ n))
            (when (string= coding-system init) (setq hist-num n)))
          coding-systems)
    (if hist-num
        (setq hist (cons 'coding-systems hist-num))
      (setq hist 'coding-systems))
    (completing-read "Encoding (coding system): "
                     coding-systems nil t init hist)))


;; Note: This function does not currently use the state provided by
;; the nxml and rng functions directly.  Instead it searches the
;; environment near point to decide what to do.
;; (defun nxhtml-complete-and-insert()
;;   "Perform XHTML completion at point.
;; This is merely an extended version of `nxml-complete' with the following changes:

;; - If region is visible and active then completion will surround the
;;   region with the choosen tag's start and end tag.  However only the
;;   starting point is checked for validity. If something is wrong after
;;   insertion you will however immediately see it if you have validation
;;   on.
;; - Can in some cases give completion help inside attribute values.
;; - There does not have to be a '<' before point for tag name
;;   completion. (`nxml-mode' requires a '<' before point for tag name
;;   completion.)
;; - For tag names there is a popup style completion available. This
;;   gives a bit more guiding since it groups the alternative tags. Set
;;   `popcmp-popup-completion' to use this.
;; - Completes xml version and encoding.
;; - Completes an empty file, ie inserts a skeleton."
;;   (interactive)
;;   (let (res
;;         (where (nxhtml-check-where)))
;;     (or (when (eq where 'in-empty-page)
;;           (nxhtml-empty-page-completion))
;;         (when (and mark-active
;;                    transient-mark-mode
;;                    (eq where 'in-text))
;;           (nxhtml-insert-tag))
;;         (progn
;;           (cond ((memq where '(in-start-tag in-closed-start-tag in-end-tag))
;;                  (re-search-forward "\\=/?[a-z]*" nil t))
;;                 ((memq where '(in-attr))
;;                  (re-search-forward "\\=[a-z]*=" nil t))
;;                 ((memq where '(in-attr-val in-xml-attr-val))
;;                  (re-search-forward "\\=[^<>\" \t\r\n]*" nil t))
;;                 )
;;           (when (run-hook-with-args-until-success 'nxml-completion-hook)
;;             (when (re-search-backward "[^=]\"\\=" nil t)
;;               (forward-char) (delete-char 1)
;;               ;;(undo-start) (undo-more 1)
;;               )
;;             t))
;;         (when (and (not where)
;;                    (char-before)
;;                    (= ?\" (char-before)))
;;           nil)
;;         (when (or (when (char-before) (= ?> (char-before)))
;;                   (eq where 'in-text))
;;           (setq res t)
;;           (nxhtml-insert-tag))
;;         ;; Eventually we will complete on entity names here.
;;         res
;;         (progn
;;           (ding)
;;           (message "Cannot complete in this context")))))

(defvar nxhtml-in-proc-instr-back-regex "<\\?[^<>]*\\=")
(defvar nxhtml-in-proc-instr-forw-regex "\\=[^<>]*\\?>")

(defconst rngalt-in-pre-attribute-value-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   "<w\\(?::w\\)?\
\\(?:[ \t\r\n]+w\\(?::w\\)?[ \t\r\n]*=\
\[ \t\r\n]*\\(?:\"[^\"]*\"\\|'[^']*'\\)\\)*\
\[ \t\r\n]+\\(w\\(:w\\)?\\)[ \t\r\n]*=[ \t\r\n]*\
\\="
   t
   t))

(defun nxhtml-check-where()
  "Get a state for `nxhtml-complete-and-insert'."
  (let ((p (point))
        (lt-pos (save-excursion (search-backward "<" nil t)))
        res)
    (cond ((= 0 (buffer-size))
           (setq res 'in-empty-page))
          ((looking-back "<!--[^<>]*\\=" 1 t)
           (setq res 'in-comment))
          ((let ((face (get-char-property (point) 'face)))
             (when (memq face '(nxml-comment-content-face
                                nxml-comment-delimiter-face))
               (setq res 'in-comment)))
           t)
          ((looking-back nxhtml-in-xml-attribute-value-regex lt-pos t)
           (setq res 'in-xml-attr-val))
          ((looking-back nxhtml-in-proc-instr-back-regex 1 t)
           (setq res 'in-proc-instr))
          ((looking-back "<!D[^>]*\\=" 1 t)
           (setq res 'in-doctype))
          ((looking-back ">[^<]*" 1 t)
           (setq res 'in-text))
          ((looking-back rng-in-start-tag-name-regex 1 t)
           (setq res 'in-tag-start)
           (when (looking-at "\\=[^<]*>")
             (setq res 'in-closed-start-tag)))
          ((looking-back rng-in-end-tag-name-regex 1 t)
           (setq res 'in-tag-end))
          ((looking-back rng-in-attribute-regex 1 t)
           (setq res 'in-attr))
          ((looking-back rng-in-attribute-value-regex 1 t)
           (setq res 'in-attr-val))
          ((looking-back rngalt-in-pre-attribute-value-regex 1 t)
           (setq res 'in-pre-attr-val))
          ((looking-back "\"")
           (setq res 'after-attr-val))
          ((and rngalt-validation-header
                (looking-back "\\`[^<]*"))
           ;; FIX-ME: This is treated the same as in text currently,
           ;; but this should be checked. Maybe it is best to test
           ;; this here and return the relevant value?
           (setq res 'after-validation-header))
          )
    ;;(message "res=%s" res)(sit-for 1)
    (unless res
      (error "Could not find a state for completion"))
    res))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make the completions additions cleaner:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst nxhtml-tag-sets
  '(("logical"
     "del"
     "ins"
     "abbr"
     "acronym"
     "fieldset"
     "blockquote"
     "q"
     "code"
     "samp"
     "cite"
     "kbd"
     "var"
     "dfn"
     "address"
     "em"
     "strong"
     "pre"
     )
    ("physical"
     "hr"
     "sup"
     "sub"
     "font"
     "basefont"
     "br"
     "big"
     "small"
     "strike"
     "u"
     "i"
     "b"
     "s"
     "tt"
     "center"
     "bdo"
     )
    ("scripting"
     "script"
     "noscript"
     "object"
     "applet"
     )
   ("structure"
    "iframe"
    "p"
    "div"
    "span"
    "h6"
    "h5"
    "h4"
    "h3"
    "h2"
    "h1"
    )

   ("form"
    "isindex"
    "label"
    "button"
    "option"
    "select"
    "input"
    "textarea"
    "form"
    )

   ("list"
    "dt"
    "dd"
    "li"
    "dir"
    "menu"
    "ol"
    "dl"
    "ul"
    )

   ("link"
    "a"
    )

   ("image"
    "img"
    "map"
    )

   ("table"
    "table"
    "tr"
    "th"
    "td"
    "caption"
    "col"
    "colgroup"
    "thead"
    "tbody"
    "tfoot"
    )

   ("document"
    "base"
    "style"
    "link"
    "head"
    "body"
    "frame"
    "frameset"
    "noframes"
    "isindex"
    "nextid"
    "meta"
    "title"
    )
   ))

(defvar nxhtml-attr-sets
  '(("scripting"
     "onblur"
     "onchange"
     "onclick"
     "ondblclick"
     "onfocus"
     "onkeydown"
     "onkeypress"
     "onkeyup"
     "onload"
     "onunload"
     "onmousedown"
     "onmousemove"
     "onmouseout"
     "onmouseover"
     "onmouseup"
     "onreset"
     "onselect"
     "onsubmit"
     )
    ("form"
     "method"
     "accept"
     "accept-charset"
     "enctype"
     )
    ("access"
     "id"
     "name"
     "disabled"
     "readonly")
    ("layout"
     "accesskey"
     "class"
     "coords"
     "shape"
     "style"
     "tabindex"
     "title"
     "align"
     "valign"
     "alink"
     "background"
     "bgcolor"
     "link"
     "text"
     "vlink"
     "compact"
     )
    ("target"
     "charset"
     "href"
     "hreflang"
     "rel"
     "rev"
     "target"
     "type"
     )
    ("language"
     "dir"
     "lang"
     "xml:lang"
     )
    ;; id
    ;; name
    ;; xml:lang
    ))

(defun nxhtml-complete-last-try ()
  (when rng-current-schema-file-name
    (let ((where (nxhtml-check-where)))
      (cond
       ;;((eq where 'after-attr-val)
        ;;(insert " ")
        ;;)
       ((eq where 'in-pre-attr-val)
        (insert ?\"))
       ((eq where 'in-comment)
        (if (not (looking-at "[^>]*<"))
            nil
          (insert " -->")
          t))
       ((eq where 'in-xml-attr-val)
        (let (attr
              delimiter
              val)
        (save-excursion
          (save-match-data
            (re-search-forward "\\=[^<> \t\r\n\"]*" nil t)))
        (let* ((name-start (match-beginning 1))
               (name-end (match-end 1))
               (colon (match-beginning 2))
               (attr (buffer-substring-no-properties name-start
                                                     (or colon name-end)))
               (value-start (1+ (match-beginning 3)))
               (tag (save-excursion
                      (when (search-backward-regexp "<[[:alpha:]]+" nil t)
                        (match-string 0))))
               (init (buffer-substring-no-properties value-start (point))))
          (setq delimiter (char-before value-start))
          (cond ((string= "encoding" attr)
                 ;; Give a default that works in browsers today
                 (setq val (nxhtml-coding-systems-complete
                            init
                            (symbol-name nxhtml-default-encoding))))
                ((string= "version" attr)
                 (setq val "1.0")))
          (when val
            (insert val)
            t)
          )))
       ((or (eq where 'in-text)
            (eq where 'after-validation-header))
        (rngalt-complete-tag-region-prepare)
        (insert "<")
        (condition-case err
            (nxhtml-redisplay-complete)
          (quit
           (message "%s" (error-message-string err))
           (undo-start)
           (undo-more 1)
           (rngalt-complete-tag-region-cleanup)))
        t)
       (t
        ;;(message "LAST TRY where=%s" (nxhtml-check-where))(sit-for 1)
        nil)
       ))))

(defun nxhtml-img-tag-do-also()
  (insert "alt=\"")
  (rngalt-validate)
  (insert (read-string "Alt attribute: ")
          "\" ")
  (insert "src=\"")
  (rngalt-validate)
  (let ((src (nxhtml-read-url nil nil 'nxhtml-image-url-predicate "Image")))
    (insert src)
    (insert "\"")
    (when (file-exists-p src)
           (let ((sizes (image-size (create-image src) t)))
             (insert
              " width=\""  (format "%d" (car sizes)) "\""
              " height=\"" (format "%d" (cdr sizes)) "\"")
             )))
  (unless (save-match-data (looking-at "[^<]\\{,200\\}>"))
    (insert " />")))

(defun nxhtml-redisplay-complete()
  (rngalt-validate)
  (rng-cancel-timers)
  (message "")
  (redisplay t)
  (nxml-complete)
  (rng-activate-timers))

(defun nxhtml-read-from-minibuffer(prompt &optional
                                          initial-contents keymap
                                          read hist default-value
                                          inherit-input-method)
  (rng-cancel-timers)
  (message "")
  (let ((res (read-from-minibuffer prompt initial-contents keymap
              read hist default-value inherit-input-method)))
    (rng-activate-timers)
    res))

(defun nxhtml-meta-tag-do-also()
  (let ((type (popcmp-completing-read
               "Type: "
               '(
                 ;;"Refresh/Redirect"
                 "HTTP Message Headers"
                 "Robot Rules"
                 "Description for Search Engines"
                 ))))
    (cond
     ((string= type "Description for Search Engines")
      (insert " name=\"Description\"")
      (insert " content=\"")
      (insert (nxhtml-read-from-minibuffer "Description: "))
      (insert "\" />"))
     ((string= type "Robot Rules")
      (insert " name=\"Robots\"")
      (insert " content=\"")
      (nxhtml-redisplay-complete)
      (insert " />"))
     ((string= type "HTTP Message Headers")
      (insert " http-equiv=\"")
      (nxhtml-redisplay-complete)
      (insert " content=\"")
      (insert (nxhtml-read-from-minibuffer "Content: "))
      (insert "\" />")))))

(defun nxhtml-style-tag-do-also()
  (insert "type=\"text/css\"")
  (insert " media=\"")
  (nxhtml-redisplay-complete)
  (insert ">")
  (indent-according-to-mode)
  (insert "\n/* <![CDATA[ */")
  (indent-according-to-mode)
  (insert "\n")
  (indent-according-to-mode)
  (insert "\n/* ]] */")
  (indent-according-to-mode)
  (insert "\n</style>")
  (indent-according-to-mode)
  (insert "\n")
  (end-of-line -2))

(defun nxhtml-script-tag-do-also()
  (let ((type (popcmp-completing-read
               "Type: "
               '("Inlined"
                 "Linked"))))
    (cond
     ((string= type "Inlined")
      (insert "type=\"text/javascript\">")
      (indent-according-to-mode)
      (insert "\n// <![CDATA[")
      (indent-according-to-mode)
      (insert "\n")
      (indent-according-to-mode)
      (insert "\n// ]]>")
      (indent-according-to-mode)
      (insert "\n</script>")
      (indent-according-to-mode)
      (end-of-line -1))
     ((string= type "Linked")
      (insert "type=\"text/javascript\"")
      (insert " src=\"")
      (nxhtml-redisplay-complete)
      (insert "></script>")))))

(defun nxhtml-link-tag-do-also()
  (let ((type (popcmp-completing-read "Type: "
                          '(
                            "Other"
                            "Shortcut icon"
                            "Style sheet"
                            ))))
    (cond
     ((string= type "Style sheet")
      (insert " rel=\"Stylesheet\" ")
      (insert "type=\"text/css\" ")
      (insert "href=\"")
      (nxhtml-redisplay-complete)
      (insert " media=\"")
      (nxhtml-redisplay-complete)
      (insert " />"))
     ((string= type "Shortcut icon")
      (insert " rel=\"Shortcut Icon\" ")
      (insert "href=\"")
      (nxhtml-redisplay-complete)
      (insert " />"))
     (t
      (insert " ")
      (nxhtml-redisplay-complete)
      ))))

(defun nxhtml-input-tag-do-also()
  (insert " ")
  (rngalt-validate)
  ;; type=
  (insert "type=\"")
  (nxhtml-redisplay-complete)
  (insert " ")

  (let* ((choice (save-match-data
                   (when (looking-back "type=\"\\(.*\\)\" ")
                     (match-string 1)))))
    ;;(insert "type=\"" choice "\" ")
    (rngalt-validate)
    (message "choice=%s" choice)(sit-for 2)
    ;; name=
    (when (member choice '("button" "checkbox" "file" "hidden" "image"
                           "password" "radio" "text"))
      (insert "name=\""
              (read-string "Name (name): ")
              "\" ")
      (rngalt-validate))
    ;; checked=
    (when (member choice '("checkbox" "radio"))
      (when (y-or-n-p "Checked? (checked): ")
        (insert "checked=\"checked\" ")
        (rngalt-validate)))
    ;; disabled=
    (unless (string= choice "hidden")
      (unless (y-or-n-p "Enabled? : ")
        (insert "disabled=\"disabled\" ")
        (rngalt-validate)))
    ;; readonly=
    (when (string= choice "text")
      (when (y-or-n-p "Readonly? (readonly): ")
        (insert "readonly=\"readonly\" "))
      (rngalt-validate))
    (when (string= choice "file")
      ;; accept=
      (require 'mailcap)
      (condition-case err
          (let ((prompt (concat
                         "Accept mime type, RET to stop ("
                         "C-g to skip"
                         "): "))
                (mime " ")
                mimes
                (types (when (boundp 'mailcap-mime-extensions)
                         (mapcar (lambda (elt)
                                   (cdr elt))
                                 mailcap-mime-extensions))))
            (while (< 0 (length mime))
              (setq mime
                    (if types
                        (completing-read prompt types)
                      (read-string prompt)))
              (when (< 0 (length mime))
                (if mimes
                    (setq mimes (concat mimes "," mime))
                  (setq mimes mime))))
            (when (and mimes
                       (< 0 (length mimes)))
              (insert "accept=\"" mimes "\" ")))
        (quit (message "Skipped accept attribute")))
      (rngalt-validate))
    (when (string= choice "image")
      ;; alt=
      (insert "alt=\"")
      (rngalt-validate)
      (insert (read-string "Alt attribute: ")
              "\" ")
      (rngalt-validate)
      ;; src=
      (insert "src=\"")
      (rngalt-validate)
      (let ((src (nxhtml-read-url nil nil 'nxhtml-image-url-predicate "Image")))
        (insert src)
        (insert "\" "))
      (rngalt-validate))
    ;; value=
    (cond
     ((member choice '("button" "reset" "submit"))
      (nxhtml-do-also-value "Label"))
     ((member choice '("checkbox" "radio"))
      (nxhtml-do-also-value "Result"))
     ((member choice '("hidden" "password" "text"))
      (nxhtml-do-also-value "Value"))
     )
    (insert "/>")
    ;;(message "type=%s" choice)(sit-for 2)
  ))

(defun nxhtml-do-also-value(label)
  (let ((v (read-string (concat label " (value): "))))
    (when (and v
               (< 0 (length v)))
      (insert " value=\"" v "\" "))))

(defun nxhtml-form-tag-do-also()
  (insert "action=\"")
  (rngalt-validate)
  (let ((src (nxhtml-read-url nil nil nil "Action")))
    (insert src "\" "))
  )

(defconst nxhtml-complete-tag-do-also
  '(("a"
     (lambda()
       (insert " href=\"")
       (rngalt-validate)
       (insert (nxhtml-read-url t))
       (insert "\"")))
    ("form" nxhtml-form-tag-do-also)
    ("img" nxhtml-img-tag-do-also)
    ("input" nxhtml-input-tag-do-also)
    ("link" nxhtml-link-tag-do-also)
    ("script" nxhtml-script-tag-do-also)
    ("style" nxhtml-style-tag-do-also)
    ("meta" nxhtml-meta-tag-do-also)
    )
  "List of functions to call at tag completion.
Each element of the list have the form

  \(TAG-NAME TAG-FUN)

If `nxhtml-tag-do-also' is non-nil then TAG-FUN is called after
by `nxml-complete' (with the special setup of this function for
`nxhtml-mode') when completing a tag with the name TAG-NAME.

The list is handled as an association list, ie only the first
occurence of a tag name is used.")


(defun nxhtml-complete-tag-do-also(tag)
  ;; First required attributes:
  (let ((tagrec (assoc tag nxhtml-complete-tag-do-also)))
    (when tagrec
      (funcall (cadr tagrec))))
  )

(defun nxhtml-check-tag-do-also()
  (when nxhtml-tag-do-also
    (nxhtml-turn-onoff-tag-do-also t)))

(defun nxhtml-turn-onoff-tag-do-also(on)
  (add-hook 'nxhtml-mode-hook 'nxhtml-check-tag-do-also)
  (dolist (b (buffer-list))
    (when (with-current-buffer b
            (eq major-mode 'nxhtml-mode))
      (if on
          (progn
            (add-hook 'rngalt-complete-tag-hooks 'nxhtml-complete-tag-do-also t t)
            )
          (remove-hook 'rngalt-complete-tag-hooks 'nxhtml-complete-tag-do-also t)
        ))))

(define-toggle nxhtml-tag-do-also t
  "When completing tag names do some more if non-nil.
For some tag names additional things can be done at completion to
speed writing up.  For example for an <img ...> tag `nxhtml-mode'
can prompt for src attribute and add width and height attributes
if this attribute points to a local file.

You can add additional elisp code for completing to
`nxhtml-complete-tag-do-also'."
  :set (lambda (symbol value)
         (nxhtml-turn-onoff-tag-do-also value)
         (set-default symbol value))
  :group 'nxhtml)



(defun nxhtml-complete-first-try ()
  (when (= 0 (buffer-size))
    (nxhtml-empty-page-completion)))

(defun nxhtml-completing-read-tag (prompt
                                  table
                                  &optional predicate require-match
                                  initial-input hist def inherit-input-method)
  (popcmp-completing-read prompt
                          table
                          predicate require-match
                          initial-input hist def inherit-input-method
                          nxhtml-help-tag
                          nxhtml-tag-sets))

(defun nxhtml-add-required-to-attr-set(tag)
  (let ((missing (when tag
                   (rngalt-get-missing-required-attr
                    (nxthml-is-single-tag tag)))))
    (if (not missing)
        nxhtml-attr-sets
      (cons (cons "Required" missing)
            nxhtml-attr-sets))))

(defun nxhtml-get-tag-specific-attr-help(tag)
  (append (cdr (assoc tag nxhtml-help-attribute-name-tag)) nxhtml-help-attribute-name)
  )

(defconst nxhtml-in-start-tag-regex
;;(defconst rng-in-start-tag-name-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   ;; Not entirely correct since < could be part of attribute value:
   "<\\(w\\(?::w?\\)?\\)+ [^<]*"
   t
   t))

(defun nxhtml-completing-read-attribute-name (prompt
                                              table
                                              &optional predicate require-match
                                              initial-input hist def inherit-input-method)
  (let* ((tag (save-match-data
                ;;(when (looking-back "<\\([a-z1-6]+\\) [^<]*")
                (when (looking-back nxhtml-in-start-tag-regex)
                  (match-string 1))))
         (attr-sets (nxhtml-add-required-to-attr-set tag))
         (help-attr (nxhtml-get-tag-specific-attr-help tag))
         )
    (popcmp-completing-read prompt
                            table
                            predicate require-match
                            initial-input hist def inherit-input-method
                            help-attr
                            attr-sets)))

(defun nxhtml-completing-read-attribute-value (prompt
                                               table
                                               &optional predicate require-match
                                               initial-input hist def inherit-input-method)
  (let (val)
    (if table
        (setq val (popcmp-completing-read prompt table
                                          predicate require-match
                                          initial-input hist def inherit-input-method))
      (let* (init
             delimiter
             (lt-pos (save-excursion (search-backward "<" nil t)))
             (in-attr-val
              (save-excursion
                (re-search-backward rng-in-attribute-value-regex lt-pos t)))
             (in-xml-attr-val
              (unless in-attr-val
                (save-excursion
                  (re-search-backward nxhtml-in-xml-attribute-value-regex lt-pos t))))
         )
        (when (or in-attr-val in-xml-attr-val)
          ;;(save-match-data (save-excursion (re-search-forward "\\=[^<> \t\r\n\"]*" nil t)))
          (let* ((name-start (match-beginning 1))
                 (name-end (match-end 1))
                 (colon (match-beginning 2))
                 (attr (buffer-substring-no-properties name-start
                                                       (or colon name-end)))
                 (value-start (1+ (match-beginning 3)))
                 (tag (save-excursion
                        (when (search-backward-regexp "<[[:alpha:]]+" nil t)
                      (match-string 0)))))
            (setq init (buffer-substring-no-properties value-start (point)))
            (setq delimiter (char-before value-start))
            (if in-xml-attr-val
                (error "in-xml-attr-val should not be true here!")
              ;;             (cond ((string= "encoding" attr)
              ;;                    ;; Give a default that works in browsers today
              ;;                    (setq val (nxhtml-coding-systems-complete
              ;;                               init
              ;;                               (symbol-name nxhtml-default-encoding))))
              ;;                   ((string= "version" attr)
              ;;                    (setq val "1.0")))
              (cond ((string= "rel" attr)
                     (cond ((string= "<link" tag)
                            (setq val (nxhtml-read-link-rel))
                            )))
                    ((string= "media" attr)
                     (cond ((string= "<link" tag)
                            (setq val (nxhtml-read-link-media)))
                           ((string= "<style" tag)
                            (setq val (nxhtml-read-link-media)))
                           ))
                    ((string= "type" attr)
                     (cond ((string= "<link" tag)
                            (setq val (nxhtml-read-link-type))
                            )))
                    ((string= "http-equiv" attr)
                     (cond ((string= "<meta" tag)
                            (setq val (nxhtml-read-meta-http-equiv)))))
                    ((string= "content" attr)
                     (cond ((string= "<meta" tag)
                            (setq val (nxhtml-read-meta-content)))))
                    ((string= "scheme" attr)
                     (cond ((string= "<meta" tag)
                            (setq val (nxhtml-read-meta-scheme)))))
                    ((string= "name" attr)
                     (cond ((string= "<meta" tag)
                            (setq val (nxhtml-read-meta-name)))))
                    ((string= "href" attr)
                     (cond ((string= "<a" tag)
                            (setq val (nxhtml-read-url t init)))
                           ((string= "<base" tag)
                            (setq val (nxhtml-read-url nil init nil "Base")))
                           ((string= "<area" tag)
                            (setq val (nxhtml-read-url nil init)))
                           ((string= "<link" tag)
                            (setq val (nxhtml-read-url nil init)))
                           (t
                            (setq val (nxhtml-read-url nil init)))))
                    ((string= "src" attr)
                     (cond ((string= "<img" tag)
                            (setq val (nxhtml-read-url nil init 'nxhtml-image-url-predicate "Image")))
                           ((string= "<script" tag)
                            (setq val (nxhtml-read-url nil init 'nxhtml-script-url-predicate "Script")))
                           ((string= "<input" tag)
                            (setq val (nxhtml-read-url nil init 'nxhtml-image-url-predicate "Image")))
                           ((string= "<frame" tag)
                            (setq val (nxhtml-read-url nil init nil "Frame Source")))
                           ((string= "<iframe" tag)
                            (setq val (nxhtml-read-url nil init nil "Frame Source")))
                           (t
                            (setq val (nxhtml-read-url nil init)))))))))))
    ;;(unless val (setq val (read-from-minibuffer prompt init)))
    (if (not val)
        (progn
          (message "No completion of attribute value available here")
          nil)
      val)))

(defun nxhtml-read-link-type()
  (require 'mailcap)
  (let ((types (when (boundp 'mailcap-mime-extensions)
                 (mapcar (lambda (elt)
                           (cdr elt))
                         mailcap-mime-extensions))))
  (completing-read "Link type: " types nil t)))

(defun nxhtml-read-link-media()
  (let ((types '(
                 "screen"
                 "tty"
                 "tv"
                 "projection"
                 "handheld"
                 "print"
                 "braille"
                 "aural"
                 "all"
                 )))
    (popcmp-completing-read "For media type: " types nil t)))

(defun nxhtml-read-link-rel()
  (let ((predefined-linktypes '(
                               "Alternate"
                               "Appendix"
                               "Bookmark"
                               "Chapter"
                               "Contents"
                               "Copyright"
                               "Glossary"
                               "Help"
                               "Index"
                               "Next"
                               "Prev"
                               "Section"
                               "Shortcut Icon"
                               "Start"
                               "Stylesheet"
                               "Subsection"
                               )))
    (popcmp-completing-read "Predefined LinkTypes: " predefined-linktypes nil t)))

(defun nxhtml-read-meta-name()
  (let ((types '(
                 "author"
                 "description"
                 "keywords"
                 "generator"
                 "revised"
                 ;;"others"
                 )))
    (popcmp-completing-read "Meta name: " types nil t)))

(defun nxhtml-read-meta-content()
  (nxhtml-read-from-minibuffer "Meta content: "))

(defun nxhtml-read-meta-scheme()
  (nxhtml-read-from-minibuffer "Meta scheme: "))

(defun nxhtml-read-meta-http-equiv()
  (let ((types '(
                 "content-type"
                 "expires"
                 "refresh"
                 "set-cookie"
                 )))
    (popcmp-completing-read "Meta http-equiv: " types nil t)))

(when (featurep 'rngalt)
  (setq rngalt-completing-read-tag 'nxhtml-completing-read-tag)
  (setq rngalt-completing-read-attribute-name 'nxhtml-completing-read-attribute-name)
  (setq rngalt-completing-read-attribute-value 'nxhtml-completing-read-attribute-value)
  (setq rngalt-complete-first-try 'nxhtml-complete-first-try)
  (setq rngalt-complete-last-try 'nxhtml-complete-last-try)
  )
(when nil
  (setq rngalt-completing-read-tag nil)
  (setq rngalt-complete-last-try nil)
  )


(require 'typesetter nil t)
(when (featurep 'typesetter)
  (defun typesetter-init-nxhtml-mode()
    (typesetter-init-html-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Validation start state

(defcustom nxhtml-validation-headers
  '(
    ("body-iso-8859-1" .
     "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>rngalt-xhtml-header</title>
  </head>
  <body>
"
     )
    ("head-iso-8859-1" .
     "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
"
     )
    ("html-iso-8859-1" .
     "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
"
     )
;;     ("doctype-iso-8859-1" .
;;      "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
;; <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
;; \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
;; "
;;      )
;;     ("xml-iso-8859-1" .
;;      "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
;; "
;;      )

    ("body-utf-8" .
     "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>rngalt-xhtml-header</title>
  </head>
  <body>
"
     )
    ("head-utf-8" .
     "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
"
     )
    ("html-utf-8" .
     "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
"
     )
;;     ("doctype-utf-8" .
;;      "<?xml version=\"1.0\" encoding=\"utf-8\"?>
;; <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
;; \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
;; "
;;      )
;;     ("xml-utf-8" .
;;      "<?xml version=\"1.0\" encoding=\"utf-8\"?>
;; "
;;      )
    )
  "Extra XHTML validation headers.
Used by `nxhtml-set-validation-header'."
  :type '(alist :key-type string :value-type string)
  :group 'nxhtml)

(defcustom nxhtml-default-validation-header nil
  "Default extra XHTML validation header.
Must be nil or one of the key values in
`nxhtml-validation-headers'."
  :type 'string
  :set (lambda (sym val)
         (if (or (null val)
                 (assoc val nxhtml-validation-headers))
             (set-default sym val)
           (lwarn 'nxhtml-default-validation-header
                  :warning "There is no Extra XHTML Validation Header named %s" val)))
  :group 'nxhtml)

(defun nxhtml-must-have-validation-headers()
  (unless nxhtml-validation-headers
    (error
     "No XHTML validation headers. Please customize nxhtml-validation-headers.")))

(defvar nxhtml-set-validation-header-hist nil)

(defvar nxhtml-current-validation-header nil)
(make-variable-buffer-local 'nxhtml-current-validation-header)
(put 'nxhtml-current-validation-header 'permanent-local t)

(defcustom nxhtml-guess-validation-header-alist
  ;;(rx line-start (0+ blank) "<body")
  '(
    ("^[[:blank:]]*<body" . "body-utf-8")
    ("^[[:blank:]]*<head" . "head-utf-8")
    ("^[[:blank:]]*<html" . "html-utf-8")
    )
  "Alist used by `nxhtml-guess-validation-header'.
Alternatives are tried from top to bottom until one fits."
  :type '(alist :key-type (regexp :tag "If NOT found in buffer")
                :value-type (string :tag "Use Extra XHTML Validation Header"))
  :group 'nxhtml)

(defun nxhtml-guess-validation-header()
  "Return extra XHTML validation that could fit current buffer.
This guess is made by matching the entries in
`nxhtml-guess-validation-header-alist' against the buffer."
  (nxhtml-must-have-validation-headers)
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (let (rec
              regexp
              key
              (guesses nxhtml-guess-validation-header-alist))
          (goto-char (point-min))
          (if (not (search-forward "</" 2000 t))
              (progn
                (setq rec (car guesses))
                (setq key (cdr rec)))
            (while (and guesses
                        (not key))
              (setq rec (car guesses))
              (setq guesses (cdr guesses))
              (setq regexp (car rec))
              (goto-char (point-min))
              (unless (re-search-forward regexp 2000 t)
                (setq key (cdr rec)))))
          key)))))

(defun nxhtml-get-default-validation-header()
  "Return default extra XHTML validation header key for current buffer.
If `nxhtml-default-validation-header' is non-nil then return
this.  Otherwise return `nxhtml-guess-validation-header'."
  (or nxhtml-default-validation-header
      (nxhtml-guess-validation-header)))

(defun nxhtml-set-validation-header(&optional key)
  "Set an extra \(fictive) XHTML validation header in the buffer.
This header is not inserted in the buffer, but is only used by
validation and XHTML completion.

The header is active if and only if
`nxhtml-validation-header-mode' is on.

The header is choosen from `nxhtml-validation-headers'. If there
is more than one you will be prompted. To set the default extra
XHTML validation header customize `nxhtml-validation-headers'.

If called non-interactive then the header corresponding to key
KEY will be used.  If KEY is nil then it is set to
`nxhtml-default-validation-header'.

For more information see `rngalt-show-validation-header'."
  (interactive
   (list
    (let ((nh (length nxhtml-validation-headers))
          (default (nxhtml-get-default-validation-header)))
      (if (> nh 1)
          (completing-read "XHTML validation header: "
                           nxhtml-validation-headers
                           nil
                           t
                           default
                           nxhtml-set-validation-header-hist)
        (if (not (y-or-n-p) "Only one XHTML validation header is defined. Define more? ")
            default
          (customize-option 'nxhtml-validation-headers)
          'adding)))))
  ;;(lwarn 'svh2 :warning "key=%s" key)
  (or key
      (setq key (nxhtml-get-default-validation-header))
      (setq key (cons 'schema "XHTML")))
  (unless (eq key 'adding)
    (setq nxhtml-current-validation-header key)
    (nxhtml-validation-header-mode 1)
    (nxhtml-apply-validation-header)))

(defun nxhtml-apply-validation-header()
  (when nxhtml-current-validation-header
    (setq rngalt-major-mode
          (if mumamo-mode
              (mumamo-main-major-mode)
            major-mode))
    (let* ((key nxhtml-current-validation-header)
           (rec (unless (listp key)
                  (assoc key nxhtml-validation-headers)))
           (header (cdr rec)))
      (if (listp key)
          (let ((schema-file (rng-locate-schema-file (cdr key))))
            (unless schema-file
              (error "Could not locate schema for type id `%s'" type-id))
            (rng-set-schema-file-1 schema-file))
        ;; FIX-ME: I do not understand this, but there is an error at
        ;; loading this file, have to test here:
        ;;(when (functionp 'nxhtml-global-validation-header-mode-cmhh)
          (rngalt-set-validation-header header)
          ;;)
        ))))

(define-minor-mode nxhtml-validation-header-mode
  "If on use an extra XHTML validation header for the buffer.
See `nxhtml-set-validation-header' for further information."
  :global nil
  :lighter " VH"
  :group 'nxhtml
  (if nxhtml-validation-header-mode
      (progn
        (unless nxhtml-current-validation-header
          (setq nxhtml-current-validation-header
                (nxhtml-get-default-validation-header)))
        (nxhtml-apply-validation-header)
        (when (featurep 'mumamo)
          (add-hook 'mumamo-change-major-mode-hook 'nxhtml-vhm-mumamo-change-major nil t)
          (add-hook 'mumamo-after-change-major-mode-hook 'nxhtml-vhm-mumamo-after-change-major nil t))
        )
    (rngalt-set-validation-header nil)))

(defun nxhtml-vhm-mumamo-change-major()
  (put 'rngalt-validation-header 'permanent-local t)
  (put 'nxhtml-validation-header-mode 'permanent-local t)
  (put 'nxhtml-current-validation-header 'permanent-local t)
  (put 'nxhtml-validation-header-mode-major-mode 'permanent-local t)
  (setq nxhtml-validation-header-mode-major-mode mumamo-set-major-running)
  )

(defun nxhtml-vhm-mumamo-after-change-major()
  (put 'rngalt-validation-header 'permanent-local nil)
  (put 'nxhtml-validation-header-mode 'permanent-local nil)
  (put 'nxhtml-current-validation-header 'permanent-local nil)
  (put 'nxhtml-validation-header-mode-major-mode 'permanent-local nil)
  )

(defcustom nxhtml-validation-header-filename-alist
  '(
    ("\.php$" . nil)
    )
  "Alist for turning on `nxhtml-validation-mode'.
If buffer file name matches the an entry in the list then
`nxhtml-global-validation-header-mode' will turn on
`nxhtml-validation-header-mode' in buffer.

The car if each element is used as a regexp to match the file
name.  The second element is the name of the extra XHTML
validation header to use. If this is nil the default extra XHTML
validation header is used."
  :type '(alist :key-type string :value-type (choice (const :tag "Default" nil)
                                                     string))
  :group 'nxhtml)

(defun nxhtml-maybe-turn-on-validation-header()
  (or (and (or (and mumamo-mode
                    (eq (mumamo-main-major-mode) 'nxhtml-mode))
               (eq major-mode 'nxhtml-mode))
           rngalt-validation-header
           nxhtml-current-validation-header
           nxhtml-validation-header-mode
           (progn
             ;;(lwarn 'maybe :warning "quick, buffer=%s" (current-buffer))
             (nxhtml-validation-header-mode 1)
             t))
      (when (buffer-file-name)
        (unless (or ;;nxhtml-validation-header-mode
                 (minibufferp (current-buffer))
                 (string= " " (substring (buffer-name) 0 1))
                 (string= "*" (substring (buffer-name) 0 1))
                 )
          (save-match-data
            (when (catch 'turn-on
                    (dolist (rec nxhtml-validation-header-filename-alist)
                      (when (string-match (car rec) (buffer-file-name))
                        (throw 'turn-on t))))
              ;;(lwarn 't :warning "turn on %s, buffer=%s" major-mode (current-buffer))
              (nxhtml-validation-header-mode 1)))))))

;; FIXME: Problem with non-local rng-variables:
(defconst nxhtml-global-validation-header-mode2 nil
  "See function `nxhtml-global-validation-header-mode'.")
(defun nxhtml-global-validation-header-mode2(&optional dummy)
  "Not available in this version.
This is not available in this version due to some nasty problems
I discovered. As soon as I have domesticed those it will be put
back in."
  (interactive)
  (describe-function 'nxhtml-global-validation-header-mode)
  nil)

(define-globalized-minor-mode nxhtml-global-validation-header-mode
  nxhtml-validation-header-mode
  nxhtml-maybe-turn-on-validation-header
  :group 'nxhtml)
;; The problem with global minor modes:
(when (and nxhtml-global-validation-header-mode
           (not (boundp 'define-global-minor-mode-bug)))
  (nxhtml-global-validation-header-mode 1))

(defun nxhtml-warnings-are-visible()
  (get 'rng-error 'face))

(defun nxhtml-toggle-visible-warnings()
  (interactive)
  "Toggle the red underline on validation errors."
  (let ((face (get 'rng-error 'face)))
    (if face
        (put 'rng-error 'face nil)
      (put 'rng-error 'face 'rng-error-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bug corrections
(defun nxml-indent-line ()
  "Indent current line as XML."
  (let ((indent (nxml-compute-indent))
	(from-end (- (point-max) (point))))
    (when indent
      (beginning-of-line)
      (let ((bol (point)))
	(skip-chars-forward " \t")
        ;; There is a problem with some lines, try a quick fix:
        (when (and (= 0 indent)
                   (not (eq (char-after) ?<)))
          (save-excursion
            (save-match-data
              (when (re-search-backward "^<" nil t)
                (when (search-forward " ")
                  (setq indent (current-column))))))
          (when (= 0 indent)
            (setq indent nxml-child-indent)))
        ;; And sometimes nxml-compute-indent get very upset, check for
        ;; that:
        (let ((here (point)))
          (beginning-of-line 0)
          (back-to-indentation)
          (when (and (= indent (current-column))
                     (eq (char-after) ?\"))
            (setq indent 0))
          (goto-char here))
        (unless (= (current-column) indent)
          (delete-region bol (point))
          (indent-to indent)))
      (when (> (- (point-max) from-end) (point))
	(goto-char (- (point-max) from-end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'nxhtml)

;;; nxhtml.el ends here
