;;; appmenu.el --- A framework for [apps] popup menus.

;; Copyright (C) 2005 by Lennart Borgman

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: Thu Jan 05 14:00:26 2006
(defconst appmenu:version "0.52") ;; Version:
;; Last-Updated: Thu Mar 08 02:29:52 2007 (3600 +0100)
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is maybe a somewhat preliminary version!
;;
;;  appmenu.el is a framework for creating cooperative context
;;  sensitive popup menus with commands from different major and minor
;;  modes. For more information see `appmenu-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defcustom appmenu-items-visible nil
  "Non-nil means show AppMenu help on AppMenu popup."
  :type 'boolean
  :group 'appmenu)

(defcustom appmenu-major-mode nil
  "Non-nil means show major mode on AppMenu popup."
  :type 'boolean
  :group 'appmenu)

(defcustom appmenu-minor-modes nil
  "Non-nil means show minor modes on AppMenu popup."
  :type 'boolean
  :group 'appmenu)

(defcustom appmenu-minor-modes-exclude nil
  "List of minor modes that should not be shown on AppMenu popup.
Applied only if `appmenu-minor-modes' is non-nil."
  :set (lambda (symbol value)
         (dolist (v value)
           (unless (memq v minor-mode-list)
             (lwarn '(appmenu-minor-modes-exclude) :error "Not a minor mode")))
         (set-default symbol value))
  :type '(repeat
          (symbol
           :tag "Minor mode"))
  :group 'appmenu)

(defcustom appmenu-minor-modes-include nil
  "List of minor modes that should be shown on AppMenu popup.
Applied only if `appmenu-minor-modes' is nil."
  :set (lambda (symbol value)
         (dolist (v value)
           (unless (memq v minor-mode-list)
             (lwarn '(appmenu-minor-modes-include) :error "Not a minor mode")))
         (set-default symbol value))
  :type '(repeat
          (symbol
           :tag "Minor mode"))
  :group 'appmenu)


;; These are for other elisp modules to use:
(defvar appmenu-always-alist nil
  "List of menu keymaps to always show.")

(defvar appmenu-modes-alist nil
  "Alist of additional menu keymaps for modes.
This is in addition to the normal major/minor mode menu map.  The
entries in this list are cons cells:

   (MODE . (TITLE . DEFINITION))

MODE should be the mode symbol. If it is t the entry is shown for
all modes.  DEFINITION should be either a keymap or a function
that returns a keymap.

The function must take no argument and return a keymap.  If the
function returns nil then the entry is not shown in the popup
menu.  Using this you can make context sensitive popup menus.

For an example of use see `nxhtml-add-appmenu' in nxhtml.el.")



(defun appmenu-help()
  (interactive)
  (describe-function 'appmenu-mode))

(defun appmenu-mode-in-list(mode mode-list)
  "For checkin the include and include lists."
  ;; First test simple entry
  (if (memq mode mode-list)
      t
    ;; Look for function entry
    (let (found)
      (dolist (entry mode-list)
        (when (eq mode (car entry))
          (setq found (funcall (nth 1 entry)))))
      found)))

(defun appmenu-map()
  (let ((map (make-sparse-keymap
              ;;"AppMenu"
              ))
        (num-minor 0))
    ;; AppMenu itself
    (when appmenu-items-visible
      (define-key map [appmenu-customize]
        (list 'menu-item "Customize AppMenu"
              (lambda() (interactive) (customize-group 'appmenu))
              :help "Customize AppMenu"
              :visible 'appmenu-items-visible))
      (define-key map [appmenu-help]
        (list 'menu-item "Help for AppMenu" 'appmenu-help
              :help "Help for how to use AppMenu"
              :visible 'appmenu-items-visible))
      (define-key map [appmenu-separator-1]
        (list 'menu-item "--")))
    ;; Minor modes
    (dolist (mm minor-mode-map-alist)
      (let ((mode (car mm)))
        (when (symbol-value mode)
          (let ((minor-extra (assoc mode appmenu-modes-alist)))
            (when minor-extra
              ;;(message "me=%s" minor-extra)(sit-for 4)
              (let* ((extra (cdr minor-extra))
                     (title (car extra))
                     (emap (cdr extra)))
                ;;(message "t=%s, fe=%s, e=%s" title (functionp emap) emap)(sit-for 4)
                (when (functionp emap) (setq emap (funcall emap)))
                (when emap
                  (define-key map
                    (read (format "[appmenu-minor-extra-%s]" mode))
                    ;;(vector intern (format "appmenu-minor-extra-%s" mode))
                    (list 'menu-item
                          title
                          emap))))))
          (when (or (and appmenu-minor-modes
                         ;;(not (memq mode appmenu-minor-modes-exclude)))
                         (not (appmenu-mode-in-list mode appmenu-minor-modes-exclude)))
                    (and (not appmenu-minor-modes)
                         ;;(memq mode appmenu-minor-modes-include)))
                         (appmenu-mode-in-list mode appmenu-minor-modes-include)))
            (let ((minor-map (cdr mm)))
              (let ((menu-bar-entry (lookup-key minor-map [menu-bar])))
                (when menu-bar-entry
                  (map-keymap
                   (lambda(key command)
                     (setq num-minor (1+ num-minor))
                     (define-key map
                       (vector (intern (concat "appmenu-minor-" (symbol-name key))))
                       command))
                   menu-bar-entry))))))))
    (when (< 0 num-minor)
      (define-key map [appmenu-header-minor]
        (list 'menu-item "Minor Modes"))
      (define-key map [appmenu-separator-major]
        (list 'menu-item "--")))
    ;; Major mode
    (when appmenu-major-mode
      (let ((major-map (symbol-value (intern-soft (format "%s-map" major-mode))))
            (major-extra (assoc major-mode appmenu-modes-alist))
            (some-major nil))
        (when major-extra
          (setq some-major t)
          (let* ((extra (cdr major-extra))
                 (title (car extra))
                 (emap (cdr extra)))
            (when (functionp emap) (setq emap (funcall emap)))
            (when emap
              (define-key map [appmenu-major-extra]
                (list 'menu-item
                      title
                      emap)))))
        (when major-map
          (let ((menu-bar-entry (lookup-key major-map [menu-bar])))
            (when menu-bar-entry
              (map-keymap
               (lambda(key command)
                 (setq some-major t)
                 (define-key map
                   (vector (intern (concat "appmenu-major-" (symbol-name key))))
                   command))
               menu-bar-entry))))
        (when some-major
          (define-key map [appmenu-header-major]
            (list 'menu-item "Major Mode")))))
    ;; Always
    (when appmenu-always-alist
      (define-key map [appmenu-header-always]
        (list 'menu-item "--")))
    (let ((ia 0))
      (dolist (am appmenu-always-alist)
        (let ((am-tit (car am))
              (am-def (cdr am)))
          (when (functionp am-def)
            (setq am-def (funcall am-def)))
          (when am-def
            (setq ia (1+ ia))
            (define-key map
              (vector (intern (format "appmenu-always-%s" ia)))
              (list 'menu-item am-tit am-def))))))
    map))

(eval-when-compile (require 'cl))
(defun appmenu-get-submenu(menu-command)
  (let (subtitle submenumap)
    (if (eq 'menu-item (car menu-command))
        (progn (setq subtitle   (cadr  menu-command))
               (setq submenumap (caddr menu-command)))
      (setq subtitle   (car menu-command))
      (setq submenumap (cdr menu-command)))
    (unless (keymapp submenumap) (error "submenu not a keymap=%s" submenumap))
    (cons subtitle submenumap)))

(defun appmenu-popup()
  "Pops up the AppMenu menu."
  (interactive)
  (let* ((mod (event-modifiers last-input-event))
         (is-mouse (or (memq 'click mod)
                       (memq 'down  mod)
                       (memq 'drag  mod))))
    (when is-mouse
      (goto-char (posn-point (event-start last-input-event)))
      (sit-for 0.01))
    (let ((where (appmenu-point-to-coord (point))))
      (if (active-minibuffer-window)
          (describe-function last-command)
        (condition-case err
            (progn
              (popup-menu (appmenu-map) where)
              t)
          (quit nil))))))

(defun appmenu-point-to-coord(point)
  (let* ((pn (posn-at-point point))
	 (x-y (posn-x-y pn))
	 (x (car x-y))
	 (y (cdr x-y))
	 (pos (list (list x (+ y 20)) (selected-window))))
    pos))

(defvar appmenu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [apps] 'appmenu-popup)
    (define-key map [mouse-3] 'appmenu-popup)
    map))

(define-minor-mode appmenu-mode
  "Use a context sensitive popup menu.
AppMenu (appmenu.el) is a framework for creating cooperative
context sensitive popup menus with commands from different major
and minor modes. Using this different modes may cooperate about
the use of popup menus.

By default the popup menu is on [apps] and [mouse-3].

The entries from [menu-bar] for the major mode and activated
minor modes are added to the popup menu, but this can be
customized.

The defvar `appmenu-modes-alist' can be used to add more entries
for different modes. If you want entries that are available in all
modes you can use `appmenu-always-alist'.

You can write functions that use whatever information you want in
Emacs to construct these entries. Since this information is only
collected when the popup menu is shown you do not have to care as
much about computation time as for entries in the menu bar."
  :global t
  :keymap appmenu-mode-map)
(when (and appmenu-mode
           (not (boundp 'define-globa-minor-mode-bug)))
  (appmenu-mode 1))

(provide 'appmenu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appmenu.el ends here
