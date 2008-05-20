;; Lennart Borgman wrote:

;;   Hi

;; > > I meant I want to get this information in elisp. Say I
;; > > have the tag name (like "img"). I then want to call some
;; > > elisp functions that can give me the information about
;; > > required and allowed attributes. I want to have this
;; > > information in the form of elisp data that I can use for
;; > > writing new functions.

;;   The name of an element is not enough to get the possible
;; attributes.  You have to provide the context too.  I don't
;; think nXML give you such a ready-to-use function, but you
;; can get inspiration from the following files:

;;     - rng-match.el
;;     - rng-pttrn.el
;;     - rng-valid.el

;;   For example, the following functions (quickly adapted from
;; functions from the above files) print the possible
;; attributes (NS + name) of the current element (under the
;; point):

(eval-when-compile
  (unless (featurep 'nxml-nxhtml-autostart)
    (let ((efn (expand-file-name "../autostart.el"))) (load efn))
    (require 'rng-valid)
    (require 'rng-nxml)))

(defun drkm-nxml:show-attributes ()
  (interactive)
  (with-output-to-temp-buffer "*Attributes*"
    (let ((all-attr (drkm-nxml:get-attributes))
          uniq)
      (mapc (lambda(attr)
              (add-to-list 'uniq attr))
            (sort all-attr
                  (lambda(a b)
                    (string< (cdr a) (cdr b)))))
      ;;(message "all-attr=%s" all-attr)(sit-for 4)
      ;;(message "uniq=%s" uniq)(sit-for 4)
      (dolist (attr uniq)
        (princ "{")
        (princ (or (car attr) ""))
        (princ "}")
        (princ (cdr attr))
        (terpri)))))

(defun drkm-nxml:get-attributes ()
  (and rng-validate-mode
       (let ((lt-pos (save-excursion (search-backward "<" nil t)))
             xmltok-dtd)
         (and lt-pos
              (= (rng-set-state-after lt-pos) lt-pos)
              (when (save-excursion
                      (re-search-backward rng-in-attribute-regex
                                          lt-pos t))
                (let ((attribute-start (match-beginning 1))
                      rng-undeclared-prefixes)
                  (and (rng-adjust-state-for-attribute lt-pos
                                                       attribute-start)
                       (drkm-rng:ipattern-possible-attributes
                        rng-match-state nil))))))))

(defun drkm-rng:ipattern-possible-attributes (ipattern accum)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond
     ((eq type 'after)
      (drkm-rng:ipattern-possible-attributes
       (rng-ipattern-get-child ipattern) accum))
     ((memq type '(
                   choice
                   interleave
                   group
                   ))
      (let ((members (rng-ipattern-get-child ipattern)))
        (while members
          (setq accum (drkm-rng:ipattern-possible-attributes
                            (car members) accum)
                members (cdr members))))
      accum)
     ((eq type 'attribute)
      (dolist (attr (cdr
                          (rng-ipattern-get-memo-map-start-attribute-deriv
                           ipattern)))
        (push (car attr) accum))
      accum)
     ((eq type 'one-or-more)
      (drkm-rng:ipattern-possible-attributes
            (rng-ipattern-get-child ipattern)
            accum))
     (t accum))))

;;   Please note I don't know very well the nXML code, or
;; neither the data model of rng-pttrn.el.  So take these
;; functions with care.  Just to show you that nXML provides
;; all necessary tools and information, but you'll maybe need
;; to learn where and how to retreive this information.

;;   Regards,

;; --drkm
