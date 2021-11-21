;;; ox-pollen.el -*- lexical-binding: t; -*-

 ;; (org-export-define-derived-backend 'pollen 'man
 ;;  :translate-alist
 ;;  '((bold . org-export-pollen-bold)
 ;;    (code . org-export-pollen-code)
 ;;    (headline . org-export-pollen-headline)
 ;;    (italic . org-export-pollen-italic)
 ;;    (plain-text . (lambda (content _)
 ;;                    content))    ))
(defmacro org-export-pollen-make-generic (name)
  `(lambda (_ contents _info)
    (format  "◊%s{%s}" ,name contents)))

(org-export-pollen-make-generic "paragraph")
(org-export-define-backend 'pollen
  `((bold . ,(org-export-pollen-make-generic "bold"))
    (center-block . (lambda (_ contents _)
                      (format "◊center{%s}" contents)))
    ;; (clock)
    ;; (code)
    (dynamic-block . ,(org-export-pollen-make-generic "dynamic-block"))
    (entity . ,(org-export-pollen-make-generic "entity"))
    (example-block . (lambda (example-block _contents info)
                       "◊example-block{%s}" (org-export-format-code-default example-block info)))
    ;; (export-block)
    ;; (export-snippet)
    (fixed-width . ,(org-export-pollen-make-generic "fixed-width"))
    (footnote-reference . org-export-pollen-footnote-reference)
    (headline . org-export-pollen-headline)
    (horizontal-rule . (lambda (_ _ _) "◊|horizontal-rule|"))
    (inline-src-block . org-export-pollen-inline-src-block)
    ;; (inline-task)
    ;; (inner-template . org-ascii-inner-template)
    (italic . ,(org-export-pollen-make-generic "italic"))
    ;; item
    (keyword . org-export-pollen-keyword)
    ;; latex-environment
    ;; latex-fragment
    (line-break . (lambda (&rest) "◊|line-break|"))
    (link . org-export-pollen-link)
    (node-property . org-org-export-node-property)
    (paragraph . (lambda (_paragraph contents  _info)
                   (format "◊p{%s}" contents)))
    ;; plain list
    (plain-text . (lambda (content _)
                    content))
    ;; planning
    (property-drawer . org-export-pollen-property-drawer)
    (quote-block . ,(org-export-pollen-make-generic "quote-block"))
    ;; radio target
    ;; (section . ,(org-export-pollen-make-generic "section"))
    (section . (lambda (_section contents _info) contents))
    (special-block (lambda (_special-block contents _info) contents))
    (statistics-cookie . org-export-pollen-statistics-cookie)
    (strike-through . ,(org-export-pollen-make-generic "strike-through"))
    (subscript . ,(org-export-pollen-make-generic "subscript"))
    (superscript . ,(org-export-pollen-make-generic "superscript"))
    (src-block . org-export-pollen-code)
    ;; table
    ;; table-cell
    ;; table-row
    (target . org-export-pollen-target)
    (timestamp . (lambda (timestamp _contents _info)
                   "◊timestamp{%s}" timestamp))
    (template . (lambda (contents info)
                  (format "#lang pollen\n%s" contents)))
    (underline . ,(org-export-pollen-make-generic "underline"))
    (verbatim . ,(org-export-pollen-make-generic "verbatim"))
    (verse-block . ,(org-export-pollen-make-generic "verse-block"))
    :menu-entry
    '(?x "Export to Pollen"
         ((?P "As pollen buffer"
              (lambda (a s v b)
                (org-export-to-buffer 'pollen "*pollen buffer exports*")))))))

(defun org-export-pollen-bold (bold contents info)
  (format "◊bold{%s}" contents))

(defun org-export-pollen-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to Pollen.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; (format "◊footnote-reference[%s]" (org-export-get-footnote-number footnote-reference info))
  (format "◊footnote-reference[\"%s\"]{%s}"
          (number-to-string  (org-export-get-footnote-number footnote-reference info))
          (org-trim (org-export-data (org-export-get-footnote-definition footnote-reference info) info))))

(defun org-export-pollen-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Pollen.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "◊inline-src-block[%s]{%s}"
	  (org-element-property :language inline-src-block)
          (org-element-property :value inline-src-block)))

(defun org-export-pollen-italic (bold contents info)
  (format "◊italic{%s}" contents))

(defun org-export-pollen-keyword(keyword _contents _info)
  (format "◊keyword[%s]{%s}"
          (org-element-property :key keyword)
          (org-element-property :value keyword)))

(defun org-export-pollen-link (link desc _info)
  "Transcode a LINK object from Org to Pollen.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  ;; TODO: Not finished
  (format "◊keyword[%s]{%s}"
          (org-element-property :raw-link keyword)
          desc))

(defun org-org-export-node-property (node-property _contents _info)
  "Transcode a NODE-PROPERTY element from Org to Pollen.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))
(defun org-export-pollen-property-drawer (property-drawer contents _info)
  (and (org-string-nw-p contents)
       (format "◊property-drawer{%s}" contents)))

(defun org-export-pollen-code (code contents info)
  (format "◊code[%s]{%s}" (org-element-property :language code) (org-element-property :value code)))

(defun org-export-pollen-headline (headline contents info)
  (let ((level (org-export-get-relative-level headline info)))
    (unless (org-element-property :footnote-section-p headline)
        (format "◊h%s{%s}\n%s"
                level
                (plist-get (cl-second headline) :raw-value)
                contents))))
(defun org-export-pollen-statistics-cookie (statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object from Org to Pollen.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))

(defun org-export-pollen-target (target _contents info)
  "Transcode a TARGET object from Org to Pollen.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "◊{%s}" (org-export-get-reference target info)))
