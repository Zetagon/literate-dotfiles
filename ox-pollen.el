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
  `(lambda (_ contents _)
    (format  "◊%s{%s}" ,name contents)))

(org-export-pollen-make-generic "paragraph")
(org-export-define-backend 'pollen
  `((bold . ,(org-export-pollen-make-generic "bold"))
    (center-block . (lambda (_ contents _)
                      (format "◊center{%s}")))
    ;; (clock)
    ;; (code)
    (dynamic-block . ,(org-export-pollen-make-generic "dynamic-block"))
    (entity . ,(org-export-pollen-make-generic "entity"))
    (example-block . ,(org-export-pollen-make-generic "example-block"))
    ;; (export-block)
    ;; (export-snippet)
    (fixed-width . ,(org-export-pollen-make-generic "fixed-width"))
    (footnote-reference . org-export-pollen-footnote-reference)
    (headline . org-export-pollen-headline)
    (horizontal-rule . (lambda (_ _ _) "◊|horizontal-rule|"))
    (src-block . org-export-pollen-code)
    (italic . ,(org-export-pollen-make-generic "italic"))
    (plain-text . (lambda (content _)
                    content))
    (section . ,(org-export-pollen-make-generic "section"))
    (paragraph . (lambda (_ content  _)
                   content))
    :menu-entry
    '(?p "Export to Pollen"
         ((?P "As pollen buffer"
              (lambda (a s v b)
                (org-export-to-buffer 'pollen "*pollen buffer exports*")))))))

(defun org-export-pollen-bold (bold contents info)
  (format "◊bold{%s}" contents))

(defun org-export-pollen-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to Pollen.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "◊footnote-reference[%s]" (org-export-get-footnote-number footnote-reference info)))

(defun org-export-pollen-italic (bold contents info)
  (format "◊italic{%s}" contents))
(defun org-export-pollen-code (code contents info)
  (format "◊code[%s]{%s}" (org-element-property :language code) (org-element-property :value code)))

(defun org-export-pollen-headline (headline contents info)
  (let ((level (org-export-get-relative-level headline info)))
    (format "◊h[%s]{%s}\n%s"
            level
            (plist-get (cl-second headline) :raw-value)
            contents)))
