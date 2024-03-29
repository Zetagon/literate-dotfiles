;;; export-books.el -*- lexical-binding: t; -*-

(defvar my/book-file "~/Documents/notes/books.org")
(defvar my/book-export-file "~/Documents/blog/content/books.md")

(defun my/export-book-list ()
  (interactive)
  (let ((headings (org-ql-select my/book-file '(and (todo "TODO" "DONE")
                                                    (ancestors (heading "Books")))))
        (export-buffer (find-file-noselect my/book-export-file)))
    (with-current-buffer export-buffer
      (delete-region (point-min) (point-max))
      (insert "
+++
title = \"My book list\"
usedate = false
+++
")
      (insert "

Here is a table of books I've read or am planning on reading.
Checkmarks means I've read it.
If a book has multiple quality ratings it means it's somewhere inbetween.
If books are listed twice it's most likely because I read it again.")

      (insert "
<style>
th {
    border-bottom: 1px solid;
}
td{
    border-left: 1px solid;
    text-align: center;
    border-bottom: 1px solid #c4c4c4;
}

td:empty, th:empty {
  background: #6663;
}
</style>
")
      (insert "|  | Author  |  Title  | Rating  |
|--|---|---|---|
")
      (dolist (heading headings)
        (let* (;; If level = 3 it's a part of a series
               (level (org-element-property :level heading))
               (md-heading-marker (make-string (+ level 1)
                                               ?\#))
               (title (org-element-property :raw-value heading))
               (author (org-element-property :AUTHOR heading))
               (todo-type (org-element-property :todo-type heading))
               (tags (mapcar #'org-no-properties (org-element-property :tags heading))))
          (when (or (= level 2)
                    ;; Only put completed works of a series
                    (equal todo-type 'done))
            (insert "| " (if (equal todo-type 'todo)
                             "☐"
                           "🗹")
                    " | " (or (when (>= level 3) "└─────►") author "") " | " (or title "")  " | "
                    (or (string-join tags " ") "") " |\n"))
          ;; (insert (concat (apply #'concat md-heading-marker " " title " " author " "
          ;;                        tags) "\n"))
          ))
      (save-buffer))))


(defun my/region-to-author-property ()
  (interactive)
  (let ((author (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (org-set-property "author" author)))
