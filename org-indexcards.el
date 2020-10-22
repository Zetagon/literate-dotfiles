;;; org-indexcards.el -*- lexical-binding: t; -*-

(defvar org-indexcards-screens nil
  "Loaded screens")
(setq org-indexcards-screens nil)

(defun org-indexcards-bsp-split ()
  "Split the largest window in the direction it is widest in.
Select the new window and switch to the buffer that was selected before executing command"
  (interactive)
  (let ((current-buf (current-buffer))
        (largest-window (--max-by (> (max (window-pixel-width it) (window-pixel-height it))
                                     (max (window-pixel-width other) (window-pixel-height other)))
                                  (window-list))))
    (select-window largest-window)
    ;; Select the new window
    (select-window
     (if (> (window-pixel-height largest-window)
            (window-pixel-width largest-window))
         (split-window-vertically)
       (split-window-horizontally)))
    (switch-to-buffer current-buf)))

(defun org-indexcards-add-screen ()
  "Add all current org-mode buffers as a screen with name NAME."
  (interactive)
  (let ((name (ivy-read "Name: " (-map #'car org-indexcards-screens)))
        screen)
    (dolist (window (window-list))
      (with-current-buffer (window-buffer window)
        (when (eq major-mode
                  'org-mode)
          (push (buffer-file-name) screen))))
    (setq org-indexcards-screens
          (a-assoc org-indexcards-screens
                                name screen))))

(defun org-indexcards-apply-screen ()
  "Open all files in screen"
  (interactive)
  (let* ((name (ivy-read "Select Screen"
                        (-map #'car org-indexcards-screens)))
        (screen (alist-get name org-indexcards-screens nil nil #'string-equal)))
    (find-file (car screen))
    (dolist (file (cdr screen))
      (org-indexcards-bsp-split)
      (find-file file))))

(defvar org-indexcards-screens-path "~/.emacs.d/org-indexcards-screens"
  "The path for where to store screens.")

(defun org-indexcards-load-screens ()
  "Load a list of screens from disk."
  (interactive)
  (setq org-indexcards-screens
        (with-temp-buffer
          (insert-file-contents org-indexcards-screens-path)
          (read (current-buffer)))))

(defun org-indexcards-save-screens ()
  "Save loaded screens to disk."
  (interactive)
  (with-temp-file org-indexcards-screens-path
    (prin1 org-indexcards-screens (current-buffer))))
