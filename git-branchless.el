;;; git-branchless.el -*- lexical-binding: t; -*-

(set-popup-rule! "*git smartlog*" :side 'left) w
(define-derived-mode git-smartlog-mode shell-mode
  :interactive nil)

;;; variables
(defvar git-branchless-proc "*git-branchless*")
;;; functions

(defun my/git-smartlog-get-ref ()
  "Get the ref for the commit at the current line.
Only works in the smartlog buffer."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "●\\|◯" (line-end-position) t)
    (forward-char)
    (word-at-point t)))

(defun my/git-smartlog-magit-log ()
  (interactive)
  (when-let ((ref (my/git-smartlog-get-ref)))
    (magit-log-setup-buffer (list ref) nil '())))

(defun my/git-smartlog-switch ()
  (interactive)
  (when-let ((ref (my/git-smartlog-get-ref)))
    (make-process :name "git-branchless"
                  :stderr "*git-branchless-err*"
                  :command (list "git" "branchless" "switch" ref))
    (my/git-smartlog)))

(defun my/git-smartlog ()
  (interactive)
  (let ((process (start-process "git smartlog" "*git smartlog*" "git" "branchless" "smartlog" "--color" "always")))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max)))
      (display-buffer (current-buffer))
      (shell-mode)
      (set-process-filter
       process
       #'comint-output-filter))))
