;;; git-branchless.el -*- lexical-binding: t; -*-

(set-popup-rule! "*git smartlog*" :side 'left) w
(define-derived-mode git-smartlog-mode shell-mode
  :interactive nil)

;;; variables
(defvar git-branchless-proc "*git-branchless*")
(defvar git-branchless-smartlog-buffer "*git smartlog*")
;;; functions

(defun my/git-smartlog-get-ref ()
  "Get the ref for the commit at the current line.
Only works in the smartlog buffer."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "◆\\|●\\|◯" (line-end-position) t)
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
                  :stderr git-branchless-proc
                  :command (list "git" "branchless" "switch" ref))
    (my/git-smartlog)))

(defun my/git-smartlog ()
  (interactive)
  (let ((inhibit-read-only t)
        (dir default-directory))
    (with-current-buffer (get-buffer-create git-branchless-smartlog-buffer)
      (delete-region (point-min) (point-max))
      (setq-local default-directory dir)
      (let ((process (start-process "git smartlog" git-branchless-smartlog-buffer "git" "branchless" "smartlog" "--color" "always")))
        (set-process-sentinel process
                              (lambda (process _event)
                                (with-current-buffer (process-buffer process)
                                  (goto-char (point-min))
                                  (search-forward "●" nil t))))
        (set-process-filter process #'comint-output-filter))
      (git-smartlog-mode)
      (view-mode)
      (display-buffer (current-buffer)))))
