;;; git-branchless.el -*- lexical-binding: t; -*-

(define-derived-mode git-smartlog-mode shell-mode
  :interactive nil)

;;; variables
(defvar git-branchless-proc "*git-branchless*")

(defvar git-branchless-smartlog-buffer "*git smartlog*")

(defvar-local git-branchless-move-source-marker nil)
;;; functions

(defun git-branchless-smartlog-get-ref ()
  "Get the ref for the commit at the current line.
Only works in the smartlog buffer."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "◆\\|●\\|◯" (line-end-position) t)
    (forward-char)
    (word-at-point t)))

(defun git-branchless-smartlog-magit-log ()
  (interactive)
  (when-let ((ref (and (equal major-mode 'git-smartlog-mode)
                       (git-branchless-smartlog-get-ref))))
    (magit-log-setup-buffer (list ref) nil '())))

(defun git-branchless-smartlog-magit-show-commit ()
  (interactive)
  (magit-show-commit (git-branchless-smartlog-get-ref)))

(defun git-branchless-smartlog-switch ()
  (interactive)
  (when-let ((ref (and (equal major-mode 'git-smartlog-mode)
                       (git-branchless-smartlog-get-ref))))
    (make-process :name "git-branchless"
                  :stderr git-branchless-proc
                  :command (list "git" "branchless" "switch" ref))
    (git-branchless-smartlog)))

(defun git-branchless-move ()
  "WIP not tested yet"
  (interactive)
  (when (equal major-mode 'git-smartlog-mode)
    (if-let ((src-marker git-branchless-move-source-marker))
        (let ((src-ref (save-excursion
                         (goto-char src-marker)
                         (git-branchless-smartlog-get-ref)))
              (dst-ref (git-branchless-smartlog-get-ref)))
          (when (y-or-n-p (format "Run 'git move -s %s -d %s' ?" src-ref dst-ref))
            (call-process "git" nil git-branchless-proc nil
                          "move" "-s" src-ref "-d" dst-ref)))
      (setq git-branchless-move-source-marker (point-marker)))))

(defun git-branchless-prev ()
  (interactive)
  (call-process "git" nil git-branchless-proc nil
                "prev")
  (git-branchless-smartlog))

(defun git-branchless-next ()
  (interactive)
  (call-process "git" nil git-branchless-proc nil
                "next")
  (git-branchless-smartlog))

(defun git-branchless-restack ()
  (interactive)
  (call-process "git" nil git-branchless-proc nil
                "restack")
  (git-branchless-smartlog))

(defun git-branchless-sync ()
  (interactive)
  (call-process "git" nil git-branchless-proc nil
                "sync")
  (git-branchless-smartlog))


(defun git-branchless-smartlog ()
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
