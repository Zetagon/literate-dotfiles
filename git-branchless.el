;;; git-branchless.el -*- lexical-binding: t; -*-

(set-popup-rule! "*git smartlog*" :side 'left) w

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
