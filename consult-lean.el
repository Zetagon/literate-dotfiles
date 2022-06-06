;;; consult-lean.el --- Consult interfaces for lean-mode -*- lexical-binding: t -*-

;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.

;; Author: Leonardo de Moura <leonardo@microsoft.com>
;;         Soonho Kong       <soonhok@cs.cmu.edu>
;;         Gabriel Ebner     <gebner@gebner.org>
;;         Sebastian Ullrich <sebasti@nullri.ch>
;; Maintainer: Sebastian Ullrich <sebasti@nullri.ch>
;; Created: Jan 09, 2014
;; Keywords: languages
;; Package-Requires: ((emacs "25.1") (dash "2.18.0") (helm "2.8.0") (lean-mode "3.3.0"))
;; URL: https://github.com/leanprover/lean-mode

;; Released under Apache 2.0 license as described in the file LICENSE.

;;; Commentary:

;; Currently provides an interface for looking up Lean definitions by name

;;; Code:

(require 'dash)
(require 'lean-server)

(defcustom consult-lean-keybinding-helm-lean-definitions (kbd "C-c C-d")
  "Lean Keybinding for helm-lean-definitions"
  :group 'lean-keybinding :type 'key-sequence)

(defun helm-lean-definitions-format-candidate (c)
  `(,(format "%s : %s %s"
             (propertize (plist-get c :text) 'face font-lock-variable-name-face)
             (plist-get c :type)
             (propertize (plist-get (plist-get c :source) :file) 'face font-lock-comment-face))
    . ,c))

(defun consult-lean--definitions-builder (input buffer)
  (with-current-buffer buffer
    (let* ((response (lean-server-send-synchronous-command 'search (list :query input)))
           (results (plist-get response :results))
           (results (-filter (lambda (c) (plist-get c :source)) results))
           (candidates (-map 'helm-lean-definitions-format-candidate results)))
      candidates)))

(defun consult-lean--make-async-source (async buffer)
  (lambda (action)
    (pcase-exhaustive action
      ('nil (funcall async action))
      (stringp
       (when-let ((res (and (not (string-empty-p action))
                            (ignore-errors (consult-lean--definitions-builder action buffer)))))
         (funcall async 'flush)
         (funcall async res))
       (funcall async action))
      (_ (funcall async action)))))

(defun consult-lean--lookup (selected candidates input _narrow)
  (plist-get
   (cdr
         (car (seq-drop-while (lambda (x)
                                (not (string-equal selected
                                                   (substring-no-properties (car x)))))
                              candidates)))
   :source))

(defun consult-lean-definitions ()
  (interactive)
  (let ((user-choice (consult--read
                      (thread-first (consult--async-sink)
                                    (consult--async-refresh-immediate)
                                    (consult-lean--make-async-source (current-buffer))
                                    ;; causes `candidate' in
                                    ;; `consult-lean--lookup' to not be
                                    ;; updated.  It is unchanged after the
                                    ;; command is first launched.
                                    (consult--async-throttle)
                                    (consult--async-split))
                      :lookup #'consult-lean--lookup
                      :prompt "Definition: ")))
    (apply 'lean-find-definition-cont
           user-choice)))

;;;###autoload
(defun helm-lean-hook ()
  "Set up helm-lean for current buffer"
  (local-set-key consult-lean-keybinding-helm-lean-definitions #'consult-lean-definitions))

;;;###autoload
(add-hook 'lean-mode-hook #'helm-lean-hook)

(provide 'consult-lean)
;;; helm-lean.el ends here
