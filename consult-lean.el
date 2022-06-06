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

(defvar consult-lean--current-buffer nil)

(defun helm-lean-definitions-format-candidate (c)
  `(,(format "%s : %s %s"
             (propertize (plist-get c :text) 'face font-lock-variable-name-face)
             (plist-get c :type)
             (propertize (plist-get (plist-get c :source) :file) 'face font-lock-comment-face))
    . ,c))

(defun consult-lean--definitions-builder (input)
  (with-current-buffer consult-lean--current-buffer
    (let* ((response (lean-server-send-synchronous-command 'search (list :query input)))
           (results (plist-get response :results))
           (results (-filter (lambda (c) (plist-get c :source)) results))
           (candidates (-map 'helm-lean-definitions-format-candidate results)))
      candidates)))

(defvar consult-lean--candidates nil)

(defun consult-lean--make-async-source (async)
  (lambda (action)
    (pcase-exhaustive action
      ('nil consult-lean--candidates)
      ;; ('setup (funcall async action))
      (stringp
       (when-let ((res (ignore-errors (consult-lean--definitions-builder action))))
         (setq consult-lean--candidates res))
       (funcall async action))
      (_ (funcall async action)))))

(defun consult-lean--lookup (selected candidates _input _narrow)
  (plist-get
   (cdr
         (car (seq-drop-while (lambda (x)
                                (not (string-equal selected
                                                   (substring-no-properties (car x)))))
                              candidates)))
   :source))

(defun consult-lean-definitions ()
  (interactive)
  (setq consult-lean--candidates nil)
  (let* ((consult-lean--current-buffer (current-buffer))
         (user-choice (consult--read
                       (lambda (action)
                         (pcase-exhaustive action
                           ('setup nil)
                           ('destroy nil)
                           ('flush nil)
                           ('refresh nil)
                           ('nil consult-lean--candidates)
                           (stringp
                            (when-let ((res (ignore-errors (consult-lean--definitions-builder action))))
                              (setq consult-lean--candidates res))
                            nil)))
                       :lookup #'consult-lean--lookup
                       :prompt "Definition: ")))
    (setq foo user-choice)
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
