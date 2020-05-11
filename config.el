(map!
 :n "<f1>" #'=notmuch
 (:map notmuch-show-mode-map
   :n "D" #'evil-collection-notmuch-show-toggle-delete

   :n "d" (λ! (notmuch-show-tag-all '("-notdone"))
              (notmuch-show-next-thread-show)))
 (:map notmuch-tree-mode-map
  :n "d" (λ! (notmuch-tree-tag-thread '("-notdone")))))

(setq mail-envelope-from 'header)
(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from 'header)

(after! notmuch
  ;;This notmuch config is made for the doom module
  ;;
  ;;I need this line to be able to link to notmuch mails
  (require 'ol-notmuch)
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:notdone AND NOT tag:deleted NOT tag:gmail/Inbox" :key "i")
          (:name "inbox Ordförande" :query "tag:notdone AND NOT tag:deleted AND tag:gmail/Inbox" :key "o")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "sent" :query "tag:sent" :key "t")
          (:name "drafts" :query "tag:draft" :key "d")
          (:name "all mail" :query "*" :key "a")))
  
  (setq +notmuch-mail-folder "~/.mail")
  (setq +notmuch-sync-backend 'mbsync)
  (setq sendmail-program "/usr/bin/msmtp")
  )

(map!
;; I need these maps to be able to use jk in the agenda buffer properly
:n "k" #'evil-previous-visual-line
:n "j" #'evil-next-visual-line
:n "D" nil
(:prefix "D"
:n "d" #'avy-kill-ring-save-whole-line
:n "r" #'avy-kill-ring-save-region)
(:map org-super-agenda-header-map
 :map org-super-agenda-header-map
 "j" #'org-agenda-next-line
 "k" #'org-agenda-previous-line
 :map org-agenda-keymap
 :map org-agenda-mode-map
 "k" #'org-agenda-previous-line
 "j" #'org-agenda-next-line)
(:leader
:desc "Today" "ot" #'my/today
:desc "Create schedule for today" "ost" #'my/create-schedule-for-today
:desc "Create schedule for this week" "osw" #'my/create-schedule-for-week
:desc "Create schedule for this month" "osm" #'my/create-schedule-for-month
 )
 )

:n "k" #'evil-previous-visual-line
:n "j" #'evil-next-visual-line

:n "D" nil
(:prefix "D"
:n "d" #'avy-kill-ring-save-whole-line
:n "r" #'avy-kill-ring-save-region)

(use-package! org-ql
  :defer t)
(use-package! org-super-agenda
  :config (org-super-agenda-mode))
(after! org
  (use-package! org-ref
    :defer t
    :config
    (setq org-ref-notes-directory "~/Documents/notes"
          org-ref-bibliography-notes "~/Dropbox/org/references/articles.org"
          org-ref-default-bibliography '("~/Dropbox/org/references/Zotero_articles.bib")
          org-ref-pdf-directory "~/Dropbox/org/references/pdfs/"
          org-latex-pdf-process '("latexmk -shell-escape -bibtex -pdf %f")
          org-ref-default-ref-type "cref"
          org-ref-default-citation-link "autocite"
          org-latex-prefer-user-labels t
          org-footnote-auto-label 'confirm
          org-ref-ref-types '("cref" "Cref" "ref" "eqref" "pageref" "nameref" "autoref"))
    (map!
     (:leader
       :n "nb" #'org-ref-open-bibtex-notes)
     (:map org-mode-map
       (:localleader
         (:prefix (";" . "Org-ref")
           "r" #'org-ref-helm-insert-ref-link
           (:prefix ("c" . "Change")
             "r" #'org-ref-change-ref-type
             "c" #'org-ref-change-cite-type))))))
  (setq org-agenda-files'("~/org/orgzly/Todo.org"
                          "~/org/orgzly/Inbox.org"
                          "~/org/orgzly/Habits.org"
                          "~/org/orgzly/Projects.org"
                          "~/org/orgzly/begrepp.org"
                          "~/org/orgzly/plan.org"
                          "~/org/orgzly/Log.org"
                          "~/org/orgzly/schema.org"
                          "~/org/orgzly/Events.org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING" "|" "DONE(d!)" "CANCELLED(c@)")))
  (setq org-global-properties
        '(("Effort_ALL" .
           "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")))
  (setq org-capture-templates
          '(;; ("w" "Weekly Review" entry (file+datetree "~/Dropbox/org/reviews.org")
            ;;  (file "~/Dropbox/org/templates/weeklyreviewtemplate.org"))
            ;; ("d" "Daily Review" entry (file+datetree "~/Dropbox/org/reviews.org")
            ;;  (file "~/Dropbox/org/templates/dailyreviewtemplate.org"))
            ("x" "Scrapbook with clipboard" entry (file+datetree "~/Documents/notes/scrapbook.org")
             "* %?
  %x
  ")
            ("X" "Scrapbook" entry (file+datetree "~/Documents/notes/scrapbook.org")
             "* %?
  ")
            ("d" "Daily Summary" entry (file+datetree "~/Dropbox/org/reviews.org")
             "* Daily Summary :daily:
  :PROPERTIES:
  :STRESS: %?
  :CONTENTMENT:
  :END:
  %t
  Summary: "
             :jump-to-captured t
             )
            ("w" "Weekly Summary" entry (file+datetree "~/Dropbox/org/reviews.org")
             "* Weekly summary :weekly:
  \** What went well :good:
  \** What can be improved :improve:
  \** Summary
  The Last column is the % of 40hours
  #+BEGIN: clocktable :scope (\"~/org/orgzly/Projects.org\") :maxlevel 2 :block thisweek :stepskip0 t :formula % :match \"school\"
  #+TBLFM: $6=40:00;t::$7=$4;t:: $8=100 * $7/$6
  #+END:
  #+BEGIN: clocktable :scope (\"~/org/orgzly/Projects.org\" \"~/org/orgzly/Log.org\" \"~/org/orgzly/Todo.org\") :maxlevel 5 :block thisweek :stepskip0 t :fileskip0 t
  #+END:
  "
             :jump-to-captured t)
            ("t" "Todo" entry (file "~/Dropbox/org/orgzly/InboxComputer.org")
             "* TODO %? ")
            ("f" "Todo" entry (file "~/Dropbox/org/orgzly/InboxComputer.org")
             "* TODO %?\n %a "
             :created t)
            ("T" "clipboard" entry (file "~/Dropbox/org/orgzly/InboxComputer.org")
             "* %?
  %T
  %x")
            ("z" "Gather info for clocked task" item (clock)
             "%x")
            ("o" "Clock in a other task" entry (file+olp+datetree "~/Dropbox/org/orgzly/Log.org" "Log")
             "* %?"
             :clock-in t
             :clock-resume t)
            ("ce" "Clock in emacs confing" entry (file+headline "~/Dropbox/org/orgzly/Log.org" "Emacs Config")
             "* %?"
             :clock-in t
             :clock-resume t)
            ("cx" "Clock in xmonad confing" entry (file+headline "~/Dropbox/org/orgzly/Log.org" "XMonad Config")
             "* %?"
             :clock-in t
             :clock-resume t)
            ("co" "Clock in general confing" entry (file+headline "~/Dropbox/org/orgzly/Log.org" "Config")
             "* %?"
             :clock-in t
             :clock-resume t)
            ("l" "Learning Log" table-line (file+headline "~/Dropbox/org/orgzly/Log.org" "Learning")
             "| %t | %K %? | | |")
            ("s" "Clock in subtask" entry (clock)
             "* %?"
             :clock-in t
             :clock-resume t
             )
            ("p" "Plan" entry (file  "~/Dropbox/org/orgzly/plan.org")
             "* %?
  %(cfw:org-capture-day)")))
  )

(setq org-roam-capture-templates
      '(("d" "default" plain #'org-roam--capture-get-point "%?"
         :file-name "%<%Y%m%d%H%M%S>"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t
         :immediate-finish t)))
(setq org-roam-directory "~/Documents/notes/")

(use-package! org-roam-bibtex
  :hook (org-roam-mode . org-roam-bibtex-mode))

(use-package! helm-bibtex
  :defer t
  :commands (helm-bibtex)
  :config
  (setq helm-bibtex-bibliography "~/Dropbox/org/references/Zotero_articles.bib"
        reftex-default-bibliography '("~/Dropbox/org/references/Zotero_articles.bib")
        helm-bibtex-library-path "~/Dropbox/org/references/pdfs"
        bibtex-completion-notes-path "~/Documents/notes/"
        bibtex-completion-pdf-field "File")
  (map!
   :leader
   :n "nh" #'helm-bibtex))

(defun my/today ()
    (interactive)
    (let ((org-agenda-custom-commands
           '(("l" "Today"
              ((agenda "" ((org-agenda-view-columns-initially nil)
                           (org-super-agenda-groups
                            '(;; (:name "Habits"
                              ;;        :habit t
                              ;;        :order 100)
                              (:discard (:and (:scheduled past
                                               :not (:habit t))))
                              (:name "Today"
                                     :time-grid t
                                     :date today)
                              (:discard
                               (:deadline t))
                              ))))))))
          (org-agenda-start-day "0d")
          (org-agenda-span 1))
      (org-agenda nil "l")))

(defun my/anytime-todos ()
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo "TODO" "NEXT" "WAITING")
          (tags "anytime"))
    :super-groups
    '((:auto-category))))

(defun my/create-schedule-for-today ()
  (interactive)
  (my//create-schedule '(not (tags "weeklyreview" "monthlyreview" "yearlyreview") )))

(defun my/create-schedule-for-week ()
  (interactive)
  (my//create-schedule '(tags "weeklyreview")))
(defun my/create-schedule-for-month ()
  (interactive)
  (my//create-schedule '(tags "monthlyreview")))

(defun my//create-schedule (tags)
  (let ((org-overriding-columns-format
         "%EFFORT %80ITEM %3PRIORITY %CATEGORY %TODO ")
        (org-agenda-view-columns-initially 't))
    (org-ql-search (append '("~/org/orgzly/InboxComputer.org" "~/org/orgzly/Inbox.org") (org-agenda-files))
                   `(and (todo)
                         (not (org-entry-blocked-p))
                         ,tags
                         (not (and
                               (property "BLOCKED" "")
                               (descendants (todo)))))
                   :super-groups
                   '((:name "Today"
                      :scheduled today)
                     (:name "Past schedule"
                      :scheduled past)
                     (:name "Deadlines"
                      :deadline t
                      :log t)
                     (:auto-category t)))))

(setq +latex-viewers '(okular))
