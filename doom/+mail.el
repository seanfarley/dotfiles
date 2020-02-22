;;; ~/projects/dotfiles/doom/+mail.el -*- lexical-binding: t; -*-

(after! mu4e

  ;============================= org settings first ============================

  ;;store org-mode links to messages
  (require 'org-mu4e)

  ;;store link to message if in header view, not to header query
  (setq org-mu4e-link-query-in-headers-mode nil)

  ;;=============================== basic settings =============================

  (setq
   user-mail-address "sean@farley.io"
   user-full-name  "Sean Farley"

   smtpmail-smtp-user           "sean@farley.io"
   smtpmail-default-smtp-server "mail.farley.io"
   smtpmail-smtp-server         "mail.farley.io"
   smtpmail-stream-type         'starttls
   smtpmail-smtp-service        587

   mu4e-user-mail-address-list '("sean.michael.farley@gmail.com"
                                 "sean@seanfarley.org"
                                 "sean@mcs.anl.gov"
                                 "sean@macports.org"
                                 "sean@lsmsa.net"
                                 "sean@farley.io"
                                 "sfarley@atlassian.com"
                                 "me@smf.io"
                                 "sfarley@iit.edu")

   mu4e-attachment-dir  "~/Downloads"

   ;; n and p already do this and I like be bounded by the current message view
   mu4e-view-scroll-to-next nil

   ;; show the indexing messages
   mu4e-hide-index-messages nil

   ;; prefer the usual behavior since we're not using org-mode for compose
   mu4e-compose-format-flowed nil

   ;; prefer text over html
   mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum

   mu4e-maildir-shortcuts '(("/inbox"     . ?i)
                            ("/lists"     . ?l)
                            ("/archive"   . ?a)
                            ("/drafts"    . ?d)
                            ("/phd"       . ?p)
                            ("/posterity" . ?t)
                            ("/barryisms" . ?b)
                            ("/spam"      . ?s))

   ;; mu4e-use-fancy-chars t

   mu4e-headers-fields
   '((:account       .   12)
     (:human-date    .   12)
     (:flags         .    6)
     (:mailing-list  .   10)
     (:from          .   22)
     (:subject       .   nil))

   mu4e-bookmarks
   (list (list :name "All unread inbox"
               :query "maildir:/inbox AND flag:unread AND NOT flag:trashed"
               :key ?u)
         (list :name "Notes"
               :query (concat "to:sean.michael.farley+self "
                              "AND NOT maildir:/phd "
                              "AND NOT maildir:/posterity")
               :key ?n)
         (list :name "PhD"
               :query (concat "((maildir:/phd AND flag:unread "
                              "  AND NOT flag:trashed) "
                              " OR from:xiaofan) AND date:2y..")
               :key ?p)

         ;; tweaks on the default bookmarks
         (list :name "Today's messages"
               :query "maildir:/inbox AND date:today..now"
               :key ?t)
         (list :name "Last 7 days"
               :query "maildir:/inbox AND date:7d..now"
               :key ?w))

   ;; an absolute must to sort the inbox with the date ascending.
   mu4e-headers-sort-direction 'ascending

   ;; another must: include related emails while searching
   mu4e-headers-include-related t

   ;; show images inline by default
   mu4e-view-show-images nil

   ;; just apply the actions without asking
   mu4e-headers-leave-behavior 'apply)

  ;;=============================== compose hooks ==============================

  ;; helper function to jump to message composing when pressing tab in the
  ;; subject line
  (defun smf/mu4e-tab-subject ()
    (interactive)
    ;; check if line starts with "Subject: "
    (if (not (string-prefix-p "Subject: " (thing-at-point 'line t)))
        ;; if it does not then call the default tab completion
        (message-tab)
      ;; otherwise move the point forward to the message body
      (forward-line 2)))

  (add-hook 'mu4e-compose-mode-hook
            (lambda () (local-set-key (kbd "TAB") #'smf/mu4e-tab-subject)))

  ;; helper method for custom from address
  (defun smf/mu4e-set-from-address ()
    "Set the From address based on the To address of the original."
    (let ((msg mu4e-compose-parent-message))
      (setq user-mail-address
            (cond
             ((null msg) "sean@farley.io")
             ((mu4e-message-contact-field-matches msg :to "sean@macports.org")
              "sean@macports.org")
             ((mu4e-message-contact-field-matches msg :to "sean@lsmsa.net")
              "sean@lsmsa.net")
             ((mu4e-message-contact-field-matches msg :to "sean@farley.io")
              "sean@farley.io")
             ((mu4e-message-contact-field-matches msg :to "me@smf.io")
              "me@smf.io")
             ((or
               (mu4e-message-contact-field-matches msg :from "macports")
               (mu4e-message-contact-field-matches msg :to "macports"))
              "sean@macports.org")
             (t "sean@farley.io")))))

  (add-hook 'mu4e-compose-pre-hook 'smf/mu4e-set-from-address)

  ;;=================================== actions ================================

  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)

  ;; add support for xwidgets if available
  (when (featurep 'xwidget-internal)
    (add-to-list 'mu4e-view-actions
                 '("xViewXWidget" . mu4e-action-view-with-xwidget) t))

  (setq mu4e-marks (assq-delete-all 'trash mu4e-marks))
  (add-to-list 'mu4e-marks
               '(trash
                 :char       "d"
                 :prompt     "dtrash"
                 :show-target (lambda (target) "/trash")
                 :action      (lambda (docid msg target)
                                (mu4e~proc-move docid
                                                mu4e-trash-folder
                                                "+S-u-N"))))

  (add-to-list 'mu4e-marks
               '(archive
                 :char       "A"
                 :prompt     "Archive"
                 :show-target (lambda (target) "/archive")
                 :action      (lambda (docid msg target)
                                (mu4e~proc-move docid
                                                mu4e-refile-folder
                                                "+S-u-N"))))

  (add-to-list 'mu4e-marks
               '(spam
                 :char       "S"
                 :prompt     "Spam"
                 :show-target (lambda (target) "/spam")
                 :action      (lambda (docid msg target)
                                (mu4e~proc-move docid "/spam" "+S-u-N"))))

  (add-to-list 'mu4e-marks
               '(patch
                 :char       "p"
                 :prompt     "Patch"
                 :ask-target (lambda ()
                               (ivy-read "Target directory: "
                                         'read-file-name-internal
                                         :matcher #'smf/find-dir-matcher
                                         :history 'smf/recent-patch-dirs))
                 ;; :show-target (lambda (target) "/spam")
                 :action      (lambda (docid msg target)
                                (shell-command
                                 (format
                                  "hg --cwd %s import --obsolete --partial %s"
                                  target
                                  (mu4e-message-field msg :path))))))

  ;; import a patch from the message view
  (defun smf/mu4e-action-hg-import-patch (msg)
    "Import the hg [patch] message."
    (interactive)
    (ivy-read "Target directory: "
              'read-file-name-internal
              :matcher #'smf/find-dir-matcher
              :history 'smf/recent-patch-dirs
              :action (lambda (d)
                        (shell-command
                         (format
                          "hg --cwd %s import --obsolete --partial %s"
                          d
                          (mu4e-message-field msg :path))))))

  (add-to-list 'mu4e-view-actions
               '("patch" . smf/mu4e-action-hg-import-patch) t)
  (add-to-list 'mu4e-headers-actions
               '("patch" . smf/mu4e-action-hg-import-patch) t)

  ;;================================= mu4e-alert ===============================

  (setq
   ;; only list mails in inbox
   mu4e-alert-interesting-mail-query
   "maildir:/inbox AND flag:unread AND NOT flag:trashed"

   ;; don't draw a newline
   mu4e-maildirs-extension-before-insert-maildir-hook '())

  (mu4e-alert-set-default-style 'notifier)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display)

  ;============================== colorize patches =============================

  ;; colorize patch-based emails
  (add-hook 'mu4e-view-mode-hook #'mu4e-patch-highlight)

  ;============================ monkey-patch `=mu4e' ===========================

  (defun smf/mu4e (orig-func &rest args)
    "Switch to *mu4e* workspace or start it."
    (if (+workspace-exists-p +mu4e-workspace-name)
        (+workspace-switch +mu4e-workspace-name)
      (apply orig-func args)))

  (advice-add #'=mu4e :around #'smf/mu4e)

  (defun smf/mu4e-update-and-index (&rest _)
    (interactive "P")
    (mu4e-update-mail-and-index t))

  ;; mu4e key bindings
  (map!
   ;; TODO add global menu item C-c M U for updating
   "C-c M U" #'smf/mu4e-update-and-index
   (:map mu4e-main-mode-map
     "U"                          #'smf/mu4e-update-and-index)
   (:map (mu4e-view-mode-map mu4e-headers-mode-map)
     ;; too common for me
     "M-<right>"                  nil
     "M-<left>"                   nil)))


(add-hook 'doom-init-ui-hook
          (lambda ()
            ;; remove the org-mode compose functionality: it removes the
            ;; coloring of indented replies
            (remove-hook 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)

            ;; try to get sent messages to the sent folder
            (require 'smtpmail)

            ;; auto start mail notifications
            (require 'mu4e-alert)))
