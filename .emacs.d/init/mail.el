(when (require 'mu4e nil 'noerror)

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (defun mu4e-archive-next-message ()
    (interactive)
    (mu4e-action-retag-message (mu4e-message-at-point) "-\\Inbox")
    (sit-for .05)
    (mu4e-view-headers-next))

  (defun mu4e-action-hg-import-patch (msg)
    "Import the hg [patch] message."
    (let ((path (read-directory-name "Target directory: " nil "~/projects/hg" t) ))
      (shell-command
       (format "cd %s; hg import %s"
               path
               (mu4e-message-field msg :path)))))

  (defun mu4e-set-from-address ()
    "Set the From address based on the To address of the original."
    (let ((msg mu4e-compose-parent-message))
          (setq user-mail-address
                (cond
                 ((null msg) "sean.michael.farley@gmail.com")
                 ((mu4e-message-contact-field-matches msg :to "sean@seanfarley.org")
                  "sean@seanfarley.org")
                 ((mu4e-message-contact-field-matches msg :to "sean@mcs.anl.gov")
                  "sean@mcs.anl.gov")
                 ((mu4e-message-contact-field-matches msg :to "sean@macports.org")
                  "sean@macports.org")
                 ((mu4e-message-contact-field-matches msg :to "sean@lsmsa.net")
                  "sean@lsmsa.net")
                 ((mu4e-message-contact-field-matches msg :to "sfarley@iit.edu")
                  "sfarley@iit.edu")
                 (t "sean.micheal.farley@gmail.com")))))

  (define-key mu4e-view-mode-map (kbd "]") 'mu4e-archive-next-message)
  (add-hook 'mu4e-compose-pre-hook 'mu4e-set-from-address)

  ;; custom actions
  (add-to-list 'mu4e-headers-actions '("aArchive message" . mu4e-action-archive-message) t)
  (add-to-list 'mu4e-view-actions '("aArchive message" . mu4e-action-archive-message) t)

  (add-to-list 'mu4e-headers-actions '("tRetag message" . mu4e-action-retag-message) t)
  (add-to-list 'mu4e-view-actions '("tRetag message" . mu4e-action-retag-message) t)

  (add-to-list 'mu4e-view-actions '("pPatch" . mu4e-action-hg-import-patch) t)

  (add-to-list 'mu4e-view-actions '("bView in browser" . mu4e-action-view-in-browser) t)

  (setq
   mu4e-user-mail-address-list '("sean.michael.farley@gmail.com"
                                 "sean@seanfarley.org"
                                 "sean@mcs.anl.gov"
                                 "sean@macports.org"
                                 "sean@lsmsa.net"
                                 "sfarley@iit.edu")

   user-mail-address "sean.michael.farley@gmail.com"
   user-full-name  "Sean Farley"

   mu4e-maildir "~/.mail"
   mu4e-attachment-dir  "~/Downloads"
   mu4e-drafts-folder "/drafts"
   mu4e-trash-folder  "/trash"

   ;; make sure to use the same header for syncing labels
   mu4e-action-tags-header "X-Keywords"

   ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
   mu4e-sent-messages-behavior 'delete

   ;; set mu4e as default emacs email program
   mail-user-agent 'mu4e-user-agent

   ;; allow for updating mail using 'U' in the main view:
   mu4e-get-mail-command "offlineimap"

   ;; update every 5 minutes
   mu4e-update-interval 300

   ;; fancy chars from http://pablo.rauzy.name/dev/init.el.html
   mu4e-headers-new-mark            '("N" . "✉")
   mu4e-headers-unread-mark         '("u" . "☐")
   mu4e-headers-seen-mark           '("S" . "☑")
   mu4e-headers-replied-mark        '("R" . "↵")
   mu4e-headers-passed-mark         '("P" . "⇉")
   mu4e-headers-encrypted-mark      '("x" . "⚷")
   mu4e-headers-signed-mark         '("s" . "✍")
   mu4e-headers-empty-parent-prefix '("-" . "◆")
   mu4e-headers-first-child-prefix  '("\\" . "▶")

   mu4e-use-fancy-chars t
   mu4e-view-show-images t

   ;; convert html messages to markdown syntax
   mu4e-html2text-command "html2text"

   mu4e-headers-date-format "%d %b %Y"
   mu4e-headers-time-format "%H:%M"

   ;; threading and duplicates
   mu4e-headers-show-threads t
   mu4e-headers-results-limit 500
   mu4e-headers-skip-duplicates t
   mu4e-headers-include-related t

   ;; fields on message view
   mu4e-view-fields '(:from :to  :cc :subject :mailing-list :tags :flags :date :maildir :attachments :signature)

   ;; smtp mail setting
   send-mail-function 'message-send-mail-with-sendmail
   sendmail-program "msmtp"

   ;; don't keep message buffers around
   message-kill-buffer-on-exit t

   ;; bookmarks
   mu4e-bookmarks '(("tag:\\\\Inbox" "Inbox"  ?i)
                    ("flag:unread"   "Unread" ?u)
                    ("tag:\\\\Sent"  "Sent"   ?s)
                    ("tag:R/phd"     "PhD"    ?p)
                    ("tag:list"      "Lists"  ?l))

  mu4e-maildir-shortcuts '(("/archive" . ?a)
                           ("/drafts"  . ?d)
                           ("/trash"   . ?t))

  ;; fields in list view
  mu4e-headers-fields '((:human-date . 15)
                        (:flags      .  5)
                        (:from       . 30)
                        (:subject    . nil))


  )
)
