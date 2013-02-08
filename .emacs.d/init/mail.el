(when (require 'mu4e nil 'noerror)

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
   (imagemagick-register-types))

  ;; default
  (setq mu4e-maildir "~/.mail")
  (setq mu4e-attachment-dir  "~/Downloads")

  (setq mu4e-drafts-folder "/drafts")
  (setq mu4e-trash-folder  "/trash")

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; to detect my mail
  (setq mu4e-user-mail-address-list '("sean.michael.farley@gmail.com"
                                      "sean@seanfarley.org"
                                      "sean@mcs.anl.gov"
                                      "sean@macports.org"
                                      "sean@lsmsa.net"
                                      "sfarley@iit.edu"))

  ;; Set mu4e as default emacs email program
  (setq mail-user-agent 'mu4e-user-agent)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.

  (setq mu4e-maildir-shortcuts
        '( ("/archive"    . ?a)
           ("/drafts"     . ?d)
           ("/trash"      . ?t)))

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap")

  ;; something about ourselves
  (setq
   user-mail-address "sean.michael.farley@gmail.com"
   user-full-name  "Sean Farley")

  ;; Fancy chars
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-view-show-images t)

  ;; convert html messages to markdown syntax
  (setq mu4e-html2text-command "html2text")

  (defun mu4e-archive-next-message ()
    (interactive)
    (mu4e-action-retag-message (mu4e-message-at-point) "-\\Inbox")
    (sit-for .05)
    (mu4e-view-headers-next))

  (define-key mu4e-view-mode-map (kbd "]") 'mu4e-archive-next-message)

  ;; Custom actions
  (setq mu4e-action-tags-header "X-Keywords")
  (add-to-list 'mu4e-headers-actions '("aArchive message" . mu4e-action-archive-message) t)
  (add-to-list 'mu4e-view-actions '("aArchive message" . mu4e-action-archive-message) t)

  (add-to-list 'mu4e-headers-actions '("tRetag message" . mu4e-action-retag-message) t)
  (add-to-list 'mu4e-view-actions '("tRetag message" . mu4e-action-retag-message) t)

  (add-to-list 'mu4e-view-actions '("bView in browser" . mu4e-action-view-in-browser) t)

  ;; Bookmarks
  (setq mu4e-bookmarks
        '(("flag:unread"                       "New"                 ?n)
          ("tag:\\\\Inbox"                     "Inbox"               ?i)
          ("tag:\\\\Sent"                      "Sent"                ?s)

          ("tag:R/phd"                         "PhD"                 ?p)
          ("tag:list"                          "Lists"               ?l)
          ))

  ;; Times and dates
  (setq mu4e-headers-date-format "%d %b %Y")
  (setq mu4e-headers-time-format "%H:%M")

  ;; threading and duplicates
  (setq mu4e-headers-show-threads t)
  (setq mu4e-headers-results-limit 500)
  (setq mu4e-headers-skip-duplicates t)
  (setq mu4e-headers-include-related t)

  ;; fields in list view
  (setq mu4e-headers-fields
        '( (:human-date    .  15)
           (:flags         .   5)
           (:from          .  30)
           (:subject       .  nil)))

  ;; Fields on message view
  (setq mu4e-view-fields
        '(:from :to  :cc :subject :mailing-list :tags :flags :date :maildir :attachments :signature))

  ;; smtp mail setting
  (setq send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)
)
