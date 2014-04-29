(autoload 'mu4e "mu4e" t)

(eval-after-load "mu4e"
  '(progn
  ;; define my custom set of labels for easy word completion
  (setq mylabels (list "\\Trash" "\\Inbox" "\\Spam" "Action" "R/anl" "R/aperture" "R/applications" "R/armins-wedding" "R/barryisms" "R/bout" "R/chipy" "R/documents" "R/facets" "R/family" "R/friends" "R/GSoC" "R/iit" "R/karlin" "R/karlin-users" "R/job" "R/llnl" "R/lsmsa" "R/lsmsa-elections" "R/lsmsa.net" "R/lsu" "R/macports" "R/math" "R/mercurial" "R/nersc" "R/petsc-announce" "R/petsc-dev" "R/petsc-exchange" "R/petsc-maint" "R/petsc-students" "R/petsc-users" "R/phd" "R/posterity" "R/recipes" "R/reunion" "R/server" "R/shelved-projects" "R/siam" "R/talks" "R/teaching" "R/tips" "R/V2" "R/vera" "R/website" "R/zfs"))

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (defun mu4e-select-label (&optional prefix)
    "Select a label from a hard-coded list"
    (interactive "P")
    (if (not (equal prefix '-))
        (setq prefix ""))
    (format "%s%s" prefix (ido-completing-read "Label: " mylabels)))

  (defun mu4e-thread-count (&optional negative)
    "Count the number of messages in a thread from the current
point. Passing a boolean prefix will count in the other
direction, i.e. count backwards"
    (interactive)
    (let* ((msg (mu4e-message-at-point))
           (msgid (mu4e-message-field msg :message-id))
           (thread-id (mu4e~headers-get-thread-info msg 'thread-id))
           (path      (mu4e~headers-get-thread-info msg 'path))
           (started negative)
           (tcount 0))
      (mu4e-headers-for-each
       (lambda (mymsg)
         (let ((my-thread-id (mu4e~headers-get-thread-info mymsg 'thread-id))
               (mymsgid (mu4e-message-field msg :message-id)))
           (when (string= thread-id (mu4e~headers-get-thread-info mymsg 'thread-id))
             (when (string= mymsgid msgid)
               (setq started (not started)))
             (when started
               (setq tcount (+ tcount 1)))))))
      tcount))

  (defun mu4e-label-thread (label)
    "Label the thread at point."
    ;; the thread id is shared by all messages in a thread
    (interactive "P")
    (let* ((msg (mu4e-message-at-point))
           (thread-id (mu4e~headers-get-thread-info msg 'thread-id))
           (path      (mu4e~headers-get-thread-info msg 'path)))
      (mu4e-headers-for-each
       (lambda (mymsg)
         (let ((my-thread-id (mu4e~headers-get-thread-info mymsg 'thread-id)))
           (when (string= thread-id (mu4e~headers-get-thread-info mymsg 'thread-id))
             (mu4e-action-retag-message mymsg label)))))))

  (defun mu4e-mark-read-thread ()
    "Label the thread at point."
    ;; the thread id is shared by all messages in a thread
    (interactive)
    (let* ((msg (mu4e-message-at-point))
           (thread-id (mu4e~headers-get-thread-info msg 'thread-id))
           (path      (mu4e~headers-get-thread-info msg 'path)))
      (mu4e-headers-for-each
       (lambda (mymsg)
         (let ((my-thread-id (mu4e~headers-get-thread-info mymsg 'thread-id))
               (mymsgid (mu4e-message-field mymsg :message-id)))
           (when (string= thread-id (mu4e~headers-get-thread-info mymsg 'thread-id))
             (mu4e~proc-move mymsgid nil "+S-u-N")))))))

  (defun mu4e-headers-label-thread (&optional prefix)
    (interactive "P")
    (mu4e-label-thread (mu4e-select-label prefix)))

  (defun mu4e-view-label-thread (&optional prefix)
    (interactive "P")
    (mu4e~view-in-headers-context
     (mu4e-label-thread (mu4e-select-label prefix))))

  (defun mu4e-headers-thread-next ()
    (interactive)
    (mu4e-headers-next (mu4e-thread-count)))

  (defun mu4e-view-thread-next ()
    (interactive)
    (mu4e-view-headers-next (mu4e~view-in-headers-context
     (mu4e-thread-count))))

  (defun mu4e-headers-thread-prev ()
    (interactive)
    ;; move the beginnging of the current thread
    (mu4e-headers-prev (mu4e-thread-count 't))
    ;; move up to the previous thread
    (mu4e-headers-prev)
    ;; now move the beginning of desired thread
    (mu4e-headers-prev (mu4e-thread-count 't)))

  (defun mu4e-view-thread-prev (&optional)
    (interactive)
    ;; move the beginnging of the current thread
    (mu4e-view-headers-prev (mu4e~view-in-headers-context (mu4e-thread-count 't)))

    ;; wtf, this errors out?
    ;; user-error: [mu4e] No message at point

    ;; move up to the previous thread
    (sit-for .1)
    (mu4e-view-headers-prev)
    ;; now move the beginning of desired thread
    ;; (mu4e-view-headers-prev (mu4e~view-in-headers-context (mu4e-thread-count 't)))
    )

  (defun mu4e-archive-next-message ()
    (interactive)
    (mu4e~view-in-headers-context
     (mu4e-label-thread "-\\Inbox"))
    (sit-for .05)
    (mu4e~view-in-headers-context
     (mu4e-mark-read-thread))
    (sit-for .1)
    (mu4e-view-thread-next))

  (defun mu4e-delete-next-message ()
    (interactive)
    (mu4e~view-in-headers-context
     (mu4e-label-thread "\\Trash"))
    (sit-for .05)
    (mu4e~view-in-headers-context
     (mu4e-mark-read-thread))
    (sit-for .1)
    (mu4e-view-thread-next))

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
                  "sean.michael.farley@gmail.com")
                 ((mu4e-message-contact-field-matches msg :to "sean@macports.org")
                  "sean@macports.org")
                 ((mu4e-message-contact-field-matches msg :to "sean@lsmsa.net")
                  "sean@lsmsa.net")
                 ((mu4e-message-contact-field-matches msg :to "sean@farley.io")
                  "sean@farley.io")
                 ((mu4e-message-contact-field-matches msg :to "me@smf.io")
                  "me@smf.io")
                 ((mu4e-message-contact-field-matches msg :to "sfarley@iit.edu")
                  "sfarley@iit.edu")
                 ((or
                   (mu4e-message-contact-field-matches msg :from "anl.gov")
                   (mu4e-message-contact-field-matches msg :to "anl.gov"))
                  "sean.michael.farley@gmail.com")
                 ((or
                   (mu4e-message-contact-field-matches msg :from "macports")
                   (mu4e-message-contact-field-matches msg :to "macports"))
                  "sean@macports.org")
                 ((or
                   (mu4e-message-contact-field-matches msg :from "iit.edu")
                   (mu4e-message-contact-field-matches msg :to "iit.edu"))
                  "sfarley@iit.edu")
                 (t "sean.micheal.farley@gmail.com")))))

  (define-key mu4e-headers-mode-map (kbd "]") 'mu4e-mark-read-thread)
  (define-key mu4e-view-mode-map (kbd "]") 'mu4e-archive-next-message)

  (define-key mu4e-headers-mode-map (kbd "l") 'mu4e-headers-label-thread)
  (define-key mu4e-view-mode-map (kbd "l") 'mu4e-view-label-thread)

  (define-key mu4e-headers-mode-map (kbd "M-n") 'mu4e-headers-thread-next)
  (define-key mu4e-view-mode-map (kbd "M-n") 'mu4e-view-thread-next)

  (define-key mu4e-headers-mode-map (kbd "M-p") 'mu4e-headers-thread-prev)
  (define-key mu4e-view-mode-map (kbd "M-p") 'mu4e-view-thread-prev)

  (define-key mu4e-headers-mode-map (kbd "d") 'mu4e-delete-next-message)
  (define-key mu4e-view-mode-map (kbd "d") 'mu4e-delete-next-message)

  (add-hook 'mu4e-compose-pre-hook 'mu4e-set-from-address)
  (add-hook 'mu4e-compose-mode-hook (lambda () (flyspell-mode)))

  ;; custom actions
  (add-to-list 'mu4e-headers-actions '("archive message" . mu4e-action-archive-message) t)
  (add-to-list 'mu4e-view-actions '("archive message" . mu4e-action-archive-message) t)

  (add-to-list 'mu4e-headers-actions '("tRetag message" . mu4e-action-retag-message) t)
  (add-to-list 'mu4e-view-actions '("tRetag message" . mu4e-action-retag-message) t)

  (add-to-list 'mu4e-view-actions '("patch" . mu4e-action-hg-import-patch) t)

  (add-to-list 'mu4e-view-actions '("bView in browser" . mu4e-action-view-in-browser) t)

  (setq
   mu4e-user-mail-address-list '("sean.michael.farley@gmail.com"
                                 "sean@seanfarley.org"
                                 "sean@mcs.anl.gov"
                                 "sean@macports.org"
                                 "sean@lsmsa.net"
                                 "sean@farley.io"
                                 "me@smf.io"
                                 "sfarley@iit.edu")

   user-mail-address "sean.michael.farley@gmail.com"
   user-full-name  "Sean Farley"

   mu4e-confirm-quit nil
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
   mu4e-bookmarks '(("tag:/Inbox AND NOT flag:trashed AND NOT tag:/Trash"  "Inbox"     ?i)
                    ("flag:unread"                      "Unread"    ?u)
                    ("tag:/Sent"                        "Sent"      ?s)
                    ("tag:R/mercurial"                  "Mercurial" ?h)
                    ("tag:R/macports"                   "Macports"  ?m)
                    ("tag:R/phd"                        "PhD"       ?p)
                    ("tag:Action"                       "Action"    ?a)
                    ("tag:list"                         "Lists"     ?l))

  mu4e-maildir-shortcuts '(("/archive" . ?a)
                           ("/drafts"  . ?d)
                           ("/trash"   . ?t))

  ;; fields in list view
  mu4e-headers-fields '((:human-date . 15)
                        (:flags      .  5)
                        (:from       . 30)
                        (:subject    . nil))

   ;; fancy chars from http://pablo.rauzy.name/dev/init.el.html
   mu4e-headers-new-mark            '("N" . "✉")
   mu4e-headers-seen-mark           '("S" . "☑")
   mu4e-headers-replied-mark        '("R" . "↵")
   mu4e-headers-passed-mark         '("P" . "⇉")

   ;; don't use a signature
   mu4e-compose-signature-auto-include nil
  )
))
