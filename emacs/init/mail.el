(autoload 'mu4e "mu4e" t)

(eval-after-load "mu4e"
  '(progn
  (require 'mu4e-contrib)
  ;; define my custom set of labels for easy word completion
  (setq mylabels (list
                  "\\Trash"
                  "\\Inbox"
                  "\\Spam"
                  "Action"
                  "R/anl"
                  "R/aperture"
                  "R/applications"
                  "R/barryisms"
                  "R/bout"
                  "R/documents"
                  "R/facebook"
                  "R/facets"
                  "R/family"
                  "R/friends"
                  "R/iit"
                  "R/kallithea"
                  "R/karlin"
                  "R/job"
                  "R/llnl"
                  "R/lsmsa"
                  "R/lsu"
                  "R/macports"
                  "R/math"
                  "R/mercurial"
                  "R/osi"
                  "R/petsc-dev"
                  "R/petsc-maint"
                  "R/petsc-students"
                  "R/phd"
                  "R/posterity"
                  "R/recipes"
                  "R/server"
                  "R/siam"
                  "R/talks"
                  "R/teaching"
                  "R/tips"
                  "R/V2"
                  "R/website"))

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (defun mu4e-headers-next-thread (&optional backwards)
    "Move point to the next thread. If BACKWARDS is non-`nil',
    move backwards."
    (interactive "P")
    (let ((find-func (if backwards 'mu4e-headers-find-if 'mu4e-headers-find-if-next)))
      (or (funcall find-func
           (lambda (msg)
             (let ((thread (mu4e-message-field msg :thread)))
               (eq (plist-get thread :level) 0)))
           backwards)
          (mu4e-message (format "No %s thread found"
                                (if backwards "previous" "next"))))))

  (defun mu4e-view-headers-next-thread (&optional backwards)
    "Move point to the next or previous (when BACKWARDS is non-`nil')
unread message header in the headers buffer connected with this
message view. If this succeeds, return the new docid. Otherwise,
return nil."
    (interactive "P")
    (mu4e~view-in-headers-context
     (mu4e-headers-next-thread backwards))
    (mu4e-select-other-view)
    (mu4e-headers-view-message))

  (defun mu4e-action-hg-import-patch (msg)
    "Import the hg [patch] message."
    (let ((path (read-directory-name "Target directory: " "~/projects/hg" nil nil nil) ))
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

  (define-key mu4e-headers-mode-map (kbd "N") 'mu4e-headers-next-thread)
  (define-key mu4e-view-mode-map (kbd "N") 'mu4e-view-headers-next-thread)

  ;; override default binding for "P"
  (define-key mu4e-headers-mode-map (kbd "P")
    (lambda() (interactive) (mu4e-headers-next-thread t)))
  (define-key mu4e-view-mode-map (kbd "P")
    (lambda() (interactive) (mu4e-view-headers-next-thread t)))

  (add-hook 'mu4e-compose-pre-hook 'mu4e-set-from-address)
  (add-hook 'mu4e-compose-mode-hook (lambda () (flyspell-mode)))

  ;; custom actions

  (add-to-list 'mu4e-view-actions '("patch" . mu4e-action-hg-import-patch) t)

  (add-to-list 'mu4e-view-actions '("bView in browser" . mu4e-action-view-in-browser) t)

  (setq mu4e-marks (delq (assoc 'delete mu4e-marks) mu4e-marks)
        mu4e-marks (delq (assoc 'trash mu4e-marks) mu4e-marks)
        mu4e-marks (delq (assoc 'untrash mu4e-marks) mu4e-marks)
        mu4e-marks (delq (assoc 'refile mu4e-marks) mu4e-marks)
        mu4e-marks (delq (assoc 'flag mu4e-marks) mu4e-marks)
        mu4e-marks (delq (assoc 'unflag mu4e-marks) mu4e-marks)
        mu4e-marks (delq (assoc 'move mu4e-marks) mu4e-marks))

  (setq mu4e-marks (delq (assoc 'archive mu4e-marks) mu4e-marks))

  (add-to-list 'mu4e-marks
               '(untag
                 :char       "-"
                 :prompt     "-tag"
                 :ask-target (lambda () (ido-completing-read "Label: " mylabels))
                 :action      (lambda (docid msg target)
                                (mu4e~proc-move docid nil "-N")
                                (mu4e-action-retag-message msg (concat "-" target)))))

  (add-to-list 'mu4e-marks
               '(tag
                 :char       "+"
                 :prompt     "+tag"
                 :ask-target (lambda () (ido-completing-read "Label: " mylabels))
                 :action      (lambda (docid msg target)
                                (mu4e~proc-move docid nil "-N")
                                (mu4e-action-retag-message msg (concat "+" target)))))

  (add-to-list 'mu4e-marks
               '(archive
                 :char       "A"
                 :prompt     "Archive"
                 :show-target (lambda (target) "archive")
                 :action      (lambda (docid msg target)
                                (mu4e-action-retag-message msg "-\\Inbox")
                                (mu4e~proc-move docid nil "+S-u-N"))))

  (add-to-list 'mu4e-marks
               '(untrash
                 :char       "="
                 :prompt     "=untrash"
                 :show-target (lambda (target) "untrash")
                 :action      (lambda (docid msg target)
                                (mu4e~proc-move docid nil "-N")
                                (mu4e-action-retag-message msg "-\\Trash"))))

  (add-to-list 'mu4e-marks
               '(trash
                 :char       "d"
                 :prompt     "dtrash"
                 :show-target (lambda (target) "trash")
                 :action      (lambda (docid msg target)
                                (mu4e-action-retag-message msg "\\Trash")
                                (mu4e~proc-move docid nil "+S-u-N"))))

  ;; removed the functions we don't need (for view)
  (fmakunbound 'mu4e-view-mark-for-move)
  (fmakunbound 'mu4e-view-mark-for-refile)
  (fmakunbound 'mu4e-view-mark-for-flag)
  (fmakunbound 'mu4e-view-mark-for-unflag)

  ;; removed the functions we don't need (for headers)
  (fmakunbound 'mu4e-headers-mark-for-move)
  (fmakunbound 'mu4e-headers-mark-for-refile)
  (fmakunbound 'mu4e-headers-mark-for-flag)
  (fmakunbound 'mu4e-headers-mark-for-unflag)

  ;; regenerate the functions we just defined above
  (mu4e~view-defun-mark-for trash)
  (mu4e~view-defun-mark-for delete)
  (mu4e~view-defun-mark-for tag)
  (mu4e~view-defun-mark-for untag)
  (mu4e~view-defun-mark-for archive)

  ;; regenerate the functions we just defined above (for headers)
  (mu4e~headers-defun-mark-for trash)
  (mu4e~headers-defun-mark-for delete)
  (mu4e~headers-defun-mark-for tag)
  (mu4e~headers-defun-mark-for untag)
  (mu4e~headers-defun-mark-for archive)

  ;; remove move
  (define-key mu4e-view-mode-map (kbd "m") nil)
  (define-key mu4e-headers-mode-map (kbd "m") nil)

  ;; remove delete
  (define-key mu4e-view-mode-map (kbd "D") nil)
  (define-key mu4e-headers-mode-map (kbd "D") nil)

  ;; remap the actions we renamed
  (define-key mu4e-view-mode-map (kbd "A") 'mu4e-view-mark-for-archive)
  (define-key mu4e-view-mode-map (kbd "<delete>") 'mu4e-view-mark-for-trash)
  (define-key mu4e-view-mode-map (kbd "<deletechar>") 'mu4e-view-mark-for-trash)
  (define-key mu4e-view-mode-map (kbd "+") 'mu4e-view-mark-for-tag)
  (define-key mu4e-view-mode-map (kbd "-") 'mu4e-view-mark-for-untag)

  ;; remap the actions we renamed (for headers)
  (define-key mu4e-headers-mode-map (kbd "A") 'mu4e-headers-mark-for-archive)
  (define-key mu4e-headers-mode-map (kbd "<delete>") 'mu4e-headers-mark-for-trash)
  (define-key mu4e-headers-mode-map (kbd "<deletechar>") 'mu4e-headers-mark-for-trash)
  (define-key mu4e-headers-mode-map (kbd "+") 'mu4e-headers-mark-for-tag)
  (define-key mu4e-headers-mode-map (kbd "-") 'mu4e-headers-mark-for-untag)

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

   mu4e~headers-sort-direction 'ascending

   mu4e-confirm-quit nil
   mu4e-maildir "~/.mail"
   mu4e-attachment-dir  "~/Downloads"
   mu4e-drafts-folder "/drafts"

   ;; add note about why we aren't setting up like most other examples
   ;; mu4e-trash-folder  "/trash"

   ;; make sure to use the same header for syncing labels
   mu4e-action-tags-header "X-Keywords"

   ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
   mu4e-sent-messages-behavior 'delete

   ;; set mu4e as default emacs email program
   mail-user-agent 'mu4e-user-agent

   ;; allow for updating mail using 'U' in the main view:
   mu4e-get-mail-command "offlineimap"

   ;; update every 5 minutes
   ;; mu4e-update-interval 300

   mu4e-use-fancy-chars t
   mu4e-view-show-images t
   mu4e-view-prefer-html nil

   ;; convert html messages to markdown syntax
   mu4e-html2text-command 'mu4e-shr2text

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
