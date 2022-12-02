;;; +mail.el -*- lexical-binding: t; -*-

;; when running an emacs that isn't from brew, /usr/local/share isn't
;; automatically added to the load-path
(let ((mu4e-path "/usr/local/share/emacs/site-lisp/mu/mu4e/"))
  (when (file-directory-p mu4e-path)
    (add-load-path! mu4e-path)))

(after! mu4e
  (require 'smtpmail)

;;; org settings

  ;;store link to message if in header view, not to header query
  (setq mu4e-org-link-query-in-headers-mode nil)

;;; basic settings

  (defun smf/iit-context-p (msg)
    "Determine if MSG is from/to my IIT address."
    (when msg
      (mu4e-message-contact-field-matches
       msg '(:to :from) "@.*iit.edu")))

  ;; TODO `set-email-account!' doesn't accept a custom match-func so we have to
  ;; duplicate what it does
  (setq
   mu4e-contexts
   (list (make-mu4e-context
          :name "farley.io"
          :enter-func (lambda ()
                        (mu4e-message "Switched to farley.io"))
          :leave-func (lambda ()
                        (progn (setq +mu4e-personal-addresses nil)
                               (mu4e-clear-caches)))
          :match-func (lambda (msg) (when msg (not (smf/iit-context-p msg))))
          :vars '((user-mail-address      . "sean@farley.io")
                  (smtpmail-smtp-user     . "sean@farley.io")
                  (smtpmail-smtp-server   . "box.farley.io")))

         (make-mu4e-context
          :name "iit.edu"
          :enter-func (lambda ()
                        (mu4e-message "Switched to iit.edu"))
          :leave-func (lambda ()
                        (progn (setq +mu4e-personal-addresses nil)
                               (mu4e-clear-caches)))
          :match-func #'smf/iit-context-p
          :vars '((user-mail-address      . "sfarley@hawk.iit.edu")
                  (smtpmail-smtp-user     . "sfarley@hawk.iit.edu")
                  (smtpmail-smtp-server   . "smtp.gmail.com")))))

  (setq
   user-full-name    "Sean Farley"

   smtpmail-stream-type         'starttls
   smtpmail-smtp-service        587
   send-mail-function           #'smtpmail-send-it

   mu4e-attachment-dir "~/Downloads"
   mu4e-drafts-folder  "/Drafts"
   mu4e-refile-folder  "/Archive"
   mu4e-sent-folder    "/Sent"
   mu4e-trash-folder   "/Trash"

   ;; bullet char in main view
   +mu4e-main-bullet "·"

   ;; n and p already do this and I like be bounded by the current message view
   mu4e-view-scroll-to-next nil

   ;; show the indexing messages
   mu4e-hide-index-messages nil

   ;; prefer the usual behavior since we're not using org-mode for compose
   mu4e-compose-format-flowed nil

   ;; prefer text over html
   mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum

   mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*")

   mu4e-maildir-shortcuts (list (list :maildir "/inbox"
                                      :key ?i)
                                (list :maildir "/lists"
                                      :key ?l)
                                (list :maildir "/Archive"
                                      :key ?a)
                                (list :maildir "/Drafts"
                                      :key ?d)
                                (list :maildir "/phd"
                                      :key ?p)
                                (list :maildir "/posterity"
                                      :key ?t)
                                (list :maildir "/barryisms"
                                      :key ?b)
                                (list :maildir "/Spam"
                                      :key ?s))

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
                              " OR from:xiaofan) AND date:2m..")
               :key ?p)
         (list :name "Sent in the last week"
               :query "maildir:/Sent AND date:1w..now"
               :key ?s)

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

;;; compose hooks

  ;; helper function to jump to message composing when pressing tab in the
  ;; subject line
  (defun smf/mu4e-tab-subject ()
    (interactive)
    ;; check if line starts with "Subject: "
    (if (not (string-prefix-p "Subject: " (thing-at-point 'line t)))
        ;; if it does not then call the default tab completion
        (message-tab)
      ;; otherwise move the point forward to the message body
      (forward-line 3)))

  (add-hook 'mu4e-compose-mode-hook
            (lambda () (local-set-key (kbd "TAB") #'smf/mu4e-tab-subject)))

;;; actions

  ;; never in my life have I used this
  (when (member '("view as pdf" . mu4e-action-view-as-pdf) mu4e-view-actions)
    (delete '("view as pdf" . mu4e-action-view-as-pdf) mu4e-view-actions))

  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)

  ;; add support for xwidgets if available
  (when (featurep 'xwidget-internal)
    (require 'xwidget)
    (when (functionp 'xwidget-webkit-goto-url-block-ext)
      (defun smf/mu4e-block-content (orig-fn &rest args)
        (cl-letf* ((old-xwidget-webkit-browse-url
                    (symbol-function 'xwidget-webkit-browse-url))
                   ((symbol-function 'xwidget-webkit-browse-url)
                    (lambda (url &optional new-session &rest _ignore)
                      (funcall old-xwidget-webkit-browse-url url new-session t)))
                   (old-xwidget-webkit-goto-url
                    (symbol-function 'xwidget-webkit-goto-url))
                   ((symbol-function 'xwidget-webkit-goto-url)
                    (lambda (url &rest _ignore)
                      (funcall old-xwidget-webkit-goto-url url t))))
          (apply orig-fn args)))

      (advice-add #'mu4e-action-view-with-xwidget
                  :around
                  #'smf/mu4e-block-content)

      (advice-add #'xwidgets-reuse-xwidget-reuse-browse-url
                  :around
                  #'smf/mu4e-block-content)

      (unless (member '("xwidgets view" . mu4e-action-view-with-xwidget)
                      mu4e-view-actions)
        (add-to-list 'mu4e-view-actions
                     '("xwidgets view" . mu4e-action-view-with-xwidget)))))

  (setq mu4e-marks (assq-delete-all 'trash mu4e-marks))
  (add-to-list 'mu4e-marks
               '(trash
                 :char       "d"
                 :prompt     "dtrash"
                 :show-target (lambda (target) mu4e-trash-folder)
                 :action      (lambda (docid msg target)
                                (mu4e--server-move docid
                                                   mu4e-trash-folder
                                                   "+S-u-N"))))

  (add-to-list 'mu4e-marks
               '(archive
                 :char       "A"
                 :prompt     "Archive"
                 :show-target (lambda (target) mu4e-refile-folder)
                 :action      (lambda (docid msg target)
                                (mu4e--server-move docid
                                                   mu4e-refile-folder
                                                   "+S-u-N"))))

  (add-to-list 'mu4e-marks
               '(spam
                 :char       "S"
                 :prompt     "Spam"
                 :show-target (lambda (target) "/Spam")
                 :action      (lambda (docid msg target)
                                (mu4e--server-move docid "/Spam" "+S-u-N"))))

  (add-to-list 'mu4e-marks
               '(patch
                 :char       "p"
                 :prompt     "Patch"
                 :ask-target (lambda ()
                               (ivy-read "Target directory: "
                                         'read-file-name-internal
                                         :matcher #'smf/find-dir-matcher
                                         :history 'smf/recent-patch-dirs))
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

;;; mu4e-alert

  (setq
   ;; only list mails in inbox
   mu4e-alert-interesting-mail-query
   "maildir:/inbox AND flag:unread AND NOT flag:trashed")

  ;; adds unread mail count to mode line
  (mu4e-alert-enable-mode-line-display)

;;; colorize patches

  ;; colorize patch-based emails
  (add-hook 'gnus-part-display-hook 'message-view-patch-highlight)

;;; monkey patchs

  (defun smf/mu4e-update-and-index (&rest _)
    (interactive "P")
    (mu4e-update-mail-and-index t))

  ;; REVIEW https://github.com/djcb/mu/pull/2330
  ;; merge into development version; remove when 1.10 is released
  (defun smf/mu4e-refresh-main ()
    (with-current-buffer mu4e-main-buffer-name
      (revert-buffer)))

  (add-hook 'mu4e-index-updated-hook #'smf/mu4e-refresh-main)

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
