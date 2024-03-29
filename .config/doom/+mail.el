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
          :match-func (lambda (msg) (when msg (not (smf/iit-context-p msg))))
          :vars '((user-mail-address      . "sean@farley.io")
                  (smtpmail-smtp-user     . "sean@farley.io")
                  (smtpmail-smtp-server   . "box.farley.io")))

         (make-mu4e-context
          :name "iit.edu"
          :enter-func (lambda ()
                        (mu4e-message "Switched to iit.edu"))
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

   ;; n and p already do this and I like be bounded by the current message view
   mu4e-view-scroll-to-next nil

   ;; show the indexing messages
   mu4e-hide-index-messages nil

   ;; prefer the usual behavior since we're not using org-mode for compose
   mu4e-compose-format-flowed nil

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
               :key ?u
               :favorite t)
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
   mu4e-search-sort-direction 'ascending

   ;; another must: include related emails while searching
   mu4e-search-include-related t

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

;;; monkey patchs

  (defun smf/mu4e-update-and-index (&rest _)
    (interactive "P")
    (mu4e-update-mail-and-index t))

  ;; REVIEW fixed in 1.10.5 (or 1.11); remove when that is available
  (defadvice! smf/mu4e--read-choice-builtin (prompt choices)
    :override #'mu4e--read-choice-builtin
    "Fixes ESC not working"
    (let ((chosen) (inhibit-quit nil)
        (prompt (format "%s%s"
                        (mu4e-format prompt)
                        (mapconcat #'car choices ", "))))
    (while (not chosen)
      (message nil) ;; this seems needed...
      (when-let ((kar (read-char-exclusive prompt)))
        (when (eq kar ?\e) (keyboard-quit)) ;; `read-char-exclusive' is a C
                                            ;; function and doesn't check for
                                            ;; `keyboard-quit', there we need to
                                            ;; check if ESC is pressed
        (setq chosen (mu4e--matching-choice choices kar))))
    chosen))

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
