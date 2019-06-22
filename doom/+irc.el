;;; ~/projects/dotfiles/doom/+irc.el -*- lexical-binding: t; -*-

(after! circe
  (disable-circe-new-day-notifier))

(after! circe-notifications
  (setq circe-notifications-alert-style 'notifier)
  (setq circe-notifications-alert-icon
        (expand-file-name "irc.png" doom-private-dir)))

(defmacro irc-password! (user)
  "Returns a lambda that circe will evaluate for the password."
  `(lambda (host)
     (funcall (plist-get (car (auth-source-search
                               :host host
                               :user ,user))
                         :secret))))

(after! irc
  (set-irc-server! "freenode"
                   `(:use-tls t
                     :nick "smf"
                     :user "smf/freenode"
                     :pass ,(irc-password! "smf/freenode")
                     :port 6697
                     :host "smf.io"
                     :server-buffer-name "{network}:{host}:{port}"
                     :channels ("#mercurial" "#bitbucket")))

  (set-irc-server! "bitlbee"
                   `(:use-tls t
                     :nick "smf"
                     :user "smf/bitlbee"
                     :pass ,(irc-password! "smf/bitlbee")
                     :port 6697
                     :host "smf.io"
                     :server-buffer-name "{network}:{host}:{port}"
                     :channels ("&bitlbee"))))
