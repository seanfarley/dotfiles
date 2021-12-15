;;; ~/projects/dotfiles/doom/+irc.el -*- lexical-binding: t; -*-

;; Below adds the ability to launch a new chat with any nick. It amalgamates all
;; nicks in all channels because that is simple and hasn't been a problem for me
;; yet. My [[http://www.bitlbee.org][BitlBee]] server appends "|fb" or "|gtalk"
;; for the corresponding chat method so that takes care of most potential name
;; conflicts.

(defun +circe--combine-channel-nicks (channel nicks)
  "Combine CHANNEL with NICK in a unique way.

This is needed due to `completing-read' only taking a
list (instead of say, a list of cons).

The spec at https://modern.ircdocs.horse/#wire-format-in-abnf
seems to imply we can use ':' as a delimiter."
  (mapcar (lambda (nick)
            (format "%s: %s" channel nick))
          nicks))

(defun +circe--split-channel-nick (channel-nick)
  "Split item from `+circe--combine-channel-nicks'."
  (split-string channel-nick ": "))

(defun +circe-buffers-fn ()
  "Returns a list of all circe derived buffers."
  (mapcan (lambda (buffer)
            (with-current-buffer buffer
              (when (derived-mode-p 'circe-mode)
                (list buffer))))
          (buffer-list)))

(defun +circe-buffer-nicks (&optional buffer-or-name)
  "Return a list or nil of nicks in a BUFFER-OR-NAME.

Similar to `circe-channel-nicks' except this ignores errors."
  (condition-case nil
      (with-current-buffer (or buffer-or-name (current-buffer))
        (circe-channel-nicks))
      (error nil)))

(defun +circe-users-fn ()
  "Return a list of strings as formatted by `+circe--combine-channel-nick'."
  (mapcan (lambda (buffer)
            (when-let ((nicks (+circe-buffer-nicks buffer)))
              (+circe--combine-channel-nicks (buffer-name buffer) nicks)))
          (+circe-buffers-fn)))
(+circe-users-fn)

(defun +circe-query-prompt-user ()
  "Prompt for a user to query.

This method will gather a list of all users across all the circe
buffers and present them to the user."
  (interactive)
  (let* ((users (+circe-users-fn))
         (buffer-nick (completing-read "/QUERY: " users)))
    (when (and buffer-nick
               (not (string-empty-p buffer-nick))
               (member buffer-nick users)) ; this /should/ work since
                                           ; `completing-read' returns the
                                           ; actual string object instead of a
                                           ; copy
      (let* ((bn-split (+circe--split-channel-nick buffer-nick))
             (buffer (car bn-split))
             (nick (car (cdr bn-split))))
        (persp-switch "*IRC*")
        (with-current-buffer buffer
          (circe-command-QUERY nick))))))

(map!
 (:when (featurep! :app irc)
   :desc "Query / chat with user"       "C-c I c" #'+circe-query-prompt-user))


(after! circe
  (setq circe-fool-list '("^gitter"))
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
                     :port 16697
                     :host "smf.io"
                     :server-buffer-name "{network}:{host}:{port}"))

  (set-irc-server! "bitlbee"
                   `(:use-tls t
                     :nick "smf"
                     :user "smf/bitlbee"
                     :pass ,(irc-password! "smf/bitlbee")
                     :port 16697
                     :host "smf.io"
                     :server-buffer-name "{network}:{host}:{port}"))

  (set-irc-server! "gitter"
                   `(:use-tls t
                     :nick "smf"
                     :user "smf/gitter"
                     :pass ,(irc-password! "smf/gitter")
                     :port 16697
                     :host "smf.io"
                     :server-buffer-name "{network}:{host}:{port}")))
