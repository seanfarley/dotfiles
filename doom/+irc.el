;;; ~/projects/dotfiles/doom/+irc.el -*- lexical-binding: t; -*-

(after! lui
  (add-hook 'lui-mode-hook #'enable-lui-track-bar))

(defun smf/tracking-next-buffer ()
  (interactive)
  (when (derived-mode-p 'circe-mode)
    (tracking-next-buffer)))

;; automatically add some buffers to the workspace
(after! persp-mode
  (defun smf/unreal-buffer-p (buffer-or-name)
    "Wrap `doom-unreal-buffer-p' but allow circe buffers through."
    (unless (and (derived-mode-p 'circe-mode)
                 (eq (safe-persp-name (get-current-persp))
                     +irc--workspace-name))
      (doom-unreal-buffer-p buffer-or-name)))

  (setq persp-add-buffer-on-after-change-major-mode-filter-functions
        '(smf/unreal-buffer-p)))

(defun smf/add-circe-buffer-to-persp ()
  (let ((persp (get-current-persp))
        (buf (current-buffer)))
    ;; only add a new circe buffer to the irc workspace when we're in another
    ;; workspace
    (unless (eq (safe-persp-name persp) +irc--workspace-name)
      ;; add new circe buffers to the persp containing circe buffers
      (persp-add-buffer buf
                        (persp-get-by-name +irc--workspace-name))
      ;; remove new buffer from accidental workspace
      (persp-remove-buffer buf persp))))

(after! circe
  (disable-circe-new-day-notifier)
  (define-key tracking-mode-map (kbd "C-c C-SPC") 'smf/tracking-next-buffer)
  (add-hook 'circe-mode-hook #'smf/add-circe-buffer-to-persp))

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
