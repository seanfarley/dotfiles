;;; ~/projects/dotfiles/doom/+irc.el -*- lexical-binding: t; -*-

(after! lui
  (add-hook 'lui-mode-hook #'enable-lui-track-bar))

(after! circe-notifications
  (setq circe-notifications-alert-style 'notifier)
  (setq circe-notifications-alert-icon
        (expand-file-name "irc.png" doom-private-dir)))

(after! irc
  (set-irc-server! "freenode"
                   '(:use-tls t
                     :nick "smf"
                     :user "smf/freenode"
                     :port 6697
                     :host "smf.io"
                     :server-buffer-name "{network}:{host}:{port}"
                     :channels ("#mercurial" "#bitbucket")))
  
  (set-irc-server! "bitlbee"
                   '(:use-tls t
                     :nick "smf"
                     :user "smf/bitlbee"
                     :port 6697
                     :host "smf.io"
                     :server-buffer-name "{network}:{host}:{port}"
                     :channels ("&bitlbee"))))

(defun smf/tracking-shorten (name)
  "Wrapper for `tracking-shorten' that only takes one NAME."
  (car (tracking-shorten (list name))))

;; create a modeline segment that contains the irc tracked buffers; TODO add
;; ability to turn on or off notifications in other frames
(def-modeline-segment! irc-track
  (when (and (boundp 'tracking-mode-line-buffers)
             (derived-mode-p 'circe-mode))
    ;; add a space at the end to pad against the following segment
    (concat
     ;; join all the buffers but replace with icons, if possible
     (mapconcat
      (lambda (b)
        (propertize
         (cond
          ((string-match "#mercurial" b)
           (all-the-icons-faicon "mercury" :v-adjust .05))
          ((string-match "#bitbucket" b)
           (all-the-icons-faicon "bitbucket" :v-adjust .05))
          ((string-match "#octobus-hg" b)
           ;; this inserts a custom fonticon, in this case, octobus
           (propertize "\xe900"
                       'face '(:family "smf-custom-icons")
                       'rear-nonsticky t
                       'display '(raise -0.1)
                       'font-lock-ignore t))
          (t (smf/tracking-shorten b)))
         'face '(:inherit)
         'help-echo "IRC"))
      tracking-buffers
      ;; `space-width' only affects the width of the spaces here, so we can tighten
      ;; it to be a bit more compact
      (propertize " · " 'display '(space-width 0.4)))
    " ")))

(def-modeline! 'irc
  '(bar matches " " buffer-info "  %l:%c %p  " selection-info)
  '(irc-track " " major-mode flycheck))

(remove-hook 'circe-mode-hook #'+doom-modeline|set-special-modeline)
(add-hook! 'circe-mode-hook (doom-set-modeline 'irc))
