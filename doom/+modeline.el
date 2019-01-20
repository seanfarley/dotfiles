;;; ~/projects/dotfiles/doom/+modeline.el -*- lexical-binding: t; -*-

;; use external package for niceties
(def-package! doom-modeline
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-github t)

(defun smf/tracking-shorten (name)
  "Wrapper for `tracking-shorten' that only takes one NAME."
  (car (tracking-shorten (list name))))

(defun smf/tracking-buffers (buffers)
  "Logic to convert some irc BUFFERS to their font-awesome icon."
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
   (propertize " · " 'display '(space-width 0.4))))

;; create a modeline segment that contains the irc tracked buffers
(doom-modeline-def-segment irc-track
  (when (and (boundp 'tracking-mode-line-buffers)
             (derived-mode-p 'circe-mode))
    ;; add a space at the end to pad against the following segment
    (concat (smf/tracking-buffers tracking-buffers) " ")))

;; create a modeline segment that only show an icon for unread messages; TODO
;; add ability to turn on or off notifications in other frames
(doom-modeline-def-segment irc-notification
  (when (boundp 'tracking-mode-line-buffers)
    ;; add a space at the end to pad against the following segment
    (when (and (smf/s-present? (smf/tracking-buffers tracking-buffers))
               (doom-modeline--active))
      (concat
       (propertize (doom-modeline-icon-material "sms"
                                                :height 0.9)
                   'face 'doom-modeline-warning
                   'help-echo (format "IRC Notifications: %s"
                                      (smf/tracking-buffers tracking-buffers))
                   'display '(raise -0.17))
      (propertize " " 'display '(space-width 0.6))))))

;; create a modeline segment that only show an icon for unread messages; TODO
;; add ability to turn on or off notifications in other frames
(doom-modeline-def-segment mu4e-unread
  (when (and (boundp 'mu4e-alert-mode-line)
             (doom-modeline--active))
    (propertize mu4e-alert-mode-line
                'face '(:height 0.85)
                'display '(raise 0.09)
                'help-echo (format "%s unread emails" mu4e-alert-mode-line))))

(doom-modeline-def-modeline 'irc
  '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches " " buffer-info remote-host buffer-position " " selection-info)
  '(irc-track misc-info persp-name mu4e-unread github minor-modes input-method major-mode process))

(add-hook! 'circe-mode-hook (doom-modeline-set-modeline 'irc))
(remove-hook 'circe-mode-hook #'doom-modeline-set-special-modeline)

(doom-modeline-def-modeline 'main+irc
  '(bar workspace-number window-number god-state xah-fly-keys matches buffer-info remote-host  parrot selection-info)
  '(misc-info persp-name lsp irc-notification mu4e-unread github debug minor-modes input-method major-mode process vcs checker))

(doom-modeline-set-modeline 'main+irc t)

;; doom-modeline takes care of this so disable doom's own python modeline
(setq +python-mode-line-indicator nil)

(add-hook 'doom-modeline-before-github-fetch-notification-hook
          (lambda () (smf/bitwarden-init)))
)
