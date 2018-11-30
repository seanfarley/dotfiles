;;; ~/projects/dotfiles/doom/+modeline.el -*- lexical-binding: t; -*-

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
(def-modeline-segment! irc-track
  (when (and (boundp 'tracking-mode-line-buffers)
             (derived-mode-p 'circe-mode))
    ;; add a space at the end to pad against the following segment
    (concat (smf/tracking-buffers tracking-buffers) " ")))

;; create a modeline segment that only show an icon for unread messages; TODO add
;; ability to turn on or off notifications in other frames
(def-modeline-segment! irc-notification
  (when (boundp 'tracking-mode-line-buffers)
    ;; add a space at the end to pad against the following segment
    (when (smf/s-present? (smf/tracking-buffers tracking-buffers))
      (concat (+doom-ml-icon "sms") " "))))

(def-modeline-format! :irc
  '(+modeline-matches " "
    +modeline-buffer-id)
  `(mode-line-misc-info
    irc-track
    +modeline-major-mode " "
    mode-line-process
    +modeline-flycheck))
(add-hook! 'circe-mode-hook (set-modeline! :irc))

;; older doom-modeline
;; (def-modeline! 'irc
;;   '(bar matches " " buffer-info "  %l:%c %p  " selection-info)
;;   '(irc-track " " major-mode flycheck))
;; (add-hook! 'circe-mode-hook (doom-set-modeline 'irc))

(remove-hook 'circe-mode-hook #'+doom-modeline|set-special-modeline)

;; define a new main modeline that has

(def-modeline-format! :main+irc
  '(+modeline-matches " "
    +modeline-buffer-state
    +modeline-buffer-id
    "  %2l:%c %p  ")
  `(irc-notification
    mode-line-misc-info
    +modeline-encoding
    +modeline-major-mode " "
    (vc-mode (" " +modeline-vcs " "))
    mode-line-process
    +modeline-flycheck))

(set-modeline! :main+irc t)
