;;; ~/projects/dotfiles/doom/+modeline.el -*- lexical-binding: t; -*-

(def-package! doom-modeline
  :hook (after-init . (lambda ()
                              (doom-modeline-init)
                              (doom-modeline-set-modeline 'smf/main t)))
  :config

  (defun smf/irc-icons (buffer)
    "Given a BUFFER name, return an icon. Else return buffer."
    (cond
     ((string-match "#mercurial" buffer)
      (all-the-icons-faicon "mercury" :v-adjust .05))
     ((string-match "#bitbucket" buffer)
      (all-the-icons-faicon "bitbucket" :v-adjust .05))
     ((string-match "#octobus-hg" buffer)
      ;; this inserts a custom fonticon, in this case, octobus
      (propertize "\xe900"
                  'face '(:family "smf-custom-icons")
                  'rear-nonsticky t
                  'display '(raise -0.1)
                  'font-lock-ignore t))
     (t buffer)))

  (doom-modeline-def-modeline 'smf/main
    '(bar workspace-number window-number god-state xah-fly-keys matches buffer-info remote-host parrot selection-info)
    '(misc-info persp-name lsp irc mu4e github debug minor-modes process vcs checker))

  (setq
   ;; doom-modeline takes care of this so disable doom's own python modeline
   +python-mode-line-indicator           nil
   doom-modeline-buffer-file-name-style  'truncate-upto-root
   doom-modeline-irc-stylize             #'smf/irc-icons
   doom-modeline-github                  t)

  (add-hook 'doom-modeline-before-github-fetch-notification-hook
            (lambda () (smf/bitwarden-init))))
