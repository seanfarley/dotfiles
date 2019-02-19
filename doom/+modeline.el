;;; ~/projects/dotfiles/doom/+modeline.el -*- lexical-binding: t; -*-

(def-package! doom-modeline
  :hook (after-init . (lambda ()
                              (doom-modeline-init)
                              (doom-modeline-set-modeline 'smf/main t)))
  :config

  ;=============================== segment tweaks ==============================

  (defun smf/vcs-tweak (prop &rest _)
    "Trim first left space from vcs segment."
    (when prop
      (substring prop 1)))

  (advice-add (alist-get 'vcs doom-modeline-fn-alist)
              :filter-return #'smf/vcs-tweak)

  (defun smf/irc-tweak (prop &rest _)
    "Trim left space."
    (when prop
      (s-trim-left prop)))

  (advice-add (alist-get 'irc doom-modeline-fn-alist)
              :filter-return #'smf/irc-tweak)

  (defun smf/mu4e-tweak (prop &rest _)
    "Trim all space and add a tiny space to the right side."
    (when prop
      (concat (s-trim prop)
              (propertize " " 'display '(space-width 0.5)))))

  (advice-add (alist-get 'mu4e doom-modeline-fn-alist)
              :filter-return #'smf/mu4e-tweak)

  ;========================= map irc channels to icons =========================

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

  ;================ customize segments to put in main  modeline ================

  (doom-modeline-def-modeline 'smf/main
    '(bar workspace-number window-number god-state xah-fly-keys matches buffer-info remote-host parrot selection-info)
    '(misc-info persp-name lsp irc mu4e github debug minor-modes process vcs checker))

  ;=============================== misc settings ===============================

  (setq
   ;; doom-modeline takes care of this so disable doom's own python modeline
   +python-mode-line-indicator           nil
   doom-modeline-buffer-file-name-style  'truncate-upto-root
   doom-modeline-irc-stylize             #'smf/irc-icons
   doom-modeline-github                  t
   doom-modeline-checker-simple-format   t)

  ;; hook to get correct github auth info
  (add-hook 'doom-modeline-before-github-fetch-notification-hook
            (lambda () (smf/bitwarden-init))))
