;;; ~/projects/dotfiles/doom/+modeline.el -*- lexical-binding: t; -*-

(after! doom-modeline
  ;=============================== segment tweaks ==============================

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
     (t buffer)))

  ;================ customize segments to put in main  modeline ================

  ;; remove buffer-position, input-method, major-mode, and buffer-encoding
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host selection-info)
    '(objed-state misc-info persp-name irc mu4e github debug lsp process vcs checker))

  ;=============================== misc settings ===============================

  (setq
   doom-modeline-buffer-file-name-style  'truncate-upto-root
   doom-modeline-irc-stylize             #'smf/irc-icons
   doom-modeline-mu4e                    t
   doom-modeline-github                  t
   doom-modeline-major-mode-icon         t
   doom-modeline-major-mode-color-icon   nil
   doom-modeline-persp-name              t
   doom-modeline-persp-name-icon         t
   doom-modeline-checker-simple-format   t)

  ;; hook to get correct github auth info
  (add-hook 'doom-modeline-before-github-fetch-notification-hook
            (lambda () (smf/bitwarden-init))))
