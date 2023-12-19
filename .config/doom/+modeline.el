;;; +modeline.el -*- lexical-binding: t; -*-

(after! doom-modeline

  (setq
   doom-modeline-buffer-file-name-style  'truncate-upto-root
   doom-modeline-buffer-encoding         nil
   doom-modeline-percent-position        nil
   doom-modeline-github                  t
   doom-modeline-major-mode-icon         t
   doom-modeline-major-mode-color-icon   nil
   doom-modeline-persp-name              t
   doom-modeline-persp-name-icon         t
   doom-modeline-checker-simple-format   t)

  ;; hook to get correct github auth info
  (add-hook 'doom-modeline-before-github-fetch-notification-hook
            (lambda () (smf/bitwarden-init))))
