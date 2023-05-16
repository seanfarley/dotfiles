;;; +modeline.el -*- lexical-binding: t; -*-

(after! doom-modeline

;;; segment tweaks

  (defadvice! smf/mu4e-tweak (prop &rest _)
    :filter-return (alist-get 'mu4e doom-modeline-fn-alist)
    "Trim all space and add a tiny space to the right side."
    (when prop
      (concat (s-trim prop)
              (propertize " " 'display '(space-width 0.5)))))

;;; customize segments to put in main modeline

  (doom-modeline-def-segment misc-info
    "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
    (when (or doom-modeline-display-misc-in-all-mode-lines
              (doom-modeline--active))
      '("" mode-line-misc-info)))

  ;; remove buffer-position, input-method, major-mode, and buffer-encoding
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host selection-info)
    '(objed-state misc-info persp-name mu4e github debug lsp process vcs checker))

;;; misc settings

  (setq
   doom-modeline-buffer-file-name-style  'truncate-upto-root
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
