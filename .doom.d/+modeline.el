;;; +modeline.el -*- lexical-binding: t; -*-

(after! doom-modeline

;;; segment tweaks

  (defun smf/mu4e-tweak (prop &rest _)
    "Trim all space and add a tiny space to the right side."
    (when prop
      (concat (s-trim prop)
              (propertize " " 'display '(space-width 0.5)))))

  (advice-add (alist-get 'mu4e doom-modeline-fn-alist)
              :filter-return #'smf/mu4e-tweak)

;;; customize segments to put in main modeline

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
