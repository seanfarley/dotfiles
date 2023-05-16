;;; +modeline.el -*- lexical-binding: t; -*-

(after! doom-modeline

;;; segment tweaks

  (defadvice! smf/mu4e-tweak (prop &rest _)
    :filter-return (alist-get 'mu4e doom-modeline-fn-alist)
    "Trim all space and add a tiny space to the right side."
    (when prop
      (concat (s-trim prop)
              (propertize " " 'display '(space-width 0.5)))))

  (setq
   doom-modeline-buffer-file-name-style  'truncate-upto-root
   doom-modeline-buffer-encoding         nil
   doom-modeline-percent-position        nil
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
