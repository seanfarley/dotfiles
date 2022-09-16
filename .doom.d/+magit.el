;;; +magit.el -*- lexical-binding: t; -*-

(after! git-commit
  (setq git-commit-summary-max-length 80))

(after! magit
  ;; magit should use auth-source
  (add-hook 'magit-process-find-password-functions
            'magit-process-password-auth-source)

  ;; magit-todo ignore json files due to huge performance hit
  (setq magit-todos-exclude-globs '("*.json")
        magit-clone-default-directory "~/projects/")

  (when (equal (plist-get (nth 2 (transient-get-suffix 'magit-commit "x"))
                          :command)
               #'magit-commit-autofixup)

    ;; see https://github.com/magit/magit/issues/3723 for explanation of
    ;; behavior for prompting the user
    (transient-replace-suffix #'magit-commit #'magit-commit-autofixup
      '("x" "Absorb changes" magit-commit-absorb))))
