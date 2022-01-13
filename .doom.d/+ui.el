;;; ~/projects/dotfiles/doom/+ui.el -*- lexical-binding: t; -*-

;; optionally specifiy :weight 'light
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14)
      doom-unicode-font (font-spec :family "FiraCode Nerd Font" :size 14))

(after! hl-line
  ;; doom set this for a tiny speed boost but it breaks highlighting in mu4e
  ;; header-view
  (setq hl-line-sticky-flag t
        global-hl-line-sticky-flag t))

;; automatically start in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . fullboth))

;; BUG below line doesn't check `ns-use-native-fullscreen'
;; (progn (setq ns-use-native-fullscreen nil) (add-to-list 'initial-frame-alist '(fullscreen . fullboth)))

(defun smf/auto-activate-venv (&rest _)
  "Automatically activate virtualenv of same name, if one exists."
  (pyvenv-deactivate)
  (when-let ((workon-home (pyvenv-workon-home))
             (window-name (safe-persp-name (get-current-persp))))
    (if (not (file-directory-p workon-home))
        (error "Can't find a workon home directory, set $WORKON_HOME"))
    (dolist (name (f-directories workon-home))
      (when (string= (file-name-nondirectory name) window-name)
        (pyvenv-activate name)))))

;; keep persp names in order of most recently used
(after! persp-mode
  (add-hook 'persp-before-switch-functions
            #'(lambda (new-persp-name w-or-f)
                (let ((cur-persp-name (safe-persp-name (get-current-persp))))
                  (when (member cur-persp-name persp-names-cache)
                    (setq persp-names-cache
                          (cons cur-persp-name
                                (delete cur-persp-name persp-names-cache)))))))

  (add-hook 'persp-renamed-functions
            #'(lambda (persp old-name new-name)
                (setq persp-names-cache
                      (cons new-name (delete old-name persp-names-cache)))))

  (add-hook 'persp-before-kill-functions
            #'(lambda (persp)
                (setq persp-names-cache
                      (delete (safe-persp-name persp) persp-names-cache))))

  (add-hook 'persp-created-functions
            #'(lambda (persp phash)
                (when (and (eq phash *persp-hash*)
                           (not (member (safe-persp-name persp)
                                        persp-names-cache)))
                  (setq persp-names-cache
                        (cons (safe-persp-name persp) persp-names-cache)))))

  ;; the above keeps the persp cache in MRU order

  (defun smf/workspace-mru ()
    "Returns the mru order of workspaces.

The function filters out workspaces that start with a '*'."
    (cl-remove-if (lambda (it)
                    (or (string-prefix-p "*" it)
                        (string= "none" it)))
                  persp-names-cache))

  (defun smf/switch-to-last-workspace ()
    "Function to switch to last used workspace.

By 'last used,' I mean the last workspace that isn't an app. For
simplicity, just test if the workspace begins with an asterik."
    (when-let ((mru-list (smf/workspace-mru)))
      (+workspace-switch (car mru-list))))

  (defun smf/activate-emacs ()
    "When activating emacs, only switch to last workspace when in an app."
    (when (string-prefix-p "*" (+workspace-current-name))
      (smf/switch-to-last-workspace)))

  (advice-add #'persp-frame-switch :after #'smf/auto-activate-venv)

  (defun smf/workspace-other ()
    (interactive)
    (when-let ((mru-list (smf/workspace-mru))
               (prev-workspace +workspace--last)
               (cur-workspace (+workspace-current-name)))
      ;; if we're coming from a previous *foo* workspace, the first entry in
      ;; `mru-list' is the current workspace; so use the second item

      (if (string-prefix-p (nth 0 mru-list) cur-workspace)
          (+workspace/switch-to (nth 1 mru-list))
        (+workspace/switch-to (nth 0 mru-list)))))

  (advice-add #'+workspace/other :override #'smf/workspace-other))
