;;; ~/projects/dotfiles/doom/+ui.el -*- lexical-binding: t; -*-

;; optionally specifiy :weight 'light
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14))

(after! hl-line
  ;; doom set this for a tiny speed boost but it breaks highlighting in mu4e
  ;; header-view
  (setq hl-line-sticky-flag t
        global-hl-line-sticky-flag t))

;; icomoon allows use of custom ranges so just upload there and make sure that
;; range matches the range below
(when (fboundp 'set-fontset-font)
  (add-hook! 'doom-init-ui-hook
    (set-fontset-font "fontset-default" '(#xe900 . #xe902) "smf-custom-icons")))

;; ;; example to test custom font; along with how to type raw unicode
;; (insert       (propertize "\ue900"
;;                   'face '(:family "smf-custom-icons")
;;                   'rear-nonsticky t
;;                   'display '(raise -0.1)
;;                   'font-lock-ignore t))

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
  (defun smf/switch-to-last-workspace ()
    "Function to switch to last used workspace.

By 'last used,' I mean the last workspace that isn't an app. For
simplicity, just test if the workspace begins with an asterik."
    (when-let ((mru-list (cl-remove-if (lambda (it)
                                         (or (string-prefix-p "*" it)
                                             (string= "none" it)))
                                       persp-names-cache)))
      (persp-frame-switch (car mru-list))))

  (defun smf/activate-emacs ()
    "When activating emacs, only switch to last workspace when in an app."
    (when (string-prefix-p "*" (+workspace-current-name))
      (smf/switch-to-last-workspace)))

  (advice-add #'persp-frame-switch :after #'smf/auto-activate-venv))

;;
;;; Scrolling Tweaks

(setq hscroll-margin 2
      hscroll-step 1

      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 3

      ;; NOTE: optimally, this would be set to true, but it seems to cause
      ;; issues with performance and cursor jumping when scrolling.
      scroll-preserve-screen-position nil

      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil

      ;;scroll-up-aggressively 0.01
      ;;scroll-down-aggressively 0.01

      ;; mouse
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling
