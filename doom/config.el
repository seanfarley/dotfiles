(setq server-use-tcp t)

;; custom fonts

;; icomoon allows use of custom ranges so just upload there and make sure that
;; range matches the range below
(when (fboundp 'set-fontset-font)
  (add-hook! 'doom-init-ui-hook
    (set-fontset-font "fontset-default" '(#xe900 . #xe902) "smf-custom-icons")))

;; don't prompt please
(setq confirm-kill-emacs nil)

;; really convenient mode for copying the whole line or region
(whole-line-or-region-global-mode)

;; save and restore the scratch buffer
(def-package! persistent-scratch
  :init
  (setq persistent-scratch-save-file
        (expand-file-name "persistent-scratch"
                          doom-cache-dir))

  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode)

  (defun smf/scratch-init ()
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))

  (add-hook 'doom-scratch-buffer-hook #'smf/scratch-init))

;; automatically add some buffers to the workspace
(after! persp-mode
  (defun smf/unreal-buffer-p (buffer-or-name)
    "Wrap `doom-unreal-buffer-p' but allow circe buffers through."
    (unless (derived-mode-p 'circe-mode)
      (doom-unreal-buffer-p buffer-or-name)))

  (setq persp-add-buffer-on-after-change-major-mode-filter-functions
        '(smf/unreal-buffer-p)))

;; enable hs-minor-mode
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; smart-jump
(smart-jump-setup-default-registers)

;; bitwardn password auth-source
(def-package! bitwarden
  :config

  (setq bitwarden-user "sean@farley.io"

        bitwarden-automatic-unlock (let* ((auth-sources
                                           '(macos-keychain-internet))
                                          (matches (auth-source-search
                                                    :user "sean@farley.io"
                                                    :host "bitwarden.farley.in"
                                                    :require '(:secret)
                                                    :max 1))
                                          (entry (nth 0 matches)))
                                     (plist-get entry :secret)))
  (bitwarden-auth-source-enable))

;; magit should use auth-source
(add-hook 'magit-process-find-password-functions
          'magit-process-password-auth-source)

;; (setq doom-theme 'doom-snazzy)

;; nifty package for popping up a quick emacs frame
(def-package! yequake
  :config
  (setq yequake-frames
      '(("Yequake & scratch" .
         ((name . "Yequake & scratch")
          (width . 0.75)
          (height . 0.5)
          (alpha . 0.75)
          (buffer-fns . ("~/projects/dotfiles/doom/config.el"
                         split-window-horizontally
                         "*scratch*"))
          (frame-parameters . ((undecorated . t))))))))

;; load personal modules
(load! "+utils")
(load! "+mac")
(load! "+bindings")
(load! "info+")
(load! "+ivy")
(load! "+org")
(load! "+org-starter")
(load! "+irc")
(load! "+twitter")
(load! "+mail")
(load! "+modeline")
