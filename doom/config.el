(setq server-use-tcp t
      server-port    9999)

;; custom fonts

;; don't prompt please
(setq confirm-kill-emacs nil)

;; really convenient mode for copying the whole line or region
(whole-line-or-region-global-mode)

;; enable hs-minor-mode
(add-hook 'prog-mode-hook #'hs-minor-mode)

(setq projectile-project-search-path '("~/projects/" "~/sandbox/"))

(require 'xonsh-mode)

(defmacro smf/bitwarden-init ()
  `(progn
     (require 'bitwarden nil t)
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
     (bitwarden-auth-source-enable)))

;; bitwardn password auth-source
(smf/bitwarden-init)

;; magit should use auth-source
(add-hook 'magit-process-find-password-functions
          'magit-process-password-auth-source)

(defun smf/git-commit-conventions (&rest _)
  "All opinions not my own are, by definition, bullshit."
  (setq git-commit-summary-max-length 80))
(add-hook 'git-commit-mode-hook #'smf/git-commit-conventions)
(advice-add #'+vc|enforce-git-commit-conventions :after
            #'smf/git-commit-conventions)

;; (setq doom-theme 'doom-snazzy)

(setq pdf-view-use-scaling t)

(fancy-narrow-mode)

(setq vterm-shell (executable-find "xonsh"))

(def-package! webpaste
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)))

(add-hook 'after-init-hook 'global-color-identifiers-mode)

(which-key-posframe-mode)

;; load personal modules
(load! "+utils")
(load! "+bindings")
(load! "+org")
(load! "+irc")
(load! "+twitter")
(load! "+mail")
(load! "+modeline")
(load! "+lsp")
(load! "+ui")
