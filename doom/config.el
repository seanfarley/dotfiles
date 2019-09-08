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

(add-hook 'doom-init-ui-hook (lambda () (require 'xonsh-mode)))

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
(add-hook 'doom-init-ui-hook (lambda () (smf/bitwarden-init)))

;; magit should use auth-source
(add-hook 'magit-process-find-password-functions
          'magit-process-password-auth-source)

;; magit-todo ignore json files
(after! magit
  (setq magit-todos-exclude-globs '("*.json")))

(after! git-commit
  (setq git-commit-summary-max-length 80))

(load-theme 'doom-nord t)

(setq pdf-view-use-scaling t)

(fancy-narrow-mode)

(def-package! webpaste
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)))

(add-hook 'after-init-hook 'global-color-identifiers-mode)

(which-key-posframe-mode)

(require 'lsp-sourcekit)
(setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain")
;; (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain")
(setq lsp-sourcekit-executable "/Library/Developer/Toolchains/swift-5.1-DEVELOPMENT-SNAPSHOT-2019-08-27-a.xctoolchain/usr/bin/sourcekit-lsp")


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
(load! "+vterm")
