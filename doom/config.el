(setq server-use-tcp t
      server-port    9999)

(setq source-directory (expand-file-name "~/projects/emacs"))

;; really convenient mode for copying the whole line or region
(whole-line-or-region-global-mode)

;; enable hs-minor-mode
(add-hook! prog-mode
           #'hs-minor-mode
           #'color-identifiers-mode)

(setq projectile-project-search-path '("~/projects/" "~/sandbox/"))

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

(setq iedit-toggle-key-default nil)

;; magit-todo ignore json files
(after! magit
  (setq magit-todos-exclude-globs '("*.json")))

(after! git-commit
  (setq git-commit-summary-max-length 80))

(add-hook! (emacs-lisp-mode ielm-mode)
           #'elisp-slime-nav-mode)
(add-hook! python-mode #'sphinx-doc-mode)

(setq pdf-view-use-scaling t)

(load-theme 'doom-nord t)

(after! which-key-mode
  (which-key-posframe-mode))

(add-hook! after-init
           #'fancy-narrow-mode
           #'global-dot-mode
           #'global-jump-tree-mode
           #'global-page-break-lines-mode)


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
(load! "+ansible")
