(setq server-use-tcp t
      server-port    9999)

(setq source-directory (expand-file-name "~/projects/emacs"))

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

(add-hook! artist-mode (display-line-numbers-mode -1))

;; magit-todo ignore json files
(after! magit
  (setq magit-todos-exclude-globs '("*.json")))

(after! git-commit
  (setq git-commit-summary-max-length 80))

(add-hook! (emacs-lisp-mode ielm-mode)
           #'elisp-slime-nav-mode)
(add-hook! python-mode #'sphinx-doc-mode)


(add-hook! jinja2-mode (remove-hook after-save-hook 'jinja2-indent-buffer))

(load-theme 'doom-nord t)
(after! shr
  (setq shr-color-visible-luminance-min 80))

(after! which-key-mode
  (which-key-posframe-mode))

(add-hook! after-init
           #'fancy-narrow-mode
           #'global-dot-mode
           #'whole-line-or-region-global-mode
           #'global-page-break-lines-mode)


;; load personal modules
(load! "+utils")
(load! "+bindings")
(load! "+org")
(load! "+twitter")
(load! "+mail")
(load! "+modeline")
(load! "+lsp")
(load! "+ui")
(load! "+vterm")
(load! "+ansible")
