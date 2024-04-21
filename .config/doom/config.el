;;; config.el -*- lexical-binding: t; -*-

(setq server-use-tcp t
      server-port    9999)

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(setq source-directory (expand-file-name "~/projects/emacs"))

(setq projectile-project-search-path (list "~/projects/"))

(defmacro smf/bitwarden-init ()
  "Needs to be a macro due to async.

Async doesn't load your entire config (for performance reasons)
so this needs to be a macro and added to async hooks.

Add it to a hook like so:
  (add-hook 'doom-modeline-before-github-fetch-notification-hook
            (lambda () (smf/bitwarden-init)))"
  `(progn
     (require 'bitwarden nil t)
     (setq bitwarden-user "sean@farley.io"

           bitwarden-automatic-unlock (let* ((auth-sources
                                              '(macos-keychain-internet))
                                             (matches (auth-source-search
                                                       :user "sean@farley.io"
                                                       :host "bitwarden.farley.io"
                                                       :require '(:secret)
                                                       :max 1))
                                             (entry (nth 0 matches)))
                                        (plist-get entry :secret)))
     (bitwarden-auth-source-enable)))

(use-package! bitwarden
  :after auth-source
  :init
  (smf/bitwarden-init))

(add-hook! ssh-config-mode #'display-line-numbers-mode)

(after! flycheck
  (add-to-list 'flycheck-gfortran-include-path "."))

(after! shr
  (setq shr-color-visible-luminance-min 80
        shr-max-width 80))

(setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)

(add-hook! prog-mode (display-fill-column-indicator-mode t))

(add-hook! doom-first-input
           #'+default/restart-server
           #'global-color-identifiers-mode
           #'whole-line-or-region-global-mode)

(setq split-height-threshold nil
      split-width-threshold 0)

(use-package! yadm
  :after (projectile magit)
  :init
  (advice-remove #'projectile-get-ext-command
                 #'doom--only-use-generic-command-a)
  (advice-add #'+vertico-file-search
              :around #'yadm-doom-vertico-file-search))

(after! smartparens
  (setq sp-navigate-reindent-after-up-in-string nil))

;; NOTE https://github.com/joaotavora/sly/issues/535
(after! orderless
  (setq-hook! 'sly-mode-hook completion-styles
              '(basic partial-completion emacs22)))

(use-package! buffer-name-relative
  :hook (doom-first-buffer . buffer-name-relative-mode))

;; load personal modules
(load! "+utils")
(load! "+bindings")
(load! "+org")
(load! "+mail")
(load! "+modeline")
(load! "+lsp")
(load! "+ui")
(load! "+vterm")
(load! "+ansible")
(load! "+magit")
(load! "+biblio")

