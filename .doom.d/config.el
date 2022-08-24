(setq server-use-tcp t
      server-port    9999)

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(setq source-directory (expand-file-name "~/projects/emacs"))

(setq langtool-bin "/usr/local/bin/languagetool")

;; enable hs-minor-mode
(add-hook! prog-mode
           #'hs-minor-mode)

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

(smf/bitwarden-init)

;; magit should use auth-source
(add-hook 'magit-process-find-password-functions
          'magit-process-password-auth-source)

(add-to-list 'auto-mode-alist '("ssh/config\\'" . ssh-config-mode))
(add-to-list 'auto-mode-alist '("private_readonly_config\\'" . ssh-config-mode))
(add-hook! ssh-config-mode #'display-line-numbers-mode)

(add-to-list 'auto-mode-alist '("bash_.*\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("zsh.*\\'" . sh-mode))

(after! magit
  ;; magit-todo ignore json files due to huge performance hit
  (setq magit-todos-exclude-globs '("*.json")
        magit-clone-default-directory "~/projects/"
        ;; already taken care of by git settings
        magit-commit-show-diff nil)

  (when (equal (plist-get (nth 2 (transient-get-suffix 'magit-commit "x"))
                          :command)
               #'magit-commit-autofixup)

    ;; see https://github.com/magit/magit/issues/3723 for explanation of
    ;; behavior for prompting the user
    (transient-replace-suffix #'magit-commit #'magit-commit-autofixup
      '("x" "Absorb changes" magit-commit-absorb))))

(after! git-commit
  (setq git-commit-summary-max-length 80))

(add-hook! (emacs-lisp-mode ielm-mode)
           #'elisp-slime-nav-mode
           #'flycheck-package-setup)

(after! flycheck
  (add-to-list 'flycheck-gfortran-include-path "."))

(after! page-break-lines-mode
  (add-to-list 'page-break-lines-modes 'prog-mode))

(defun smf/fort-to-c ()
  "Convert fortran-style array indices to C-style."
  (interactive)
  (let ((overlay (embrace--delete (string-to-char "(") t)))
    (embrace--insert (string-to-char "[") overlay)))

(add-hook! python-mode #'sphinx-doc-mode)

;; TODO make +python module
;; TODO investigate django packages and incorporate them
(add-hook! +web-django-mode (prettify-symbols-mode -1))

(load-theme 'doom-nord t)
(after! shr
  (setq shr-color-visible-luminance-min 80))

(setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)

(after! which-key-mode
  (which-key-posframe-mode))

(add-hook! doom-first-input
           #'fancy-narrow-mode
           #'global-dot-mode
           #'whole-line-or-region-global-mode
           #'global-page-break-lines-mode)

(after! prodigy
  (let ((launchd-dbus (string-trim
                       (shell-command-to-string
                        "launchctl getenv DBUS_LAUNCHD_SESSION_BUS_SOCKET"))))
    (prodigy-define-service
      :name "Pantalaimon"
      :command "pantalaimon"
      :args `("-c" ,(expand-file-name "~/.config/pantalaimon/pantalaimon.conf"))
      :env `(("DBUS_SESSION_BUS_ADDRESS" ,(concat "unix:path=" launchd-dbus)))
      :tags '(matrix)
      :stop-signal 'sigkill
      :kill-process-buffer-on-stop t)))

(use-package! highlight-sexp
  :hook ((lisp-mode . highlight-sexp-mode)
         (emacs-lisp-mode . highlight-sexp-mode)
         (ielm-mode . highlight-sexp-mode)
         (scheme-mode . highlight-sexp-mode)
         (racket-mode . highlight-sexp-mode)
         (hy-mode . highlight-sexp-mode)
         (lfe-mode . highlight-sexp-mode)
         (dune-mode . highlight-sexp-mode)
         (clojure-mode . highlight-sexp-mode)
         (fennel-mode . highlight-sexp-mode))
  :commands (highlight-sexp-mode)
  :config
  (setq hl-sexp-background-color (face-attribute
                                  'magit-blame-highlight
                                  :background)))

(after! vundo
    (setq vundo-glyph-alist vundo-unicode-symbols))

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
(load! "+yadm")

;; TODO don't know where to put this? ui?
(setq compilation-scroll-output t)
