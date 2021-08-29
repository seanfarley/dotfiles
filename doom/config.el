(setq server-use-tcp t
      server-port    9999)

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; in case emacs crashed, forcibly delete the server connection and restart it
(server-force-delete)
(server-start)

(setq source-directory (expand-file-name "~/projects/emacs"))

;; enable hs-minor-mode
(add-hook! prog-mode
           #'hs-minor-mode
           #'org-link-minor-mode)

(setq projectile-project-search-path (list "~/projects/"))

(defmacro smf/bitwarden-init ()
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

;; bitwardn password auth-source
(add-hook 'doom-init-ui-hook (lambda () (smf/bitwarden-init)))

;; magit should use auth-source
(add-hook 'magit-process-find-password-functions
          'magit-process-password-auth-source)

(add-hook! artist-mode (display-line-numbers-mode -1))

(add-to-list 'auto-mode-alist '("ssh/config\\'" . ssh-config-mode))
(add-hook! ssh-config-mode #'display-line-numbers-mode)

(after! magit
  ;; magit-todo ignore json files due to huge performance hit
  (setq magit-todos-exclude-globs '("*.json"))

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

(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

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
(load! "+gnus-patch")

;; TODO don't know where to put this? ui?
(setq compilation-scroll-output t)
