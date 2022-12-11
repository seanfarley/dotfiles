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

(add-hook! (emacs-lisp-mode ielm-mode)
           #'flycheck-package-setup)

(add-hook! (list python-mode clojure-mode hy-mode js2-mode
                 julia-mode racket-mode scheme-mode)
           (setq-local lispy-no-space t)
           (lispy-mode t))

(after! flycheck
  (add-to-list 'flycheck-gfortran-include-path "."))

(after! page-break-lines-mode
  (add-to-list 'page-break-lines-modes 'prog-mode))

(after! shr
  (setq shr-color-visible-luminance-min 80))

(setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)

(add-hook! prog-mode (display-fill-column-indicator-mode t))

(add-hook! doom-first-input
           #'fancy-narrow-mode
           #'global-dot-mode
           #'whole-line-or-region-global-mode
           #'global-page-break-lines-mode)

(setq split-height-threshold nil
      split-width-threshold 0)

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
  :init
  (after! highlight-indent-guides
    (setq hl-sexp-background-color (face-attribute
                                    'highlight-indent-guides-odd-face
                                    :background))))

(use-package! tramp-yadm
  :after (tramp projectile)
  :config
  (setq +vc-gutter-in-remote-files t)
  :init
  (tramp-yadm-register))

(use-package! outline-minor-faces
  :after outline
  :config
  (setq lispy-font-lock-keywords nil)
  (add-hook 'outline-minor-mode-hook
            #'outline-minor-faces-mode))

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

