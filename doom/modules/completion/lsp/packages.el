;; -*- no-byte-compile: t; -*-
;;; completion/lsp/packages.el

(when (package! lsp-mode)
  (package! lsp-ui)
  (package! company-lsp)

  ;; disable doom packages that lsp already has
  (package! tide :disable t)
  (package! racer :disable t)
  (package! company-shell :disable t)
  (package! anaconda-mode :disable t)

  ;; prefer ms-python ironically enough
  (when (featurep! +python)
    (package! lsp-python-ms
      :recipe (:fetcher
               github
               :repo "andrew-christianson/lsp-python-ms"))))
