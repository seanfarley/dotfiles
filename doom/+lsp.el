;;; ~/projects/dotfiles/doom/+lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(def-package! lsp-python-ms
  :config
  ;; for dev build of language server
  (setq lsp-python-ms-dir (expand-file-name "python-language-server/"
                                            doom-cache-dir)

        lsp-python-ms-cache-dir (expand-file-name ".lsp-python/"
                                                  doom-cache-dir)))

(after! lsp
  (require 'lsp-python-ms)
  (setq lsp-ui-doc-enable t))
