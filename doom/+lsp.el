;;; ~/projects/dotfiles/doom/+lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(after! lsp
  ;; for dev build of language server (set before loading so that defvar uses
  ;; the correct values)
  (setq lsp-python-ms-dir (expand-file-name "python-language-server/"
                                            doom-cache-dir)

        lsp-python-ms-cache-dir (expand-file-name ".lsp-python/"
                                                  doom-cache-dir))

  (require 'lsp-python-ms)
  (setq lsp-ui-doc-enable t))
