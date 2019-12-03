;;; ~/projects/dotfiles/doom/+lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(after! lsp
  (setq lsp-pyls-configuration-sources ["flake8"]
        lsp-pyls-plugins-pylint-enabled nil
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-flake8-enabled t
        lsp-ui-doc-enable t))
