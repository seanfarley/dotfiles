;;; +lsp.el -*- lexical-binding: t; -*-

;; mark lsp-format-buffer as safe for before-save-hook
(add-to-list 'safe-local-eval-forms '(add-hook 'before-save-hook
                                               #'lsp-format-buffer nil t))

(after! dap-mode
  (setq dap-python-executable "python3"
        dap-python-debugger 'debugpy)

  (map! :map dap-ui-repl-mode-map
        "C-p" #'comint-previous-input
        "C-n" #'comint-next-input))

(after! lsp-mode
  (setq
   lsp-signature-auto-activate nil
   lsp-ui-doc-enable t)

  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
  (add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode))

(after! lsp-ui
  ;; for some reason doom sets these too small
  (setq lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 150))
