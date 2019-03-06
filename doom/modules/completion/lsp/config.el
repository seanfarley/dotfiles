;;; completion/lsp/config.el -*- lexical-binding: t; -*-

(def-package! lsp-mode
  :commands (lsp-mode lsp-define-stdio-client)
  :init
  (require 'lsp)
  (require 'lsp-clients)
  :config
  (setq lsp-session-file (expand-file-name ".lsp-session-v1"
                                           doom-cache-dir))

  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (require 'lsp-ui)
  (require 'lsp-ui-imenu)
  :config
  (set-lookup-handlers! 'lsp-ui-mode
                        :definition #'lsp-ui-peek-find-definitions
                        :references #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t))

(def-package! company-lsp
  :after lsp-mode
  :config
  (set-company-backend! 'lsp-mode 'company-lsp)
  (setq company-lsp-enable-recompletion t))

(def-package! lsp-python-ms
  :when (featurep! +python)
  :hook (python-mode . lsp)
  :config

  ;; for dev build of language server
  (setq lsp-python-ms-dir (expand-file-name "python-language-server/"
                                            doom-cache-dir)

        lsp-python-ms-cache-dir (expand-file-name ".lsp-python/"
                                                  doom-cache-dir)))

(when (featurep! +python)
  (after! python
    (require 'lsp-python-ms)))
