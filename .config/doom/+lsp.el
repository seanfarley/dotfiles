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

  ;; never seems to work for me; always hangs with a reentrant error (and recent
  ;; lsp-mode commits don't fix it); so just set to a bogus executable
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(cc-mode c++-mode)
                    :remote? t
                    :server-id 'cc-remote))

  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu))

(after! lsp-mode
  (setq lsp-pylsp-configuration-sources ["ruff"]
        lsp-pylsp-plugins-black-enabled t
        ;; pythong-lsp-ruff needs to disable the other linters
        lsp-pylsp-plugins-pylint-enabled nil
        lsp-pylsp-plugins-flake8-enabled nil
        lsp-pylsp-plugins-mccabe-enabled nil
        lsp-pylsp-plugins-pydocstyle-enabled nil
        lsp-pylsp-plugins-pycodestyle-enabled nil

        lsp-signature-auto-activate nil
        lsp-ui-doc-enable t)

  (add-hook! lsp-mode #'lsp-headerline-breadcrumb-mode))

  (add-hook! python-mode (setq-local flycheck-disabled-checkers '(python-pylint)))

;; helpful snippet to not start a process and just connect to a port
;; (defun lsp-tcp-connect-to-port ()
;;   (list
;;    :connect (lambda (filter sentinel name environment-fn)
;;               (let* ((host "localhost")
;;                      (port (string-to-number (read-string "Enter port: ")))
;;                      (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))

;;                 (set-process-query-on-exit-flag tcp-proc nil)
;;                 (set-process-filter tcp-proc filter)
;;                 (cons tcp-proc tcp-proc)))
;;    :test? (lambda () t)))

;; (require 'lsp)
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-tcp-connect-to-port)
;;                   :major-modes '(gdscript-mode)
;;                   :priority -1
;;                   :server-id 'gdscript-ls))

(after! lsp-python-ms
  (set-lsp-priority! 'mspyls -100))

(after! lsp-ui
  ;; for some reason doom sets these too small
  (setq lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 150))
