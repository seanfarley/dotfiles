;;; ~/projects/dotfiles/doom/+lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(after! lsp-mode
  (setq lsp-pyls-configuration-sources ["flake8"]
        lsp-pyls-plugins-pylint-enabled nil
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-flake8-enabled t
        lsp-ui-doc-enable t))

;====== helpful snippet to not start a process and just connect to a port ======

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
