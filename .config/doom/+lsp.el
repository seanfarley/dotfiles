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
  (add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)

  ;; https://github.com/blahgeek/emacs-lsp-booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(after! lsp-ui
  ;; for some reason doom sets these too small
  (setq lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 150))
