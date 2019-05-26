;;; ~/projects/dotfiles/doom/+lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(after! lsp-python-ms

  ;; for dev build of language server
  (setq lsp-python-ms-dir (expand-file-name "python-language-server/"
                                            doom-cache-dir)

        lsp-python-ms-cache-dir (expand-file-name ".lsp-python/"
                                                  doom-cache-dir))

  (defcustom lsp-python-ms-dotnet-install-dir
    (locate-user-emacs-file "lsp-python-ms/dotnet/")
    "Install directory for dotnet."
    :group 'lsp-python-ms
    :risky t
    :type 'directory)

  (defun lsp-python-ms--locate-dotnet()
  "Return dotnet's path.If not found, ask the user whether to install."
  (let ((dotnet-exe (or
                     ;; specified installation path
                     (and (file-directory-p lsp-python-ms-dotnet-install-dir)
                          (car (directory-files lsp-python-ms-dotnet-install-dir t "^dotnet\\(\\.exe\\)?$")))
                     ;; system path
                     (executable-find "dotnet"))))
    (unless (and dotnet-exe
                 (not (string-empty-p (shell-command-to-string (format "%s --list-runtimes" dotnet-exe))))
                 (not (string-empty-p (shell-command-to-string (format "%s --list-sdks" dotnet-exe)))))
      (error "Dotnet sdk not found!"))
    dotnet-exe))

  (defun lsp-python-ms--ensure-server()
    "Compile Microsoft Python Language Server in `lsp-python-ms-dir'."
    (let* ((dotnet (lsp-python-ms--locate-dotnet))
           (default-directory lsp-python-ms-dir)
           (command)
           (log))
      (when (file-directory-p default-directory)
        (delete-directory default-directory t))
      (mkdir default-directory t)
      (setq command "git clone --depth 1 https://github.com/Microsoft/python-language-server.git")
      (message "clone server source: %s" command)
      (setq log (shell-command-to-string command))
      (message "%s\n" log)
      (setq command (format "%s build -c Release  python-language-server/src/LanguageServer/Impl" dotnet))
      (message "build server: %s" command)
      (setq log (shell-command-to-string command))
      (message "%s\n" log)
      (with-temp-buffer
        (insert log)
        (goto-char (point-min))
        (unless (search-forward-regexp "Build succeeded." nil t)
          (error "Build server failed!You can check log message in *MESSAGE* buffer!"))
        (copy-directory "python-language-server/output/bin/Release" default-directory t t t)
        (when (file-directory-p "python-language-server")
          (delete-directory "python-language-server" t))
        (message "Build server finished."))))

  (defun lsp-python-ms-update-server ()
    "Update Microsoft python language server."
    (interactive)
    (message "Server update started...")
    (lsp-python-ms--ensure-server)
    (message "Server update finished...")))

(after! lsp
  (require 'lsp-python-ms)
  (setq lsp-ui-doc-enable t))
