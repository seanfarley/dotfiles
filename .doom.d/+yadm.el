;;; +yadm.el -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'tramp-sh)

(defconst tramp-yadm-method "yadm"
  "When this method name is used, forward all calls to yadm logic.")

;; copied from `tramp-sh' but changed the bare minimum (only two so far)
(defvar tramp-yadm-file-name-handler-alist
  (cl-copy-list tramp-sh-file-name-handler-alist))

(add-to-list 'tramp-yadm-file-name-handler-alist
             '(expand-file-name . tramp-yadm-handle-expand-file-name))

(add-to-list 'tramp-yadm-file-name-handler-alist
             '(shell-command . tramp-yadm-handle-shell-command))

(defun tramp-yadm-file-name-p (filename)
  "Check if it's a FILENAME for yadm."
  (and (tramp-tramp-file-p filename)
       (string= (tramp-file-name-method
                 (tramp-dissect-file-name filename))
                tramp-yadm-method)))

(defun tramp-yadm-file-name-handler (operation &rest args)
  "Invoke the yadm handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION."
  (if-let ((fn (assoc operation tramp-yadm-file-name-handler-alist)))
      (save-match-data (apply (cdr fn) args))
    (tramp-run-real-handler operation args)))

(tramp-register-foreign-file-name-handler
 #'tramp-yadm-file-name-p #'tramp-yadm-file-name-handler)

(defun tramp-yadm-handle-expand-file-name (name &optional dir)
  "Like `tramp-sh-handle-expand-file-name' for yadm files.
Since yadm has a non-default GIT_DIR, we need to map it back to .git."
  (if (and (length dir)
           (string= name ".git")
           (string= (getenv "HOME") (tramp-file-name-localname
                                     (tramp-dissect-file-name
                                      (directory-file-name
                                       (expand-file-name dir))))))
      (expand-file-name "/yadm::~/.local/share/yadm/repo.git")
    (tramp-sh-handle-expand-file-name name dir)))

(defun tramp-yadm-handle-shell-command (command &optional output-buffer error-buffer)
  "Like `shell-command' for yadm files.

This only exists to intercept the `projectile-git-command' and
remove '-o' from the arguments."
  (let ((command (if (string= command projectile-git-command)
                     "git ls-files -zc --exclude-standard"
                   command)))
    (tramp-handle-shell-command command output-buffer error-buffer)))


(add-to-list 'tramp-methods
             `(,tramp-yadm-method
               (tramp-login-program "yadm")
               (tramp-login-args (("enter")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-args ("-c"))))

(after! projectile
  (add-to-list 'projectile-known-projects "/yadm::~")
  (add-to-list 'projectile-known-projects "/ssh:euclid:~/projects/phd-euclid"))
