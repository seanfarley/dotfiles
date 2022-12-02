;;; +ansible.el -*- lexical-binding: t; -*-

;; known limitations:
;;   - must have labels with the same name as the role
;;   - not sure about the UX for running the whole playbook
;;   - undecided on UX when can't find a role

;; current buffer is a file under one of the 'roles' subdirectories -> deploy just that role
;; current buffer is server.yml and cursor is on a role -> deploy just that role

;; not sure what to do when buffer is a file that is not under a 'role'; read
;; all the tags and then prompt the user?

(defvar ansible-process-buffer "*ansible-process*"
  "Name of the buffer for the ansible process. Set to nil to ignore.")

(defvar ansible-playbook-file "server.yml"
  "Name of the project's playbook file to be sent to the
  ansible-playbook command.

Caveats: I'm not sure every project has just one playbook that is
the 'main' one.")

(defun smf/ansible-proc-filter (proc str)
  ;; write output to process buffer for debugging
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert str)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))))

  (let ((str (s-trim str)))
    (when (string-match "^ERROR" str)
      (let* ((name (if (buffer-live-p (process-buffer proc))
                       (buffer-name (process-buffer proc))))
             (msg (format "Error (see the %s buffer): " name)))
        (message (concat msg str))))
    (when (string-match "^TASK \\[\\(.+\\)\\]" str)
      (message (match-string 1 str)))
    (when (string-match (concat "^.+: ok=\\([0-9]+\\)\\s-+"
                                "changed=\\([0-9]+\\)\\s-+"
                                "unreachable=\\([0-9]+\\)\\s-+"
                                "failed=\\([0-9]+\\)\\s-+"
                                "skipped=\\([0-9]+\\)\\s-+"
                                "rescued=\\([0-9]+\\)\\s-+"
                                "ignored=\\([0-9]+\\)") str)
      (message (concat "Ansible finished: "
                       "ok=%s, changed=%s, "
                       "unreachable=%s, failed=%s, "
                       "skipped=%s, rescued=%s, ignored=%s")
               (match-string 1 str)
               (match-string 2 str)
               (match-string 3 str)
               (match-string 4 str)
               (match-string 5 str)
               (match-string 6 str)
               (match-string 7 str)))))

(defun smf/spawn-ansible (role)
  (let ((default-directory (projectile-project-root))
        (cmd (list "ansible-playbook" "--tags" role
                   (if ansible-vault-password-file
                       (concat "--vault-password-file=" ansible-vault-password-file))
                   (concat (projectile-project-root) ansible-playbook-file))))
    (make-process :name "ansible-playbook"
                  :buffer ansible-process-buffer
                  :connection-type 'pipe
                  :command (cl-remove-if #'null cmd)
                  :filter #'smf/ansible-proc-filter)))

(defun smf/role-from-dir-name (buffer)
  (when buffer-file-name
    (with-current-buffer buffer
      (let* ((root (projectile-project-root))
             (dir (directory-file-name (file-name-directory buffer-file-name))))
        (while (and (not (string-match dir root))
                    (not (string-match "roles/\\([^/]+\\)$" dir)))
          (setq dir (directory-file-name
                     (file-name-directory dir))))
        (match-string 1 dir)))))

(defun smf/role-from-buffer ()
  (save-excursion
    (end-of-visual-line)
    ;; searching for 'role:' should be unique enough to identify the
    ;; server.yml file
    (re-search-forward "^.*- role: \\(.+\\)" nil t -1)
    (match-string-no-properties 1)))

(defun smf/run-ansible ()
  (interactive)
  (let ((role (smf/role-from-dir-name (buffer-name))))
    (when (and (not role)
               (string= (buffer-name) ansible-playbook-file))
      (with-current-buffer ansible-playbook-file
        (setq role (smf/role-from-buffer))))
    (if (not role)
        (message "Ansible: couldn't determine role")
      (smf/spawn-ansible role))))

(defun ansible-docsets ()
  (interactive)
  (setq-local dash-docs-docsets '("Ansible")))

(add-hook! +ansible-yaml-mode 'ansible-docsets)

(after! ansible
  (map!
   (:map ansible-key-map
    "C-c C-c"  #'smf/run-ansible))

  (setq ansible-vault-password-file "~/.vault_pass.txt"))
