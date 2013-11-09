(defvar persistent-scratch-filename
    "~/.emacs.d/persistent-scratch"
    "Location of *scratch* file contents for persistent-scratch.")

(defvar persistent-scratch-backup-directory
    "~/.emacs.d/persistent-scratch-backups/"
    "Location of backups of the *scratch* buffer contents for
    persistent-scratch.")

(defun make-persistent-scratch-backup-name ()
  "Create a filename to backup the current scratch file by
  concatenating PERSISTENT-SCRATCH-BACKUP-DIRECTORY with the
  current date and time."
  (concat
   persistent-scratch-backup-directory
   (format-time-string "%y-%m-%d_%H-%M-%S_%s" (current-time))))

(defun save-persistent-scratch-backup ()
  "Save the current persistant scratch file into the backup
  directory"
  (interactive)
  (message "Making persistant scratch backup...")
  (if (not (file-exists-p persistent-scratch-backup-directory))
      (make-directory persistent-scratch-backup-directory))
  (if (file-exists-p persistent-scratch-filename)
      (copy-file persistent-scratch-filename
                 (make-persistent-scratch-backup-name))))

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
  PERSISTENT-SCRATCH-FILENAME"
  (interactive)
  (with-current-buffer (get-buffer "*scratch*")
    ;; Only save if modified bit is set
    (if (buffer-modified-p)
        (progn (message "Saving scratch")
               (write-region (point-min) (point-max)
                             persistent-scratch-filename)
               ;; mark as not modified
               (set-buffer-modified-p nil)))))

(defun load-persistent-scratch ()
  "Load the contents of PERSISTENT-SCRATCH-FILENAME into the
  scratch buffer, clearing its contents first."
  (if (file-exists-p persistent-scratch-filename)
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (insert-file-contents persistent-scratch-filename))))

(load-persistent-scratch)
(push #'save-persistent-scratch kill-emacs-hook)
