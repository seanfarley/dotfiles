;;; ~/projects/dotfiles/doom/+utils.el -*- lexical-binding: t; -*-


;; deletes backward until a space is hit
(defun smf/backward-kill-word ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (kill-region (point) (mark))
    (progn
      (delete-region (point) (save-excursion (skip-syntax-backward " ") (point)))
      (delete-region (point) (save-excursion (skip-syntax-backward "^ ") (point))))))

(defun smf/delete-to-end-of-buffer (add-to-kill-ring-p)
  "Deletes from point to end of buffer. If prefix argument is
     given, kill the region, adding it to the kill ring."
  (interactive "P")
  (if add-to-kill-ring-p
      (kill-region (point) (point-max))
    (delete-region (point) (point-max))))

(defun smf/make-frame ()
  "Make a new, fullscreen frame."
  (interactive)
  (let ((frame (make-frame)))
    (select-frame frame)
    (toggle-frame-fullscreen)))
