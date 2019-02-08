;;; ~/projects/dotfiles/doom/+utils.el -*- lexical-binding: t; -*-


(defun +boy/switch-to-last-window ()
  "Switch to the previously selected window, skipping any other window in between."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found."))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

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
