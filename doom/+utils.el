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

(defun smf/comment-or-uncomment-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun smf/delete-to-end-of-buffer (add-to-kill-ring-p)
  "Deletes from point to end of buffer. If prefix argument is
     given, kill the region, adding it to the kill ring."
  (interactive "P")
  (if add-to-kill-ring-p
      (kill-region (point) (point-max))
    (delete-region (point) (point-max))))

(defun smf/s-blank? (s)
  "Is S nil or the empty string?"
  (declare (pure t) (side-effect-free t))
  (or (null s) (string= "" s)))

(defun smf/s-present? (s)
  "Is S anything but nil or the empty string?"
  (declare (pure t) (side-effect-free t))
  (not (smf/s-blank? s)))
