;;; ~/projects/dotfiles/doom/+mpd.el -*- lexical-binding: t; -*-

;; always load simple-mpc so that the play, pause, next, etc. functions are
;; loaded
(require 'simple-mpc)

(defun =mpc ()
  "Start simple-mpc app.

This of this as a replacement for your music app. It will
create a workspace called *simple-mpc* and switch to it."
  (interactive)
  (+workspace-switch "*simple-mpc*" t)
  (if (get-buffer "*simple-mpc-main*")
      (switch-to-buffer "*simple-mpc-main*")
    (simple-mpc)))

(setq simple-mpc-playlist-format "%artist%	%album%	%title%"
      simple-mpc-table-separator "	")
