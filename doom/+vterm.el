(setq vterm-shell (executable-find "xonsh"))

;; hack to fix the autocomplete face color
(after! vterm
  (set-face-attribute 'vterm-color-black nil
                      :background "#9099AB")) ;; a very bright grey

(defun =vterm ()
  "Start vterm app.

This of this as a replacement for your terminal app. It will
create a workspace call *vterm* and switch to it."
  (interactive)
  (require 'vterm)
  (+workspace-switch "*vterm*" t)
  (if (get-buffer "vterm")
      (switch-to-buffer "vterm")
    (vterm)))
