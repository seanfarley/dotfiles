(setq vterm-shell (executable-find "xonsh"))

;; hack to fix the autocomplete face color
(after! vterm
  (set-face-attribute 'vterm-color-black nil
                      :background "#9099AB") ;; a very bright grey
  (add-hook 'vterm-mode-hook #'goto-address-mode)
  (add-hook 'vterm-mode-hook #'compilation-shell-minor-mode))

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


(map!
  (:map vterm-mode-map

    ;; TODO upstream this to doom
    [remap whole-line-or-region-yank] #'vterm-yank
    "S-SPC"                           (lambda ()
                                        (interactive)
                                        (vterm-send-key " "))
    "s-<backspace>"                   (lambda ()
                                        (interactive)
                                        (vterm-send-key "u" nil nil t))
    "M-<left>"                        (lambda ()
                                        (interactive)
                                        (vterm-send-key "b" nil t nil))
    "M-<right>"                       (lambda ()
                                        (interactive)
                                        (vterm-send-key "f" nil t nil))))
