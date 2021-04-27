(setq vterm-shell (executable-find "xonsh"))

;; TODO upstream something to fix the autocomplete face color
;; https://github.com/hlissner/emacs-doom-themes/issues/324#issuecomment-587164161
(after! vterm
  (set-face-attribute 'vterm-color-black nil
                      :background "#9099AB") ;; a very bright grey
  (add-hook! vterm-mode #'goto-address-mode)
  (add-hook! vterm-mode (setq-local font-lock-keywords-only t)))

(defun =vterm ()
  "Start vterm app.

This of this as a replacement for your terminal app. It will
create a workspace called *vterm* and switch to it."
  (interactive)
  (require 'vterm)
  (+workspace-switch "*vterm*" t)
  (if (get-buffer "*vterm*")
      (switch-to-buffer "*vterm*")
    (+vterm/here "~")))


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
