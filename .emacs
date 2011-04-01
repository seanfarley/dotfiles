; Emacs Load Path
; (setq load-path (cons "~/.emacs.d" load-path))
; (add-to-list 'load-path "~/.emacs.d/color-theme-sunburst.el")

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/plugins")

; Set whitespace on
(require 'whitespace)
(custom-set-variables
  '(whitespace-style (quote (face tabs spaces trailing lines space-before-tab indentation empty space-after-tab space-mark tab-mark)))
  '(global-whitespace-mode t)
)

; Change the color-theme
(require 'color-theme)
(color-theme-initialize)
; Load the zenburn color-theme from
; https://github.com/bbatsov/zenburn-emacs
(require 'zenburn)
(zenburn)

; Highlight both the row and column
(require 'crosshairs)
(custom-set-variables '(crosshairs-mode t))

; Try to fix the craziness of customization
; (require 'cus-edit+)
; (customize-toggle-outside-change-updates 99)
; (remove-hook 'kill-emacs-query-functions 'customize-customized)
