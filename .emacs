; Emacs Load Path
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/auto-install")

; Add modifier keys for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

; Don't save backup files
(setq make-backup-files nil)

; Load auto-install
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa")
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; Set whitespace on
(require 'whitespace)
(custom-set-variables
  '(whitespace-style (quote (face tabs spaces trailing lines space-before-tab indentation empty space-after-tab tab-mark)))
  '(global-whitespace-mode t)
)

; Change the color-theme
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-solarized-dark)

; Highlight both the row and column
(require 'crosshairs)
(custom-set-variables '(crosshairs-mode t))

; Change the right command button to meta
(custom-set-variables '(ns-right-command-modifier (quote meta)))

; YASnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(yas/global-mode 1)

(require 'gtags)
; (require 'tex-site)

; Load ido
(require 'ido)

; Ace-jump (Easymotion equivalent)
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c C-j") 'ace-jump-mode)

