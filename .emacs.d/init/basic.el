; -------------------------
; Basic settings
; -------------------------

; First disable things
(setq vc-handled-backends nil)
(setq ring-bell-function #'ignore)

; Font test:
; ell 'l', one '1', little eye 'i', big eye 'I'
; zero '0', little oh 'o', big oh 'O'

; Auto-revert
(global-auto-revert-mode t)

; Turn off gui-type stuff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tabbar-mode) (tabbar-mode -1))

; Backup files
(setq auto-save-interval 500)
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

; Disable startup message in echo area
(setq inhibit-startup-echo-area-message t)

; Turn off blinking cursor
(blink-cursor-mode (- (*) (*) (*)))
(show-paren-mode t)

; Tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 120 2))
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

; Columns
(column-number-mode t)
(line-number-mode t)
(setq-default fill-column 90)

; Ignored extensions
(setq completion-ignored-extensions
  '(".o" ".elc" "~" ".bin" ".bak" ".obj" ".map" ".a" ".ln" ".mod" ".gz"
    ".aux" ".tdo" ".fmt" ".swp" ".pdfsync" ".pdf" ".vrb" ".idx" ".ind"
    ".bbl" ".toc" ".blg" ".snm" ".ilg" ".log" ".out" ".pyc" ".DS_Store"
    "-blx.bib" ".run.xml" ".hi"))
