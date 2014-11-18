; -------------------------
; Basic settings
; -------------------------

; First disable things
(setq vc-handled-backends nil)
(setq ring-bell-function #'ignore)

; Font test:
; ell 'l', one '1', little eye 'i', big eye 'I'
; zero '0', little oh 'o', big oh 'O'

; No splash
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

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
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 120 4))
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(setq-default c-basic-offset 4)
(setq-default web-mode-code-indent-offset 4)
(setq-default web-mode-markup-indent-offset 4)
(setq-default web-mode-css-indent-offset 4)
(setq-default web-mode-sql-indent-offset 4)

; Columns
(column-number-mode t)
(line-number-mode t)
(setq-default fill-column 79)

; Ignored extensions
(setq completion-ignored-extensions
  '(".o" ".elc" "~" ".bin" ".bak" ".obj" ".map" ".a" ".ln" ".mod" ".gz"
    ".aux" ".tdo" ".fmt" ".swp" ".pdfsync" ".pdf" ".vrb" ".idx" ".ind"
    ".bbl" ".toc" ".blg" ".snm" ".ilg" ".log" ".out" ".pyc" ".DS_Store"
    "-blx.bib" ".run.xml" ".hi" ".fls" ".fdb_latexmk" ".bcf" ".rel"))

;; cool frame title with currently edited buffer name
(setq frame-title-format
      (concat "%b - " invocation-name "@" system-name))

;; keep minibuffer history between session
(savehist-mode t)

;; files should always end with a new line
(setq require-final-newline t)

;; don't let Customize mess with my .emacs
(setq custom-file (concat (getenv "HOME") "/.emacs.d/custom.el"))
(load custom-file 'noerror)

;; enable winner
(winner-mode t)

(when (window-system)
  (condition-case err
    (set-default-font "Source Code Pro for Powerline Light")
    (error
      (set-default-font "Sauce Code Powerline Light"))))
