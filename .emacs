; -------------------------
;
; Basic settings
; -------------------------

; Font test:
; ell 'l', one '1', little eye 'i', big eye 'I'
; zero '0', little oh 'o', big oh 'O'
(if window-system
  (progn
   (set-default-font "Andale Mono")
   (set-face-attribute 'default nil :font "Andale Mono")))

; Turn off gui-type stuff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tabbar-mode) (tabbar-mode -1))

; Backup files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

; Disable startup message in echo area
(setq inhibit-startup-echo-area-message t)

; Turn off blinking cursor
(blink-cursor-mode (- (*) (*) (*)))

; Tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

; Scrolling settings from
; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

; Change the right command button to meta
(custom-set-variables '(ns-right-command-modifier (quote meta)))

; -------------------------
; Customizations generated but tweaked
; -------------------------

; Turn on whitespace
(require 'whitespace)
(custom-set-variables
  '(whitespace-style (quote (face tabs spaces trailing lines space-before-tab indentation empty space-after-tab tab-mark)))
  '(global-whitespace-mode t))

; Tame the echo-area font
(custom-set-faces
 '(echo-area ((((type ns)) (:stipple nil :strike-through nil :underline nil :slant normal :weight normal :width normal)))))

; -------------------------
; Keybindings
; -------------------------

; Add modifier keys for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

; Moving between windows
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

; Sets `C-c d` to `M-x kill-whole-line`
(global-set-key "\C-cd" 'kill-whole-line)

; Copies whole line
(global-set-key "\C-cc" "\C-a\C- \C-e\M-w")

; -------------------------
; Load paths
; -------------------------

; Emacs Load Path
(add-to-list 'load-path "~/.emacs.d")
(let ((default-directory  "~/.emacs.d/plugins"))
        (normal-top-level-add-subdirs-to-load-path))


; -------------------------
; Packages
; -------------------------

; Monky
(require 'monky)
(setq monky-process-type 'cmdserver)

; Highlight both the row and column
(require 'crosshairs)

(custom-set-variables '(crosshairs-mode t))
(global-set-key (kbd "C-|") 'crosshairs-mode)

; color-theme
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-solarized-light)
; Sets the column highlight color to match hl-line (bug?)
; (set-face-background 'col-highlight "#0a2832") ;dark
(set-face-background 'col-highlight "#e9e2cb") ;light
; (require 'color-theme-zenburn)
; (color-theme-zenburn)

; ; YASnippet
; (require 'yasnippet) ;; not yasnippet-bundle
; (yas/initialize)
; (yas/load-directory "~/.emacs.d/snippets")
; (yas/global-mode 1)

; CEDET
(load-file "~/.emacs.d/cedet-1.0/common/cedet.el")
(when (require 'gtags)
  (global-set-key "\C-cf" 'gtags-find-file)
  (global-set-key "\M-." 'gtags-find-tag)
  (global-set-key (kbd "C->") 'gtags-find-tag-from-here)
  (global-set-key "\M-*" 'gtags-pop-stack)
  (define-key gtags-mode-map (kbd "C-c r") 'gtags-find-rtag)
  (define-key gtags-select-mode-map "p" 'previous-line)
  (define-key gtags-select-mode-map "n" 'next-line)
  (define-key gtags-select-mode-map "q" 'gtags-pop-stack)
  (define-key gtags-select-mode-map "\C-m" 'gtags-select-tag)
  (define-key gtags-select-mode-map " " 'gtags-select-tag)
  (define-key gtags-select-mode-map "\C-o" 'gtags-select-tag-other-window)
  (message "Loaded gtags")
  (when (require 'semanticdb-global)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode))
)
(gtags-mode t)

(require 'anything-config)

; Load ido
(require 'ido)
(require 'ido-ubiquitous)
(ido-ubiquitous t)
(require 'ido-yes-or-no)
(ido-yes-or-no-mode t)

; Enable ido
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

; Ace-jump (Easymotion equivalent)
(require 'ace-jump-mode)
(define-key global-map (kbd "\C-ca") 'ace-jump-mode)

; Enable undohist for persistent undo
(require 'undohist)
(undohist-initialize)

; Enable undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

; Remove vc
(setq vc-handled-backends ())

; Enable rainbow matching
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

; Give each buffer a unique name
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

; -------------------------
; Functions
; -------------------------

; Wrap region with a tag
(defun my-insert-tags (tag)
  (interactive "sTag: ")
  (if (region-active-p)
      (let ((beg (region-beginning)))
        (save-excursion
          (goto-char (region-end))
          (insert "</" (car (split-string tag)) ">")
          (goto-char beg)
          (insert "<" tag ">")))
    (insert "<" tag ">")
    (save-excursion
      (insert "</" (car (split-string tag)) ">"))))

;; Change cursor color according to mode; inspired by
;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
(setq djcb-read-only-color       "gray")
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see thep docs for set-cursor-type

(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color       "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color          "gray")
(setq djcb-normal-cursor-type    'box)

(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."

  (cond
    (buffer-read-only
      (set-cursor-color djcb-read-only-color)
      (setq cursor-type djcb-read-only-cursor-type))
    (overwrite-mode
      (set-cursor-color djcb-overwrite-color)
      (setq cursor-type djcb-overwrite-cursor-type))
    (t
      (set-cursor-color djcb-normal-color)
      (setq cursor-type djcb-normal-cursor-type))))

(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)

; Ctrl-Tab cycling
; from http://www.emacswiki.org/cgi-bin/wiki/ControlTABbufferCycling
(defvar stesla-hated-buffers '("KILL" "*Apropos*" "*Completions*" "*grep*"
                                ".newsrc-dribble" ".bbdb" "sent-mail" "*vc*"
                               "*Compile-Log*" "*Help*" "*Messages*"))

(defvar stesla-hated-buffer-regexps
  '("^ " "*Buffer" "^\\*trace" "^\\*tramp" "^\\*"))

(setq iswitchb-buffer-ignore
  (append stesla-hated-buffer-regexps  stesla-hated-buffers))

(defmacro stesla-buffer-regexp-mapcar (regexp buffers)
  "Find BUFFERS whose name matches REGEXP"
  `(mapcar (lambda (this-buffer)
             (if (string-match ,regexp (buffer-name this-buffer))
                 this-buffer))
           ,(if (symbolp buffers) (symbol-value buffers) buffers)))

(defmacro stesla-hated-buffer-from-regexps (regexps)
  "Generate a one-dimensional list of buffers that match REGEXPS"
  (append
   '(append)
   (mapcar (lambda (regexp)
             `(delete nil (stesla-buffer-regexp-mapcar ,regexp
                                                       (buffer-list))))
           (if (symbolp regexps) (symbol-value regexps) regexps))))

(defun stesla-delete-from-list (delete-these from-list)
  "Delete DELETE-THESE from FROM-LIST."
  (cond
   ((car delete-these)
    (if (member (car delete-these) from-list)
        (stesla-delete-from-list (cdr delete-these)
                                (delete (car delete-these) from-list))
      (stesla-delete-from-list (cdr delete-these) from-list)))
   (t from-list)))

(defun stesla-hated-buffers ()
  "List of buffers I never want to see."
  (delete nil
          (append
           (mapcar 'get-buffer stesla-hated-buffers)
           (stesla-hated-buffer-from-regexps stesla-hated-buffer-regexps))))

;; `stesla-rotate-buffers': Like `bury-buffer' but with the capability to
;; exclude certain specified buffers.
(defun stesla-rotate-buffers (&optional n)
  "Switch to the Nth next buffer.  Negative arguments move backwards."
  (interactive)
  (unless n
    (setq n 1))
  (let ((my-buffer-list
         (stesla-delete-from-list (stesla-hated-buffers)
                                 (buffer-list (selected-frame)))))
    (switch-to-buffer
     (if (< n 0)
       (nth (+ (length my-buffer-list) n) my-buffer-list)
       (bury-buffer)
       (nth n my-buffer-list)))))

(global-set-key (kbd "C-<tab>") 'stesla-rotate-buffers)
(global-set-key (kbd "C-M-<tab>") (lambda ()
                                    (interactive)
                                    (stesla-rotate-buffers -1)))
