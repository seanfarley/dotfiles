; -------------------------
; Basic settings
; -------------------------

; First disable things
(setq vc-handled-backends nil)
(cua-mode 0)

; Font test:
; ell 'l', one '1', little eye 'i', big eye 'I'
; zero '0', little oh 'o', big oh 'O'

; Turn off gui-type stuff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tabbar-mode) (tabbar-mode -1))

; Backup files
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

; Disable startup message in echo area
(setq inhibit-startup-echo-area-message t)

; Turn off blinking cursor
(blink-cursor-mode (- (*) (*) (*)))

; Tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 120 2))
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

; Columns
(column-number-mode t)
(line-number-mode t)
(setq-default fill-column 100)

; Scrolling settings from
; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

; Customized variable generated
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ns-right-command-modifier (quote meta))
 '(safe-local-variable-values (quote ((c-offsets-alist (inexpr-class . +) (inexpr-statement . +) (lambda-intro-cont . +) (inlambda . c-lineup-inexpr-block) (template-args-cont c-lineup-template-args +) (incomposition . +) (inmodule . +) (innamespace . +) (inextern-lang . +) (composition-close . 0) (module-close . 0) (namespace-close . 0) (extern-lang-close . 0) (composition-open . 0) (module-open . 0) (namespace-open . 0) (extern-lang-open . 0) (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +) (objc-method-args-cont . c-lineup-ObjC-method-args) (objc-method-intro . [0]) (friend . 0) (cpp-define-intro c-lineup-cpp-define +) (cpp-macro-cont . +) (cpp-macro . [0]) (inclass . +) (stream-op . c-lineup-streamop) (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist) (arglist-cont c-lineup-gcc-asm-reg 0) (arglist-intro . +) (catch-clause . 0) (else-clause . 0) (do-while-closure . 0) (label . 2) (access-label . -) (substatement-label . 2) (substatement . +) (statement-case-open . 0) (statement-case-intro . +) (statement-block-intro . +) (statement-cont . +) (statement . 0) (brace-entry-open . 0) (brace-list-entry . 0) (brace-list-intro . +) (brace-list-close . 0) (brace-list-open . 0) (block-close . 0) (inher-cont . c-lineup-multi-inher) (inher-intro . +) (member-init-cont . c-lineup-multi-inher) (member-init-intro . +) (annotation-var-cont . +) (annotation-top-cont . 0) (topmost-intro-cont . c-lineup-topmost-intro-cont) (topmost-intro . 0) (knr-argdecl . 0) (func-decl-cont . +) (inline-close . 0) (inline-open . +) (class-close . 0) (class-open . 0) (defun-block-intro . +) (defun-close . 0) (defun-open . 0) (string . c-lineup-dont-change) (arglist-close . c-lineup-arglist) (substatement-open . 0) (case-label . 0) (block-open . 0) (c . 1) (comment-intro . 0) (knr-argdecl-intro . -)) (c-cleanup-list scope-operator brace-else-brace brace-elseif-brace brace-catch-brace empty-defun-braces list-close-comma defun-close-semi) (c-hanging-semi&comma-criteria c-semi&comma-no-newlines-before-nonblanks) (c-hanging-colons-alist (member-init-intro before) (inher-intro) (case-label after) (label after) (access-label after)) (c-hanging-braces-alist (substatement-open after) (brace-list-open after) (brace-entry-open) (defun-open after) (class-open after) (inline-open after) (block-open after) (block-close . c-snug-do-while) (statement-case-open after) (substatement after)) (c-comment-only-line-offset . 0) (c-tab-always-indent . t))))
 '(whitespace-style (quote (face tabs trailing space-before-tab indentation empty space-after-tab tab-mark))))

; -------------------------
; Customizations generated but tweaked
; -------------------------

; Turn on whitespace
(require 'whitespace)
(global-whitespace-mode t)

; Tame the echo-area font
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(echo-area ((((type ns)) (:stipple nil :strike-through nil :underline nil :slant normal :weight normal :width normal))))
 '(whitespace-indentation ((((class color) (background dark)) (:background "#073642" :foreground "white")) (((class color) (background light)) (:background "#eee8d5" :foreground "black")) (t (:inverse-video t)))))

; -------------------------
; Keybindings
; -------------------------

; Add modifier keys for M-x
(global-set-key "\C-x\C-m" 'monky-status)

; Moving between windows
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

; Cmd-backspace delete to beginning of line (by default, you can do
; this is emacs with C-0 C-k)
(global-set-key [s-backspace] [?\C-0 ?\C-k])

(global-set-key [s-return] 'ns-toggle-fullscreen)

; -------------------------
; Load paths
; -------------------------

; Emacs Load Path
(add-to-list 'load-path "~/.emacs.d")

; AUCTeX
(load "tex.el")

; CEDET
(load-file "~/.emacs.d/plugins/cedet/common/cedet.el")

; now load all the plugins
(let ((default-directory  "~/.emacs.d/plugins"))
        (normal-top-level-add-subdirs-to-load-path))

; -------------------------
; emacsclient
; -------------------------
(setq server-use-tcp t)
(server-start)

; -------------------------
; Packages
; -------------------------

; Gtags
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

; Autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete/dict")
(ac-config-default)
;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)

; Monky
(require 'monky)
(setq monky-process-type 'cmdserver)

; Whole-line or region
(require 'whole-line-or-region)
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and
   we are not at the end of the line, then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts
   comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(whole-line-or-region-mode)
(global-set-key "\M-;" 'comment-dwim-line)

; Highlight both the row and column
(require 'crosshairs)

(global-hl-line-mode 1)
(global-hl-line-highlight)
(global-set-key (kbd "C-|") 'column-highlight-mode)

; color-theme
(require 'color-theme)
(require 'color-theme-solarized)
(require 'color-theme-zenburn)

;; (color-theme-zenburn)
(color-theme-solarized-light)

;; Sets the column highlight color to match hl-line (bug?)
;; (set-face-background 'col-highlight "#0a2832") ;dark
(set-face-background 'col-highlight "#e9e2cb") ;light

; YASnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(yas/load-directory "~/.emacs.d/custom-snippets")
(yas/global-mode 1)

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
(setq completion-ignored-extensions
  '(".o" ".elc" "~" ".bin" ".bak" ".obj" ".map" ".a" ".ln" ".mod" ".gz"
    ".aux" ".tdo" ".fmt" ".swp" ".pdfsync" ".pdf" ".vrb" ".idx" ".ind"
    ".bbl" ".toc" ".blg" ".snm" ".ilg" ".log" ".out" ".pyc" ".DS_Store"
    "-blx.bib" ".run.xml"))

; Ace-jump (Easymotion equivalent)
(require 'ace-jump-mode)
(define-key global-map (kbd "C-j") 'ace-jump-mode)

; Enable undohist for persistent undo
(require 'undohist)
(undohist-initialize)

; Enable undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

; Give each buffer a unique name
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

; Use zen-coding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'html-helper-mode-hook 'zencoding-mode)

(load-library "matlab-load")
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
 (add-to-list
  'auto-mode-alist
  '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))
(setq matlab-shell-command "matlab")

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq markdown-command "markdown-2.7")
(setq markdown-command-needs-filename t)

(require 'hungry-delete)
(global-set-key (kbd "C-c <DEL>") 'hungry-delete-backward)
(global-set-key (kbd "C-c d") 'hungry-delete-forward)


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
                               "*Compile-Log*" "*Help*" "*Messages*" ".aux"))

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
(global-set-key (kbd "C-S-<tab>") (lambda ()
                                    (interactive)
                                    (stesla-rotate-buffers -1)))

(defun compile-pkg (&optional command startdir)
  "Compile a package, moving up to the parent directory
  containing configure.ac, if it exists. Start in startdir if defined,
  else start in the current directory."
  (interactive)

  (let ((dirname) (dir-buffer nil))
    (setq startdir (expand-file-name (if startdir startdir ".")))
    (setq command  (if command command compile-command))

    (setq dirname (upward-find-file "config.log" startdir))
    (setq dirname (if dirname dirname (upward-find-file "configure.log" startdir)))
    (setq dirname (if dirname dirname (upward-find-file "Makefile" startdir)))
    (setq dirname (if dirname dirname (expand-file-name ".")))
    ; We've now worked out where to start. Now we need to worry about
    ; calling compile in the right directory
    (save-excursion
      (setq dir-buffer (find-file-noselect dirname))
      (set-buffer dir-buffer)
      (compile command)
      (kill-buffer dir-buffer))))

(defun upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
  manage to find it, return the containing directory. Else if we
  get to the toplevel directory and still can't find it, return
  nil. Start at startdir or . if startdir not given"

  (let ((dirname (expand-file-name (if startdir startdir ".")))
        (found nil) ; found is set as a flag to leave loop if we find it
        (top nil))  ; top is set when we get to / so that we only check it once

    ; While we've neither been at the top last time nor have we found the file.
    (while (not (or found top))
      ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/") (setq top t))

      ; Check for the file
      (if (file-exists-p (expand-file-name filename dirname)) (setq found t)
        ; If not, move up a directory
        (setq dirname (expand-file-name ".." dirname))))
    ; return statement
    (if found dirname nil)))

(defun std-compile ()
  "Like 'compile', but uses compile-pkg"
  (interactive)
  (compile-pkg compile-command))

(defun anything-semantic-construct-candidates (tags depth)
  (apply 'append (mapcar (lambda (tag)
    (if (listp tag)
      (let ((type (semantic-tag-type tag))
            (class (semantic-tag-class tag)))
      (if (or (and (stringp type)
                   (string= type "class"))
              (eq class 'function)
              (eq class 'variable))
          (cons (cons (concat (make-string (* depth 2) ?\s)
                              (semantic-format-tag-summarize tag nil t)) tag)
                (anything-semantic-construct-candidates (semantic-tag-components tag) (1+ depth)))))))
                         tags)))

(defvar anything-c-source-semantic
  '((name . "Semantic Tags")
    (init . (lambda ()
              (setq anything-semantic-candidates
                    (condition-case nil
                        (anything-semantic-construct-candidates (semantic-fetch-tags) 0)
                      (error nil)))))
    (candidates . (lambda ()
                    (if anything-semantic-candidates
                        (mapcar 'car anything-semantic-candidates))))
    (action . (("Goto tag" . (lambda (candidate)
                               (let ((tag (cdr (assoc candidate anything-semantic-candidates))))
                                 (semantic-go-to-tag tag))))))))

; -------------------------
; Hooks
; -------------------------

; c-mode-common-hook
(defun my-c-mode-hook ()
  (gtags-mode t)
  (linum-mode 1)
  (define-key c-mode-map (kbd "C-c c") 'std-compile)
)
(add-hook 'c-mode-hook 'my-c-mode-hook)

;;
;; Setup for ediff.
;;
(require 'ediff)

(defvar ediff-after-quit-hooks nil
  "* Hooks to run after ediff or emerge is quit.")

(defadvice ediff-quit (after edit-after-quit-hooks activate)
  (run-hooks 'ediff-after-quit-hooks))

(setq hg-mergetool-emacsclient-ediff-active nil)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-keep-variants nil)

(defun local-ediff-before-setup-hook ()
  (setq local-ediff-saved-frame-configuration (current-frame-configuration))
  (setq local-ediff-saved-window-configuration (current-window-configuration))
  ;; (local-ediff-frame-maximize)
  (if hg-mergetool-emacsclient-ediff-active
      (raise-frame)))

(defun local-ediff-quit-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(defun local-ediff-suspend-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(add-hook 'ediff-before-setup-hook 'local-ediff-before-setup-hook)
(add-hook 'ediff-quit-hook 'local-ediff-quit-hook 'append)
(add-hook 'ediff-suspend-hook 'local-ediff-suspend-hook 'append)

;; Useful for ediff merge from emacsclient.
(defun hg-mergetool-emacsclient-ediff (local remote base merged)
  (setq hg-mergetool-emacsclient-ediff-active t)
  (if (file-readable-p base)
      (ediff-merge-files-with-ancestor local remote base nil merged)
    (ediff-merge-files local remote nil merged))
  (recursive-edit))

(defun hg-mergetool-emacsclient-ediff-after-quit-hook ()
  (exit-recursive-edit))

(add-hook 'ediff-after-quit-hooks 'hg-mergetool-emacsclient-ediff-after-quit-hook 'append)

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
