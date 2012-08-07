; load all the plugin paths
(let ((default-directory  "~/.emacs.d/plugins"))
        (normal-top-level-add-subdirs-to-load-path))

; -------------------------
; Plugins
; -------------------------

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
(ac-flyspell-workaround)

; ac-math
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of {{{latex-mode}}}
(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
     (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
               ac-sources))
)
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

; Monky
(require 'monky)
(setq monky-process-type 'cmdserver)

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
(color-theme-solarized-dark)

;; Sets the column highlight color to match hl-line (bug?)
;; (set-face-background 'col-highlight "#0a2832") ;dark
(set-face-background 'col-highlight "#e9e2cb") ;light

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

; Enable undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(defun undo-tree-make-history-save-file-name ()
  "Return file name to use for auto-saves of current buffer's history"
  (if buffer-file-name
   (progn
       (if (not (file-exists-p "~/.emacs.d/undohistory/")) (make-directory "~/.emacs.d/undohistory" t))
       (concat (expand-file-name "~/.emacs.d/undohistory/")
                  (replace-regexp-in-string "/" "!" buffer-file-name)))
    (expand-file-name (concat "#%" (buffer-name) "#"))))

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

; hungry-delete
(require 'hungry-delete)
(global-set-key (kbd "C-c <DEL>") 'hungry-delete-backward)
(global-set-key (kbd "C-c d") 'hungry-delete-forward)

; ebib
(autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)
(global-set-key "\C-ce" 'ebib)

; rust
(require 'rust-mode)

; notmuch
(require 'notmuch nil 'noerror)

(define-key notmuch-search-mode-map "d"
 (lambda ()
  "mark messages in thread as deleted"
  (interactive)
  (notmuch-search-tag '("+deleted" "-inbox"))))

(define-key notmuch-search-mode-map "S"
 (lambda ()
  "mark messages in thread as spam"
  (interactive)
  (notmuch-search-tag '("+spam" "-inbox"))))

; Mac OS Address Book integration
(if (require 'eudcb-mab nil t)
    (setq eudc-protocol 'mab)
  (setq eudc-protocol 'bbdb))

; cua
(setq cua-enable-cua-keys nil)
(cua-mode)
