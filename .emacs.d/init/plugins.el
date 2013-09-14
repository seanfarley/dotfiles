; load all the plugin paths
(let ((default-directory  "~/.emacs.d/plugins"))
        (normal-top-level-add-subdirs-to-load-path))

; -------------------------
; Plugins
; -------------------------

; Turn on whitespace
(require 'whitespace)
(global-whitespace-mode t)
(setq whitespace-global-modes '(c-mode c++-mode python-mode
                                       text-mode conf-mode
                                       lisp-mode emacs-lisp-mode
                                       sh-mode))

; Monky
(require 'monky)

; Highlight both the row and column
(require 'crosshairs)

(global-hl-line-mode 1)
(global-hl-line-highlight)
(global-set-key (kbd "C-|") 'column-highlight-mode)

;; Sets the column highlight color to match hl-line (bug?)
;; (set-face-background 'col-highlight "#0a2832") ;dark
(set-face-background 'col-highlight "#e9e2cb") ;light

; Load ido
(require 'ido)
(require 'ido-ubiquitous)
(ido-mode t)
(ido-ubiquitous t)
(require 'ido-yes-or-no)
(ido-yes-or-no-mode t)

; Enable ido
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
(setq ido-ignore-extensions t)

; Ace-jump (Easymotion equivalent)
(require 'ace-jump-mode)
(define-key global-map (kbd "C-j") 'ace-jump-mode)

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
(setq matlab-indent-function t)
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))
(setq matlab-shell-command "matlab")

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mmd" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq markdown-command "multimarkdown")
(setq markdown-command-needs-filename t)
(setq markdown-enable-itex t)

; hungry-delete
(require 'hungry-delete)
(global-set-key (kbd "C-c <M-backspace>") 'hungry-delete-backward)
(global-set-key (kbd "C-c M-d") 'hungry-delete-forward)

; ebib
(autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)

; rust
(require 'rust-mode)

; outline-magic
(add-hook 'outline-mode-hook
           (lambda ()
             (require 'outline-cycle)))

(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)
            (define-key outline-minor-mode-map [(f10)] 'outline-cycle)))

(require 'pandoc-mode)
(setq pandoc-binary "pandoc")
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(define-key pandoc-mode-map "\C-c/o" 'pandoc-set-output)

(require 'weblogger)

; haskell mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

; load websocket
(require 'websocket)

; load smartrep
(require 'smartrep)

; load ein
(require 'ein)
(setq ein:use-auto-complete t)
(setq ein:use-smartrep t)

;; Workaround the annoying warnings:
;;    Warning (mumamo-per-buffer-local-vars):
;;    Already 'permanent-local t: buffer-file-name
(when (and (>= emacs-major-version 24) (>= emacs-minor-version 2))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

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

; load codesearch
(require 'codesearch)

; load vim-modeline
(require 'vim-modeline)
(add-to-list 'find-file-hook 'vim-modeline/do)

; iedit
(require 'iedit)
(require 'iedit-rect)

; clang-complete-async
(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/plugins/emacs-clang-complete-async/clang-complete")
  (setq ac-clang-async-do-autocompletion-automatically t)
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process)
)

(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(global-auto-complete-mode t)

; yasnippet
(require 'yasnippet)
(setq yas/root-directory '("~/.emacs.d/snippets"
"~/.emacs.d/plugins/yasnippet/snippets"))
(mapc 'yas/load-directory yas/root-directory)

(yas-global-mode 1)

(setq auto-mode-alist (cons '("\\.m$" . objc-mode) auto-mode-alist))

;; w3m
(when (require 'w3m-load nil 'noerror)
  (setq w3m-display-inline-images t)
)

; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

; php mode
(require 'php-mode)

; ctable
(require 'ctable)

; epc
(require 'epc)

; jedi
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook (lambda () (define-key jedi-mode-map (kbd "<C-tab>") nil)))
(add-hook 'python-mode-hook (lambda () (define-key jedi-mode-map (kbd "C-c C-,") 'jedi:complete)))
(add-hook 'python-mode-hook 'jedi:setup)

; visual basic
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|vbs\\|cls\\)$" .
                                 visual-basic-mode)) auto-mode-alist))
