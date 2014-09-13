; load all the plugin paths
(let ((default-directory  "~/.emacs.d/plugins"))
        (normal-top-level-add-subdirs-to-load-path))

; -------------------------
; Plugins
; -------------------------

; add known files to sh-mode
(setq auto-mode-alist (cons '(".?aliases" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".?bash_prompt" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".?bashrc" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".?exports" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".?functions" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".?osx" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".?osx" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".?profile" . sh-mode) auto-mode-alist))

; add known files to conf-mode
(setq auto-mode-alist (cons '(".?hgrc" . conf-mode) auto-mode-alist))

; Turn on whitespace
(require 'whitespace)
(global-whitespace-mode t)
(setq whitespace-global-modes '(c-mode c++-mode python-mode
                                       text-mode conf-mode
                                       lisp-mode emacs-lisp-mode
                                       sh-mode tcl-mode))

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
(require 'ido-yes-or-no)
(ido-mode t)
(ido-yes-or-no-mode t)

; Enable ido
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
(setq ido-ignore-extensions t)

; Ace-jump (Easymotion equivalent)
(autoload 'ace-jump-mode "ace-jump-mode" "Ace Jump Mode." t)
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
(autoload 'zencoding-mode "zencoding-mode" "Zen Coding Mode." t)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'html-helper-mode-hook 'zencoding-mode)

;; (load-library "matlab-load")
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

(autoload 'pandoc-mode "pandoc-mode" "Pandoc Mode." t)
(setq pandoc-binary "pandoc")
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(eval-after-load "pandoc-mode"
  '(define-key pandoc-mode-map "\C-c/o" 'pandoc-set-output))

;; Workaround the annoying warnings:
;;    Warning (mumamo-per-buffer-local-vars):
;;    Already 'permanent-local t: buffer-file-name
(when (and (>= emacs-major-version 24) (>= emacs-minor-version 2))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

; Autocomplete
(autoload 'auto-complete-config "auto-complete-mode" "Auto-complete Mode." t)
(eval-after-load "auto-complete-config"
  '(progn
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

     ; clang-complete-async
     (autoload 'auto-complete-clang-async "ac-mode-setup" "Auto-complete Clang." t)

     (defun ac-cc-mode-setup ()
       (setq ac-clang-complete-executable "~/.emacs.d/plugins/emacs-clang-complete-async/clang-complete")
       (setq ac-clang-async-do-autocompletion-automatically t)
       (setq ac-sources '(ac-source-clang-async))
       (ac-clang-launch-completion-process))

     (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
     (add-hook 'auto-complete-mode-hook 'ac-common-setup)
))

; load codesearch
(require 'codesearch)

; load vim-modeline
(require 'vim-modeline)
(add-to-list 'find-file-hook 'vim-modeline/do)

; iedit
(autoload 'iedit "iedit-rectangle-mode" "iEdit Mode." t)
(autoload 'iedit-rect "iedit-rectangle-mode" "iEdit Rectangle Mode." t)

; yasnippet
(require 'yasnippet)
(setq yas/root-directory '("~/.emacs.d/snippets"))
(mapc 'yas/load-directory yas/root-directory)

(yas-global-mode 1)

(setq auto-mode-alist (cons '("\\.m$" . objc-mode) auto-mode-alist))

; web mode
(autoload 'web-mode "web-mode" "Web Mode." t)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

; php mode
(autoload 'php-mode "php-mode" "PHP Mode." t)

; ctable
(require 'ctable)

; epc
(autoload 'epc "jedi" "Asynchronous RPC Stack." t)

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

;; notifications
(require 'sauron)

; make sure our path to terminal-notifier is found before alert.el is loaded
(require 'alert)

; have sauron feed notifications to alert.el
(add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)

(setq
  alert-default-style 'notifier ; use notifier for alert.el
  sauron-separate-frame nil     ; don't use a seperate frame
)

; d-mode
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

(require 'nlinum)
(add-hook 'prog-mode-hook 'nlinum-mode)

;; filladap
(require 'filladapt)
(setq-default filladapt-mode t)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (featurep 'filladapt)
              (c-setup-filladapt))))

;; breadcrumb
(require 'breadcrumb)
(global-set-key [(hyper /)]              'bc-set)
(global-set-key [(meta j)]               'bc-previous)
(global-set-key [(shift meta j)]         'bc-next)
(global-set-key [(meta up)]              'bc-local-previous)
(global-set-key [(meta down)]            'bc-local-next)
(global-set-key [(control c)(j)]         'bc-goto-current)
(global-set-key [(control x)(control j)] 'bc-list)

;; hide-show visual markers
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)
(add-hook 'prog-mode-hook 'hideshowvis-enable)

;; projectile
(require 'recentf)
(require 'projectile)
(projectile-global-mode)

;; ack-and-a-half
(require 'ack-and-a-half)

(require 'ido-vertical-mode)
(ido-vertical-mode t)

;; smart modeline
(require 'smart-mode-line)

(setq sml/hidden-modes " \\(Projectile.*\\|hs\\|yas\\|Undo-Tree\\|Fly\\|Filladapt\\|WLR\\|AC\\|WS\\|MML\\)")

(sml/apply-theme 'respectful)
(sml/setup)

;; ido-ubiquitous
(require 'ido-ubiquitous)
(setq ido-ubiquitous-command-overrides 'enable-old)
(ido-ubiquitous t)

;; floobit
(require 'floobits)

;; auto-complete for rst
(require 'auto-complete-rst)
(auto-complete-rst-init)
(setq auto-complete-rst-other-sources
      '(ac-source-filename
        ac-source-abbrev
        ac-source-dictionary
        ac-source-yasnippet))
