;;; private/boy/+bindings.el -*- lexical-binding: t; -*-

;; Change the default key of persp-mode to avoid conflicts with projectile.
(setq persp-keymap-prefix (kbd "C-c e")
      projectile-keymap-prefix (kbd "C-c p"))

;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq doom-leader-key nil
      doom-localleader-key nil)

(map!
  "s-a"                               #'mark-whole-buffer
  "s-c"                               #'kill-ring-save
  "s-f"                               #'swiper
  "s-g"                               #'isearch-repeat-forward
  "s-k"                               #'kill-this-buffer
  "s-l"                               #'goto-line
  "s-n"                               #'smf/make-frame
  "s-q"                               #'save-buffers-kill-terminal
  "s-r"                               #'recompile
  "s-s"                               #'save-buffer
  "s-v"                               #'yank
  "s-w"                               #'delete-window
  "s-x"                               #'kill-region
  "s-z"                               #'undo-tree-undo
  "s-Z"                               #'undo-tree-redo
  "s-}"                               #'forward-paragraph
  "s-{"                               #'backward-paragraph
  "s-;"                               #'smf/comment-or-uncomment-region-or-line
  "H-<left>"                          #'beginning-of-buffer
  "H-<right>"                         #'end-of-buffer
  "H-<down>"                          [?\C-v]
  "H-<up>"                            [?\M-v]
  "C-s-f"                             #'toggle-frame-fullscreen
  [s-backspace]                       [?\C- ?\C-a backspace]

  ;; custom methods
  "M-;"                               #'comment-dwim
  "C-M-d"                             #'smf/delete-to-end-of-buffer
  (:map whole-line-or-region-local-mode-map
    "C-w"                             #'smf/backward-kill-word)

  ;; restore scroll
  "M-v"                               #'scroll-down-command

  ;; misc
  (:after helpful
    (:map helpful-mode-map
      "C-g"                           #'quit-window))

  "C-="                               #'er/expand-region

  ;; below from undeadkernel

  "C-a"                               #'doom/backward-to-bol-or-indent
  ;; Editor related bindings
  [remap newline]                     #'newline-and-indent
  "C-j"                               #'+default/newline

  ;; Buffer related bindings
  "C-x b"                             #'persp-switch-to-buffer
  "C-x C-b"                           #'persp-switch-to-buffer
  "C-x B"                             #'ivy-switch-buffer
  "C-x k"                             #'doom/kill-this-buffer-in-all-windows

  "C-`"                               #'+popup/toggle

  ;; Switching windows
  "C-x p"                             #'+popup/other
  "C-x C-o"                           #'+boy/switch-to-last-window
  "C-x O"                             #'switch-window-then-swap-buffer

  "C-s-<up>"                          #'windmove-up
  "C-s-<down>"                        #'windmove-down
  "C-s-<right>"                       #'windmove-right
  "C-s-<left>"                        #'windmove-left

  ;; Restore common editing keys in minibuffer
  (:map (minibuffer-local-map
         minibuffer-local-ns-map
         minibuffer-local-completion-map
         minibuffer-local-must-match-map
         minibuffer-local-isearch-map
         read-expression-map)
    "C-g"                             #'abort-recursive-edit
    "C-a"                             #'move-beginning-of-line)

  ;; Doom emacs bindings
  (:prefix "C-c d"
    "d"                               #'+doom-dashboard/open
    "f"                               #'recentf-open-files
    (:when (featurep! :ui neotree)
      "n"                             #'+neotree/open
      "N"                             #'neotree/find-this-file)
    (:when (featurep! :ui treemacs)
      "n"                             #'+treemacs/toggle
      "N"                             #'+treemacs/find-file)
    "o"                               #'+popup/other
    "t"                               #'+popup/toggle
    "c"                               #'+popup/close
    "C"                               #'+popup/close-all
    "r"                               #'+popup/raise
    "R"                               #'+popup/restore
    "s"                               #'doom/open-scratch-buffer
    "S"                               #'doom/switch-to-scratch-buffer
    "u"                               #'doom/sudo-this-file
    "e"                               #'+eshell/open-popup
    "E"                               #'+eshell/open
    "l"                               #'doom/toggle-line-numbers
    :desc "Reload Private Config" "R" #'doom/reload)

  ;; Org related bindings
  (:prefix "C-c o"
    "s"                               #'org-caldav-sync
    "a a"                             #'org-agenda
    "a t"                             #'org-todo-list
    "a m"                             #'org-tags-view
    "a v"                             #'org-search-view
    "c"                               #'org-capture
    :desc "org-capture-goto-target" "C"
                                      (λ! (require 'org-capture)
                                          (call-interactively
                                      #'org-capture-goto-target))
    "b"                               #'org-iswitchb
    "e l b"                           #'org-beamer-export-to-latex
    "e l B"                           #'org-beamer-export-as-latex
    "e l P"                           #'org-beamer-export-to-pdf
    "l"                               #'org-store-link)

  ;; Snippets
  (:prefix "C-c s"
    :desc "New snippet"           "n" #'yas-new-snippet
    :desc "Insert snippet"        "i" #'yas-insert-snippet
    :desc "Find snippet"          "s" #'+default/find-in-snippets
    :desc "Find snippet for mode" "S" #'+default/browse-snippets
    :desc "Find global snippet"   "/" #'yas-visit-snippet-file
    :desc "Reload snippets"       "r" #'yas-reload-all
    :desc "Create Temp Template"  "c" #'aya-create
    :desc "Use Temp Template"     "e" #'aya-expand)

  ;; Version control bindings
  (:prefix "C-c v"
    :desc "Magit status"          "g" #'magit-status
    :desc "Browse issues tracker" "i" #'+vc/git-browse-issues
    :desc "Browse remote"         "o" #'+vc/git-browse
    :desc "Magit commit"          "c" #'magit-commit
    :desc "Magit blame"           "b" #'magit-blame
    :desc "Initialize repo"       "I" #'magit-init
    :desc "Magit buffer log"      "l" #'magit-log-buffer-file
    :desc "List repositories"     "L" #'magit-list-repositories
    :desc "Git revert hunk"       "r" #'git-gutter:revert-hunk
    :desc "Git stage hunk"        "s" #'git-gutter:stage-hunk
    :desc "Git stage file"        "S" #'magit-stage-file
    :desc "Git time machine"      "t" #'git-timemachine-toggle
    :desc "Git unstage file"      "U" #'magit-unstage-file
    :desc "Next hunk"             "]" #'git-gutter:next-hunk
    :desc "Previous hunk"         "[" #'git-gutter:previous-hunk)

  ;; Working with windows, workgroups and stuff.
  (:prefix "C-c w"
    "d"                               #'+workspace/display
    "r"                               #'+workspace/rename
    "c"                               #'+workspace/new
    "k"                               #'+workspace/delete
    "s"                               #'+workspace/save-session
    "l"                               #'+workspace/load-session
    "L"                               #'+workspace/load-last-session
    "o"                               #'doom/kill-other-buffers
    "u"                               #'winner-undo
    "U"                               #'winner-redo
    "p"                               #'+workspace/switch-left
    "n"                               #'+workspace/switch-right
    "w"                               #'+workspace/switch-to
    ;; requires private package 'resize-window'
    "h"                               #'resize-window
    "1"                               (λ! (+workspace/switch-to 0))
    "2"                               (λ! (+workspace/switch-to 1))
    "3"                               (λ! (+workspace/switch-to 2))
    "4"                               (λ! (+workspace/switch-to 3))
    "5"                               (λ! (+workspace/switch-to 4))
    "6"                               (λ! (+workspace/switch-to 5))
    "7"                               (λ! (+workspace/switch-to 6))
    "8"                               (λ! (+workspace/switch-to 7))
    "9"                               (λ! (+workspace/switch-to 8))
    "0"                               #'+workspace/switch-to-last)

  ;; Plugins

  ;; goto-chg
  "H-."                               #'goto-last-change

  ;; Smartparens
  (:after smartparens
    (:map smartparens-mode-map
      "C-M-a"                         #'sp-beginning-of-sexp
      "C-M-e"                         #'sp-end-of-sexp
      "C-M-f"                         #'sp-forward-sexp
      "C-M-b"                         #'sp-backward-sexp
      "C-M-d"                         #'sp-splice-sexp
      "C-M-k"                         #'sp-kill-sexp
      "C-M-t"                         #'sp-transpose-sexp
      ;; TODO rethink these bindings
      ;; "C-<right>"                  #'sp-forward-slurp-sexp
      ;; "M-<right>"                  #'sp-forward-barf-sexp
      ;; "C-<left>"                   #'sp-backward-slurp-sexp
      ;; "M-<left>"                   #'sp-backward-barf-sexp
      ))

  ;; Company mode
  "C-;"                               #'+company/complete

  ;; Counsel
  (:when (featurep! :completion ivy)
    (:after counsel
      (:map counsel-ag-map
        ;; search/replace on results
        [backtab]                     #'+ivy/wgrep-occur
        ;; preview
        "C-SPC"                       #'ivy-call-and-recenter
        "M-RET"                       (+ivy-do-action!
                                       #'+ivy-git-grep-other-window-action))))
  "C-h b"                             #'counsel-descbinds
  "C-M-y"                             #'counsel-yank-pop
  "C-h F"                             #'counsel-faces
  "C-h p"                             #'counsel-package
  "C-h a"                             #'counsel-apropos
  "C-h V"                             #'counsel-set-variable
  "C-'"                               #'counsel-imenu

  ;; Repl Toggle
  "C-c C-z"                           #'+eval/open-repl

  ;; Company mode and the like
  (:after company
    (:map company-active-map
      "C-o"                           #'company-search-kill-others
      "C-n"                           #'company-select-next
      "C-p"                           #'company-select-previous
      "C-h"                           #'company-show-doc-buffer
      "C-s"                           #'company-search-candidates
      "M-s"                           #'company-filter-candidates
      "C-;"                           #'company-complete-common-or-cycle
      "TAB"                           #'company-complete-common-or-cycle
      [backtab]                       #'company-select-previous
      "C-RET"                         #'counsel-company)
    (:map company-search-map
      "C-n"                           #'company-search-repeat-forward
      "C-p"                           #'company-search-repeat-backward
      "C-s"                           (λ! (company-search-abort)
                                          (company-filter-candidates))))

  ;; NeoTree bindings
  (:after neotree
    :map neotree-mode-map
    "q"                               #'neotree-hide
    [return]                          #'neotree-enter
    "RET"                             #'neotree-enter
    "SPC"                             #'neotree-quick-look
    "v"                               #'neotree-enter-vertical-split
    "s"                               #'neotree-enter-horizontal-split
    "c"                               #'neotree-create-node
    "D"                               #'neotree-delete-node
    "g"                               #'neotree-refresh
    "r"                               #'neotree-rename-node
    "R"                               #'neotree-refresh
    "h"                               #'+neotree/collapse-or-up
    "l"                               #'+neotree/expand-or-open
    "n"                               #'neotree-next-line
    "p"                               #'neotree-previous-line
    "N"                               #'neotree-select-next-sibling-node
    "P"                               #'neotree-select-previous-sibling-node)

  ;; Refactoring and compilation
  (:map prog-mode-map
    "M-RET"                           #'emr-show-refactor-menu)
  (:after cc-mode
    (:map c++-mode-map
      "M-RET"                         #'srefactor-refactor-at-point)
    (:map c-mode-map
      "M-RET"                         #'srefactor-refactor-at-point))
  (:after help-mode
    ;; (:map help-map
    ;;   "e" 'doom/popup-toggle-messages)
    (:map help-mode-map
      "o"                             #'ace-link-help
      ">"                             #'help-go-forward
      "<"                             #'help-go-back))
  (:after info
    (:map Info-mode-map
      "o"                             #'ace-link-info))

  ;; yasnippet
  (:after yasnippet
    ;; keymap while yasnippet is active
    (:map yas-minor-mode-map
      "<C-tab>"                       #'yas-insert-snippet)
    ;; keymap while editing an inserted snippet
    (:map yas-keymap
      "C-e"                           #'+snippets/goto-end-of-field
      "C-a"                           #'+snippets/goto-start-of-field
      "<M-right>"                     #'+snippets/goto-end-of-field
      "<M-left>"                      #'+snippets/goto-start-of-field
      "<M-backspace>"                 #'+snippets/delete-to-start-of-field
      [backspace]                     #'+snippets/delete-backward-char
      [delete]                        #'+snippets/delete-forward-char-or-field))

  ;; flycheck
  (:after flycheck
    (:map flycheck-error-list-mode-map
      "C-n"                           #'flycheck-error-list-next-error
      "C-p"                           #'flycheck-error-list-previous-error
      "RET"                           #'flycheck-error-list-goto-error))

  ;; flyspell
  (:after flyspell
    (:map flyspell-mode-map
      "C-;" nil ; Do not override
      "C-M-i"                         #'flyspell-auto-correct-previous-word))

  ;; ivy
  (:after ivy
    (:map ivy-minibuffer-map
      "TAB"                           #'ivy-alt-done
      "C-g"                           #'keyboard-escape-quit))

  ;; magit
  (:after magit
    (:map magit-mode-map
      ;; Don't let Tab binding in my bindings conflict with Tab in magit
      "<tab>"                         #'magit-section-toggle))

  ;; latex
  (:after latex
    (:when (not (or (null boy--synonyms-key) (string= "" boy--synonyms-key)))
      ("C-c y"                        #'www-synonyms-insert-synonym))
    (:map LaTeX-mode-map
      ;; Replace LaTeX-section with a version that inserts '%' after the section
      ;; macro
      "C-c C-s"                       #'+boy/latex-section
      ;; Run LatexMk without asking
      "<f8>"                          #'+boy/run-latexmk))

  ;; ;; ein notebokks
  ;; (:after ein:notebook-multilang
  ;;   (:map ein:notebook-multilang-mode-map
  ;;     "C-c h"                      #'+ein/hydra/body))

  ;; unfill
  "M-Q"                               #'unfill-paragraph

  ;; banner-comment
  "C-c h"                             #'banner-comment

  ;; circe emacs bindings
  (:prefix "C-c m"
    "m"                               #'=email
    "c"                               #'+email/compose)

  ;; circe emacs bindings
  (:prefix "C-c i"
    "i"                               #'=irc
    "k"                               #'+irc/quit
    "r"                               #'circe-reconnect-all))

(which-key-add-key-based-replacements "C-c !"   "checking")
(which-key-add-key-based-replacements "C-c d p" "doom popups")
(which-key-add-key-based-replacements "C-c d"   "doom")
(which-key-add-key-based-replacements "C-c e"   "perspective")
(which-key-add-key-based-replacements "C-c m"   "mail")
(which-key-add-key-based-replacements "C-c o a" "org agenda")
(which-key-add-key-based-replacements "C-c o e" "org export")
(which-key-add-key-based-replacements "C-c o"   "org")
(which-key-add-key-based-replacements "C-c p"   "projectile")
(which-key-add-key-based-replacements "C-c s"   "snippets")
(which-key-add-key-based-replacements "C-c v"   "versioning")
(which-key-add-key-based-replacements "C-c w"   "workspace")
