;;; doom/+bindings.el -*- lexical-binding: t; -*-

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

  ;; misc
  (:after helpful
    (:map helpful-mode-map
      "C-g"                           #'quit-window))

  ;; common typo for me
  "C-x C-b"                           #'persp-switch-to-buffer

  ;; Switching windows
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

  ;; Working with windows, workgroups and stuff.
  (:prefix "C-c w"
    "w"                               #'+workspace/switch-to)

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
      "C-<right>"                  nil
      "M-<right>"                  nil
      "C-<left>"                   nil
      "M-<left>"                   nil
      "C-M-d"                      nil))

  ;; flyspell
  (:after flyspell
    (:map flyspell-mode-map
      "C-;" nil ; Do not override
      "C-M-i"                         #'flyspell-auto-correct-previous-word))

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

  ;; unfill
  "M-Q"                               #'unfill-paragraph

  ;; banner-comment
  "C-c h"                             #'banner-comment)
