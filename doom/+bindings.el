;;; doom/+bindings.el -*- lexical-binding: t; -*-

(map!
  "s-k"                               #'kill-this-buffer
  "s-n"                               #'smf/make-frame
  "s-x"                               #'kill-region
  "s-N"                               #'+default/new-buffer

  "s-}"                               #'forward-paragraph
  "s-{"                               #'backward-paragraph

  "H-<left>"                          #'beginning-of-buffer
  "H-<right>"                         #'end-of-buffer
  "H-<down>"                          [?\C-v]
  "H-<up>"                            [?\M-v]

  ;; custom methods
  "C-M-d"                             #'smf/delete-to-end-of-buffer
  (:map whole-line-or-region-local-mode-map
    ;; behave more like the terminal
    "C-w"                             #'smf/backward-kill-word)

  ;; common typo for me
  "C-x C-b"                           #'persp-switch-to-buffer

  ;; Switching windows
  "C-x O"                             #'switch-window-then-swap-buffer

  "C-s-<up>"                          #'windmove-up
  "C-s-<down>"                        #'windmove-down
  "C-s-<right>"                       #'windmove-right
  "C-s-<left>"                        #'windmove-left

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

  ;; unfill
  "M-Q"                               #'unfill-paragraph

  ;; banner-comment
  "C-c h"                             #'banner-comment)
