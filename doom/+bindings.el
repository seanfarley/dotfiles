;;; doom/+bindings.el -*- lexical-binding: t; -*-

;; the most important binding: since using hammerspoon, I map C-g to ESC ... but
;; in emacs I want C-g to just be itself; since hammerspoon is buggy about its
;; own filters we just map ESC to C-g here. This also fixes the bug when using
;; spotlight from emacs (as the last app) where C-g was disabled.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(setq mac-command-modifier 'super
      mac-option-modifier  'meta
      ;; sane trackpad/mouse scroll settings
      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil
      ;; Curse Lion and its sudden but inevitable fullscreen mode!
      ;; NOTE Meaningless to railwaycat's emacs-mac build
      ns-use-native-fullscreen nil
      ;; Visit files opened outside of Emacs in existing frame, rather
      ;; than a new one
      ns-pop-up-frames nil)

(map!
  "s-k"                               #'kill-this-buffer
  "s-n"                               #'smf/make-frame
  "s-x"                               #'kill-region
  "s-N"                               #'+default/new-buffer

  "s-}"                               #'forward-paragraph
  "s-{"                               #'backward-paragraph

  "s-0"                               #'doom/reset-font-size

  "C-M-s-<left>"                      #'beginning-of-buffer
  "C-M-s-<right>"                     #'end-of-buffer
  "C-M-s-<down>"                      [?\C-v]
  "C-M-s-<up>"                        [?\M-v]

  "C-|"                               #'column-highlight-mode

  ;; custom methods
  "C-M-d"                             #'smf/delete-to-end-of-buffer
  (:map whole-line-or-region-local-mode-map
    ;; behave more like the terminal
    "C-w"                             (lambda ()
                                        (interactive)
                                        (if (not (eq major-mode 'vterm-mode))
                                            (smf/backward-kill-word)
                                          (vterm--self-insert))))

  ;; common typo for me
  "C-x C-b"                           #'persp-switch-to-buffer

  "C-s-<up>"                          #'windmove-up
  "C-s-<down>"                        #'windmove-down
  "C-s-<right>"                       #'windmove-right
  "C-s-<left>"                        #'windmove-left

  "C-<tab>"                           #'+workspace/switch-right
  "C-S-<tab>"                         #'+workspace/switch-left

  ;; Plugins
  "C-:"                               #'avy-goto-char-timer

  ;; goto-chg
  "C-M-s-z"                           #'goto-last-change

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
  "C-c h"                             #'banner-comment

  ;; embrace
  "C-,"                               #'embrace-commander)
