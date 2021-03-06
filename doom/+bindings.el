;;; doom/+bindings.el -*- lexical-binding: t; -*-

;; the most important binding: since using hammerspoon, I map C-g to ESC ... but
;; in emacs I want C-g to just be itself; since hammerspoon is buggy about its
;; own filters we just map ESC to C-g here. This also fixes the bug when using
;; spotlight from emacs (as the last app) where C-g was disabled.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; because `read-char-exclusive' is a C function and doesn't read the remapping
;; above, we need to patch methods that call `read-char-exclusive' to call
;; `keyboard-quit'
(defadvice! quit-on-esc (orig-fn &rest args)
  :around '(org-export--dispatch-action)
  (cl-letf* ((old-read-char (symbol-function 'read-char-exclusive))
             ((symbol-function 'read-char-exclusive)
              (lambda (&optional prompt inherit-input-method seconds)
                (pcase (funcall old-read-char prompt inherit-input-method seconds)
                  (27 (keyboard-quit))
                  (x x)))))
    (apply orig-fn args)))

(after! projectile
  ;; super common typo
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map))

;; on linux, I still want mac keybindings ^_^
(when IS-LINUX
  (map! "s-`" #'other-frame  ; fix frame-switching
        ;; fix OS window/frame navigation/manipulation keys
        "s-w" #'delete-window
        "s-W" #'delete-frame
        "s-q" (if (daemonp) #'delete-frame #'save-buffers-kill-terminal)
        "C-s-f" #'toggle-frame-fullscreen
        ;; Restore somewhat common navigation
        "s-l" #'goto-line
        ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
        ;; it imposes some other functionality and overhead we don't need)
        "s-f" #'swiper
        "s-z" #'undo
        "s-Z" #'redo
        "s-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
        "s-v" #'yank
        "s-s" #'save-buffer
        "s-x" #'kill-region
        ;; Buffer-local font scaling
        "s-+" #'doom/reset-font-size
        "s-=" #'doom/increase-font-size
        "s--" #'doom/decrease-font-size
        ;; Conventional text-editing keys & motions
        "s-a" #'mark-whole-buffer
        "s-/" (cmd! (save-excursion (comment-line 1)))
        :n "s-/" #'evilnc-comment-or-uncomment-lines
        :v "s-/" #'evilnc-comment-operator
        :gi  [s-backspace] #'doom/backward-kill-to-bol-and-indent
        :gi  [s-left]      #'doom/backward-to-bol-or-indent
        :gi  [s-right]     #'doom/forward-to-last-non-comment-or-eol
        :gi  [M-backspace] #'backward-kill-word
        :gi  [M-left]      #'backward-word
        :gi  [M-right]     #'forward-word))

(map!
 "s-k"                               #'kill-this-buffer
 "s-x"                               #'kill-region
 "s-n"                               #'smf/make-frame
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

 ;; jumping / goto
 "M-g g"                             nil
 "M-g TAB"                           nil
 "M-g ESC"                           nil
 "C-c g g"                           #'avy-goto-char
 "C-c g c"                           #'avy-goto-char
 "C-c g l"                           #'avy-goto-line
 "C-c g w"                           #'avy-goto-word-0

 ;; goto-chg
 "C-M-s-z"                           #'goto-last-change

 ;; smartparens
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
   "C-<right>"                     nil
   "M-<right>"                     nil
   "C-<left>"                      nil
   "M-<left>"                      nil
   "C-M-d"                         nil))

 ;; flyspell
 (:after flyspell
  (:map flyspell-mode-map
   "C-;"                           nil ; Do not override
   "C-."                           nil ; prefer dot-mode binding over auto
                                        ; correct
   "C-M-i"                         #'flyspell-auto-correct-previous-word))

 ;; python
 (:after python
  (:map python-mode-map
   ;; common typo for me with projectile
   "C-c C-p"                       nil))

 ;; unfill
 "M-Q"                               #'unfill-paragraph

 ;; banner-comment
 "C-c c h"                           #'banner-comment

 ;; embrace
 "C-,"                               #'embrace-commander)
