;;; +bindings.el -*- lexical-binding: t; -*-

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

;; on linux, I still want mac keybindings ^_^
(when IS-LINUX
  (map!
   ;; fix frame-switching
   "s-`"                           #'other-frame
   ;; fix OS window/frame navigation/manipulation keys
   "s-w"                           #'delete-window
   "s-W"                           #'delete-frame
   "s-q"                           (if (daemonp)
                                       #'delete-frame
                                     #'save-buffers-kill-terminal)
   "C-s-f"                         #'toggle-frame-fullscreen
   ;; Restore somewhat common navigation
   "s-l"                           #'goto-line
   ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
   ;; it imposes some other functionality and overhead we don't need)
   "s-f"                           #'swiper
   "s-z"                           #'undo
   "s-Z"                           #'redo
   "s-c"                           (if (featurep 'evil)
                                       #'evil-yank
                                     #'copy-region-as-kill)
   "s-v"                           #'yank
   "s-s"                           #'save-buffer
   "s-x"                           #'kill-region
   ;; Buffer-local font scaling
   "s-+"                           #'doom/reset-font-size
   "s-="                           #'doom/increase-font-size
   "s--"                           #'doom/decrease-font-size
   ;; Conventional text-editing keys & motions
   "s-a"                           #'mark-whole-buffer
   "s-/"                           (cmd!
                                    (save-excursion
                                      (comment-line 1)))
   :n   "s-/"                      #'evilnc-comment-or-uncomment-lines
   :v   "s-/"                      #'evilnc-comment-operator
   :gi  [s-backspace]              #'doom/backward-kill-to-bol-and-indent
   :gi  [s-left]                   #'doom/backward-to-bol-or-indent
   :gi  [s-right]                  #'doom/forward-to-last-non-comment-or-eol
   :gi  [M-backspace]              #'backward-kill-word
   :gi  [M-left]                   #'backward-word
   :gi  [M-right]                  #'forward-word))

(map!
 :leader :desc "Undo tree" "U"     #'vundo)

(map!
 "s-k"                             #'kill-this-buffer
 "s-x"                             #'kill-region
 "s-n"                             #'smf/make-frame
 "s-N"                             #'+default/new-buffer

 "s-}"                             #'forward-paragraph
 "s-{"                             #'backward-paragraph
 "C-<tab>"                         #'next-buffer
 "C-S-<tab>"                       #'previous-buffer

 "s-0"                             #'doom/reset-font-size

 "C-M-s-<left>"                    #'beginning-of-buffer
 "C-M-s-<right>"                   #'end-of-buffer
 "C-M-s-<down>"                    [?\C-v]
 "C-M-s-<up>"                      [?\M-v]

 "C-|"                             #'column-highlight-mode

 ;; custom methods
 "C-M-S-D"                         #'smf/delete-to-end-of-buffer
 (:map whole-line-or-region-local-mode-map
       ;; behave more like the terminal
       "C-w"                       (lambda ()
                                     (interactive)
                                     (if (not (eq major-mode 'vterm-mode))
                                         (smf/backward-kill-word)
                                       (vterm--self-insert))))

 ;; common typos for me
 "C-x C-b"                         #'persp-switch-to-buffer
 (:after projectile
         (:map projectile-mode-map
               "C-c C-p"           #'projectile-command-map))

 "C-s-<up>"                        #'windmove-up
 "C-s-<down>"                      #'windmove-down
 "C-s-<right>"                     #'windmove-right
 "C-s-<left>"                      #'windmove-left

 (:leader
  (:prefix-map ("w" . "workspaces/windows")
               :desc "Switch to other non-app workspace"
               "o"                 #'smf/workspace-other
               :desc "Switch to other workspace"
               "O"                 #'+workspace/other))

 ;; jumping / goto
 "M-g g"                           nil
 "M-g TAB"                         nil
 "M-g ESC"                         nil
 "C-c g g"                         #'avy-goto-char
 "C-c g c"                         #'avy-goto-char
 "C-c g l"                         #'avy-goto-line
 "C-c g w"                         #'avy-goto-word-0

 ;; goto-chg
 "C-M-s-z"                         #'goto-last-change

 ;; smartparens
 (:after smartparens
         (:map smartparens-mode-map
               "C-M-u"             #'sp-backward-up-sexp
               "C-M-r"             #'sp-raise-sexp
               "C-M-w"             #'sp-copy-sexp
               "C-M-<right>"       #'sp-forward-slurp-sexp
               "C-M-<left>"        #'sp-forward-barf-sexp
               "C-M-<up>"          #'sp-backward-slurp-sexp
               "C-M-<down>"        #'sp-backward-barf-sexp))

 ;; flyspell
 (:after flyspell
         (:map flyspell-mode-map
               "C-;"               nil  ; Do not override
               "C-."               nil  ; prefer dot-mode binding over auto
                                        ; correct
               "C-M-i"             #'flyspell-auto-correct-previous-word))

 ;; python
 (:after python
         (:map python-mode-map
               ;; common typo for me with projectile
               "C-c C-p"           nil))

 ;; magit
 (:after magit
         (:map magit-status-mode-map
               "M-r"               #'magit-section-up))

 ;; unfill
 "M-Q"                             #'unfill-paragraph

 (:after (org org-agenda org-roam)
         (:map org-mode-map
               ;; I use meta-arrow keys for navigation so let's stop org from
               ;; using them to indent
               "<M-S-left>"        nil
               "<M-left>"          nil
               "<M-right>"         nil
               ;; since I commonly mistype =C-c C-'= instead of =C-c '=, let's
               ;; add that keybinding,
               "C-c C-'"           #'org-edit-special

               ;; same as python
               "C-c <"             #'org-shiftmetaleft
               "C-c >"             #'org-shiftmetaright

               ;; insert org-roam key (usually a link)
               "C-c n r k"         #'smf/org-roam-insert-key)

         (:map org-src-mode-map
               "C-c C-'"           #'org-edit-src-exit
               ;; I find it infuriating that my muscle memory =⌘+s= in
               ;; =org-src-mode= will save the buffer as a new file. Instead,
               ;; let's make it do the same thing as =C-c '=
               "s-s"               #'org-edit-src-exit)

         (:map org-read-date-minibuffer-local-map
               "M-h"               (lambda ()
                                     (interactive)
                                     (org-eval-in-calendar
                                      '(calendar-backward-day 1)))
               "M-l"               (lambda ()
                                     (interactive)
                                     (org-eval-in-calendar
                                      '(calendar-forward-day 1)))
               "M-k"               (lambda ()
                                     (interactive)
                                     (org-eval-in-calendar
                                      '(calendar-backward-week 1)))
               "M-j"               (lambda ()
                                     (interactive)
                                     (org-eval-in-calendar
                                      '(calendar-forward-week 1)))
               "C-M-h"             (lambda ()
                                     (interactive)
                                     (org-eval-in-calendar
                                      '(calendar-backward-month 1)))
               "C-M-l"             (lambda ()
                                     (interactive)
                                     (org-eval-in-calendar
                                      '(calendar-forward-month 1)))
               "C-M-j"             (lambda ()
                                     (interactive)
                                     (org-eval-in-calendar
                                      '(calendar-forward-year 1)))
               "C-M-k"             (lambda ()
                                     (interactive)
                                     (org-eval-in-calendar
                                      '(calendar-backward-year 1)))
               )))
