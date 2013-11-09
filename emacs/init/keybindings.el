; -------------------------
; Keybindings
; -------------------------

;; add modifier keys for mu4e
(global-set-key (kbd "C-c m") 'mu4e)

;; Moving between windows
(global-set-key (kbd "C-H-<up>") 'windmove-up)
(global-set-key (kbd "C-H-<down>") 'windmove-down)
(global-set-key (kbd "C-H-<right>") 'windmove-right)
(global-set-key (kbd "C-H-<left>") 'windmove-left)

;; cmd-backspace delete to beginning of line (by default, you can do
;; this is emacs with C-0 C-k)
(global-set-key [s-backspace] '(lambda () (interactive) (kill-line 0)))
(global-set-key [H-backspace] [?\C-  ?\C-a backspace])

(global-set-key (kbd "C-H-f") 'toggle-fullscreen)

(global-set-key (kbd "s-r") 'recompile)

;; Keybonds
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper q)] 'save-buffers-kill-terminal)
(global-set-key [(hyper k)] 'kill-this-buffer)
(global-set-key [(hyper r)] 'recompile)
(global-set-key [(hyper })] 'forward-paragraph)
(global-set-key [(hyper {)] 'backward-paragraph)
(global-set-key [(hyper f)] 'isearch-forward)
(global-set-key [(hyper g)] 'isearch-repeat-forward)

;; mac switch meta key
(defun mac-switch-meta nil
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'hyper))
      (progn
        (setq mac-option-modifier nil)
        (setq mac-command-modifier 'meta))))

(if (boundp 'mac-option-modifier) (mac-switch-meta))

;; easy spell
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(global-set-key (kbd "C-<f4>") 'flyspell-mode)
(global-set-key (kbd "H-<f4>") 'flyspell-buffer)
(global-set-key (kbd "M-S-<f4>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f4>") 'flyspell-check-next-highlighted-word)

;; Fix annoying list-buffer behavior
(global-set-key "\C-x\C-b" 'ido-switch-buffer)

;; Also, capture typos for C-x b
(global-set-key "\C-x\C-n" 'ido-switch-buffer)

;; Ebib
(eval-after-load 'latex
  '(define-key LaTeX-mode-map "\C-cb" 'ebib))

;; Sauron keys
(global-set-key (kbd "C-c s") 'sauron-toggle-hide-show)
(global-set-key (kbd "C-c t") 'sauron-clear)
