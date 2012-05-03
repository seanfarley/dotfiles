; -------------------------
; Keybindings
; -------------------------

; Add modifier keys for M-x
(global-set-key "\C-x\C-m" 'monky-status)

; Moving between windows
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

; Cmd-backspace delete to beginning of line (by default, you can do
; this is emacs with C-0 C-k)
(global-set-key [s-backspace] [?\C-0 ?\C-k])

(global-set-key [s-return] 'ns-toggle-fullscreen)
