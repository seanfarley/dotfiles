; -------------------------
; emacsclient
; note: this is put first so that ~/.edit.sh will continue to work
; -------------------------
(setq server-use-tcp t)
(server-start)

; -------------------------
; Load paths
; note: init/ is for stand-alone .el files
; note: scripts/ is for stand-alone .el files
; note: plugins/ is only for subrepos
; -------------------------

(add-to-list 'load-path "~/.emacs.d/scripts")

(load "~/.emacs.d/init/basic.el")
(load "~/.emacs.d/init/scratch.el")
(load "~/.emacs.d/init/ediff.el")

(load "~/.emacs.d/init/functions.el")
(load "~/.emacs.d/init/keybindings.el")
(load "~/.emacs.d/init/ctrl-tab.el")
(load "~/.emacs.d/init/ediff.el")

(load "~/.emacs.d/init/plugins.el")

(load "~/.emacs.d/init/themes.el")

(load "~/.emacs.d/init/mail.el")

(load "~/.emacs.d/init/chat.el")

(load "~/.emacs.d/init/tex.el")

;; start sauron
(sauron-start)
