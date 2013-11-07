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

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/scripts")

(load "init/basic.el")
(load "init/scratch.el")
(load "init/ediff.el")

; loading auctex is complicated and needs to be first
(load "init/tex.el")

; same with cedet; needs to be loaded first
(setq byte-compile-warnings nil)
(load "init/cedet.el")

(load "init/functions.el")
(load "init/keybindings.el")
(load "init/ctrl-tab.el")
(load "init/ediff.el")

(load "init/plugins.el")

(load "init/themes.el")

(load "init/mail.el")

(load "init/chat.el")

;; start sauron
(sauron-start)
