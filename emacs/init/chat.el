(setq load-path (cons "~/.emacs.d/plugins/erc" load-path))
(require 'tls)
(require 'erc)
(require 'erc-join)
(require 'erc-hl-nicks)
(require 'erc-menu)
(require 'znc)

(setq
  ;; znc
  znc-detatch-on-kill t
  znc-servers `(
                ("smf.io" 6697 t ((freenode "smf/freenode"
                                               ,(find-keychain-internet-password "smf" "smf.io"))))
                ("smf.io" 6697 t ((bitlbee "smf/bitlbee"
                                              ,(find-keychain-internet-password "smf" "smf.io"))))
                )

  ;; erc
  erc-prompt ">"
  erc-email-userid "sean.michael.farley@gmail.com"
  erc-autojoin-channels-alist '((".*smf.io.*" "&bitlbee" "#mercurial"))
  erc-prompt-for-password nil
  erc-auto-query 'buffer
  erc-query-display 'buffer
  erc-prompt-for-nickserv-password nil
  erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                            "324" "329" "332" "333" "353" "477")
  erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
  ;; erc-hl-nicks-nick-base-face '((t nil))
  erc-hl-nicks-color-contrast-strategy '(invert contrast)

  ;; erc modules
  erc-modules '(autojoin
                button
                completion
                dcc
                fill
                irccontrols
                match
                netsplit
                noncommands
                notify
                readonly
                ring                            ; Input history
                spelling
                stamp                           ; Time stamps
                track                           ; Mode line notification
                truncate)
)

(erc-update-modules)
(erc-autojoin-enable)
(erc-hl-nicks-enable)
(erc-menu-enable)
(erc-track-mode t)
(erc-spelling-mode 1)
(erc-button-mode 1)

(add-hook 'erc-after-connect
          (lambda (server nick)
            (add-hook 'erc-server-NOTICE-hook 'erc-auto-query)))

(defun erc-start-or-switch (channel)
  "Connect to ERC, or switch to given channel"
  (interactive)
  (if (get-buffer "*irc-freenode*") ;; ERC already active?
      (progn
        (set-buffer "*irc-freenode*")
        (if (erc-server-process-alive)
            (switch-to-buffer channel) ;; yes: switch to #channel
          (znc-all)))                  ;; no: start ERC
    (znc-all)))                        ;; no: start ERC

(global-set-key (kbd "C-c m") (lambda () (interactive) (erc-start-or-switch "#mercurial")))
(global-set-key (kbd "C-c k") (lambda () (interactive) (erc-start-or-switch "#kallithea")))

(defun user-keys (hashtable)
  "Return all keys in hashtable."
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys))

(defun buffer-users (buffer)
  "Return users for a given ERC buffer"
  (set-buffer buffer)
  (user-keys erc-channel-users))

(defun erc-chat (nick)
  "Start a query with nick"
  (interactive
   (list (completing-read "Nick: "
                          (append (buffer-users "&bitlbee")
                                  (buffer-users "#mercurial")))))
  (unless (memq nick (buffer-users "&bitlbee")) (set-buffer "#mercurial"))
  (erc-cmd-QUERY nick))

(global-set-key (kbd "C-x m") 'erc-chat)
