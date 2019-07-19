;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; after persp-mode is loaded, open all our org files and add them to the main
;; workspace
(after! persp-mode
  (defun smf/org-init ()
    "method to load org files and agenda when emacs starts"
    (interactive)
    ;; start org-agenda
    (org-agenda nil "a")

    ;; add the org files to the main workspace
    (persp-add-buffer
     (mapcar (lambda (f) (file-name-nondirectory f))
             org-agenda-files)
     (persp-get-by-name +workspaces-main))

    ;; also add the agenda
    (persp-add-buffer "*Org Agenda(a)*" (persp-get-by-name +workspaces-main)))

  ;; once the scratch buffer has loaded it should be late enough to load the org
  ;; agenda files as well
  (add-hook 'doom-init-ui-hook #'smf/org-init))

(after! org
  (when (all-the-icons-faicon "mercury")
    ;; icons for each org file, only works in gui mode
    (setq-default
     org-agenda-category-icon-alist
     `(("hg" ,(list (propertize
                     (all-the-icons-faicon "mercury")
                     'face `(:family ,(all-the-icons-faicon-family) :height 1.3)))
        nil nil :height (16) :width (16) :ascent center)
       ("personal" ,(list (all-the-icons-faicon "user")) nil nil :ascent center)
       ("phd" ,(list (all-the-icons-faicon "superscript")) nil nil :ascent center))))

  (add-to-list
   ;; add a template to capture slack messages
   'org-capture-templates
   '("s" "Slack" entry
     (file+headline (lambda () +org-capture-todo-file) "Inbox")
     "* TODO %?\nCaptured %<%Y-%m-%d %H:%M>\n%(smf/slack-text-and-link)"
     :prepend t :kill-buffer t))

  (map!
   ;; I'll change the prefix for these function (instead of using smf/launcher)
   ;; since they are so common
   "C-c l" #'org-store-link
   "C-c a" #'org-agenda-list
   "C-c c" (lambda () (interactive) (org-capture nil "t"))

   :map org-mode-map
   ;; I use meta-arrow keys for navigation so let's stop org from
   ;; using them to indent
   "<M-S-left>" nil
   "<M-left>" nil
   "<M-right>" nil
   ;; since I commonly mistype =C-c C-'= instead of =C-c '=, let's
   ;; add that keybinding,
   "C-c C-'" #'org-edit-special

   ;; same as python
   "C-c <" #'org-shiftmetaleft
   "C-c >" #'org-shiftmetaright

   :map org-src-mode-map
   "C-c C-'" #'org-edit-src-exit
   ;; I find it infuriating that my muscle memory =⌘+s= in
   ;; =org-src-mode= will save the buffer as a new file. Instead,
   ;; let's make it do the same thing as =C-c '=
   "s-s" #'org-edit-src-exit)

  (setq-default
   ;; set up root org directory
   org-directory "~/Nextcloud/org/"

   org-agenda-files (mapcar (lambda (f) (concat org-directory f))
                            (list "inbox.org"
                                  "personal.org"
                                  "phd.org"
                                  "entertainment.org"))

   ;; set the file for capturing todos
   +org-capture-todo-file "inbox.org"
   org-default-notes-file (concat org-directory "/" +org-capture-todo-file)

   ;; don't auto-fold my documents
   org-startup-folded nil

   ;; automatically apply syntax highlighting:
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-ellipsis " ⤵"

   ;; and don't prompt
   org-confirm-babel-evaluate nil

   ;; when using imenu, make sure I can follow the outline to the full available
   ;; depth
   org-imenu-depth 6

   ;; also, I like using shift+arrow keys to highlight, so let's set that
   org-support-shift-select 'always

   ;; also search archive files
   ;; org-agenda-text-search-extra-files '(agenda-archives)

   ;; please don't close and mess up my windows,
   org-agenda-window-setup 'current-window
   org-agenda-restore-windows-after-quit 't

   ;; more agenda settings
   org-agenda-persistent-filter t
   org-agenda-sticky t

   org-todo-keywords '((sequence "TODO(t)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELED(c)"))

   ;; use a bit better looking colors for todo faces
   org-todo-keyword-faces '(("TODO" . (:foreground "OrangeRed" :weight bold))
                            ("SOMEDAY" . (:foreground "GoldenRod" :weight bold))
                            ("DONE" . (:foreground "LimeGreen" :weight bold))
                            ("CANCELED" . (:foreground "gray" :weight bold)))

   ;; misc todo settings
   org-enforce-todo-dependencies t
   org-use-fast-todo-selection t
   org-fast-tag-selection-single-key nil

   ;; force me to write a note about the task when marking it done
   org-log-done 'note
   org-log-into-drawer t
   org-log-state-notes-insert-after-drawers nil

   ;; also log when items are rescheduled and refiled
   org-log-reschedule 'time
   org-log-refile     'time

   ;; prepend the filename for each org target
   org-refile-use-outline-path 'file

   ;; since we're using ivy just put all the files + headers in a list
   org-outline-path-complete-in-steps nil

   ;; try to minimize blank lines
   org-cycle-separator-lines 0
   org-blank-before-new-entry '((heading)
                                (plain-list-item . auto))

   org-return-follows-link t
   org-confirm-babel-evaluate nil)

  ;; auto save all org files after doing a common action
  (advice-add 'org-agenda-quit      :before #'org-save-all-org-buffers)
  ;; (advice-add 'org-agenda-schedule  :after #'org-save-all-org-buffers)
  (advice-add 'org-agenda-todo      :after #'org-save-all-org-buffers)
  (advice-add 'org-agenda-refile    :after #'org-save-all-org-buffers)
  (advice-add 'org-agenda-clock-in  :after #'org-save-all-org-buffers)
  ;; (advice-add 'org-agenda-clock-out :after #'org-save-all-org-buffers)

  ;; (advice-add 'org-deadline         :after #'org-save-all-org-buffers)
  ;; (advice-add 'org-schedule         :after #'org-save-all-org-buffers)
  ;; (advice-remove 'org-schedule  #'org-save-all-org-buffers)

  (advice-add 'org-todo             :after #'org-save-all-org-buffers)
  (advice-add 'org-refile           :after #'org-save-all-org-buffers)
  ;; (advice-add 'org-clock-in         :after #'org-save-all-org-buffers)
  ;; (advice-add 'org-clock-out        :after #'org-save-all-org-buffers)
  (advice-add 'org-store-log-note   :after #'org-save-all-org-buffers)

  ;; also, let's turn on auto-fill-mode
  (add-hook 'org-mode-hook 'auto-fill-mode)

  (defun smf/org-capture-finalize ()
    "Call Hammerspoon to switch back to previous app."
    (call-process-shell-command "/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs -c 'backFromEmacs()' &"))

  ;; (add-hook 'org-capture-prepare-finalize-hook #'smf/org-capture-finalize)

                                        ;=================================== slack =================================

  (defun smf/slack-selected ()
    "Return the deep-linked url to a slack message and selected text.

It's mandatory that the user select the text because that's how
we figure out the deep link to which message.

The `sleep-for' is used to make sure we don't close the websocket
before receiving the message and to also make this behave
synchronously."
    (let ((json-msg nil)
          (ws-url (smf/slack-ws-url)))
      (let ((ws-slack (websocket-open
                       ws-url
                       :on-message (lambda (_websocket frame)
                                     (setq json-msg
                                           (json-read-from-string
                                            (websocket-frame-text frame))))))
            (js "
sel = window.getSelection();
selected_text = sel.toString();
if (selected_text.length != 0) {
    n = sel.anchorNode.parentNode;
    while (n !== document && !n.classList.contains('c-message')) {
        n = n.parentNode;
    }
    archive_url = n.getElementsByClassName('c-timestamp--static')[0].href;
    ns_0 = archive_url.lastIndexOf('/');
    ns_1 = archive_url.lastIndexOf('/', ns_0 - 1);
    raw_ts = archive_url.substr(ns_0 + 2);
    ts = raw_ts.slice(0, -6) + '.' + raw_ts.slice(-6);
    channel_id = archive_url.slice(ns_1 + 1, ns_0);
    team_id = boot_data['team_id'];
    deep_link = `team=${team_id}&id=${channel_id}&message=${ts}`;
    obj = {\"link\": deep_link, \"text\": selected_text};
    JSON.stringify(obj);
}
"))
        (websocket-send-text ws-slack
                             (json-encode `((id . 1337)
                                            (returnByValue . "true")
                                            (method . "Runtime.evaluate")
                                            (params . ((expression . ,js))))))
        (sleep-for 0.1)
        (websocket-close ws-slack))
      json-msg))

  (defun smf/slack-text-and-link ()
    "Format the selected text and create an org link."
    (let* ((res (alist-get 'result (alist-get 'result (smf/slack-selected))))
           (type (alist-get 'type res))
           (value (alist-get 'value res)))
      (if (string= type "undefined") (throw 'no-text-selected t))
      (let* ((ret (json-read-from-string value))
             (link (alist-get 'link ret))
             (text (string-trim (alist-get 'text ret))))
        (format "[[slack:channel?%s][%s]]" link text))))

  (defun smf/slack-ws-url ()
    "Return the most recently used slack url.

Luckily for us, this is ordered by most recently used (which will
have the selected text we want to capture for org-mode)."
    (interactive)
    (require 'json)
    (let* ((port 31337)
           (url (format "http://localhost:%d/json" port)))
      (with-current-buffer (url-retrieve-synchronously url t)
        (goto-char (point-min))
        (re-search-forward "^$")
        (alist-get 'webSocketDebuggerUrl (elt (json-read) 0)))))

  (defun smf/org-link-slack-follow (link)
    "Open LINK in slack."
    (call-process-shell-command
     (format "open 'slack://%s' &" link)))

  (org-link-set-parameters
   "slack"
   :follow #'smf/org-link-slack-follow)

  (defun smf/slack-capture ()
    "Convenience function to run `org-capture' with the slack template."
    ;; (let ((org-capture-entry (org-capture-select-template "s"))))
    ;; (org-capture))
    (+org-capture/open-frame nil "s"))

  (defun smf/org-agenda ()
    "Convenience function to switch workspace and display the org agenda."
    (+workspace-switch "main")
    (org-agenda-list)))
