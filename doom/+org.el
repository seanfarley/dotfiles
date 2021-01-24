;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; needs to be set before org loads
(setq org-directory "~/Nextcloud/org/")

(after! org
  ;; icons used to be here but they caused line height problems

  (defun smf/org-capture ()
    (interactive)
    (org-capture nil "t"))

  (map!
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
   org-agenda-files (mapcar (lambda (f) (concat org-directory f))
                            (list "inbox.org"
                                  "personal.org"
                                  "coiled.org"
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

  (defun smf/org-agenda ()
    "Convenience function to switch workspace and display the org agenda."
    (+workspace-switch "main")
    (org-agenda-list)))

;=================================== org-ref ===================================

(setq bibtex-completion-bibliography '("~/Nextcloud/refs/master.bib")
      bibtex-completion-library-path '("~/Nextcloud/refs/pdfs")
      bibtex-completion-additional-search-fields '(tags))

(setq org-ref-bibliography-notes "~/Nextcloud/refs/notes.org"
      org-ref-default-bibliography '("~/Nextcloud/refs/master.bib")
      org-ref-pdf-directory (expand-file-name "~/Nextcloud/refs/pdfs/"))

;============================== org-noter-pdftools =============================

(add-hook! 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)
