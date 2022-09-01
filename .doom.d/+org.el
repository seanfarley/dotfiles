;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; needs to be set before org loads
(setq org-directory "~/Nextcloud/org/")

;; NOTE this adds a latex-macros block to org-mode and is useful to copy to a
;; project's .dir-locals.el
;; ((nil . ((eval . (progn
;;                    (add-to-list 'org-src-lang-modes '("latex-macros" . latex))
;;                    (defvar org-babel-default-header-args:latex-macros
;;                      '((:results . "raw")
;;                        (:exports . "results")))
;;                    (defun prefix-all-lines (pre body)
;;                      (with-temp-buffer
;;                        (insert body)
;;                        (string-insert-rectangle (point-min) (point-max) pre)
;;                        (buffer-string)))
;;                    (defun org-babel-execute:latex-macros (body _params)
;;                      (concat
;;                       (prefix-all-lines "#+LATEX_HEADER: " body)
;;                       "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
;;                       (prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
;;                       "\n#+HTML_HEAD_EXTRA: \\)</div>\n")))))))

(use-package! org-capture
  ;; TODO try to remove defer in favor of a :command, if possible
  :defer t)

(after! org
  ;; icons used to be here but they caused line height problems

  (defun smf/org-capture ()
    (interactive)
    (org-capture nil "t"))

  (defun smf/org-roam-insert-key ()
    "Add and return a roam_key to Org-roam file."
    (interactive)
    (unless org-roam-mode (org-roam-mode))
    (let* ((key (read-string "Key: ")))
      (when (string-empty-p key)
        (user-error "Key can't be empty"))
      (org-roam--set-global-prop "roam_key" key)
      key))

  (setq-default
   org-agenda-files (mapcar (lambda (f) (concat org-directory f))
                            (list "inbox.org"
                                  "personal.org"
                                  "phd.org"))

   ;; set the file for capturing todos
   +org-capture-todo-file "inbox.org"
   org-default-notes-file (concat org-directory "/" +org-capture-todo-file)

   ;; don't auto-fold my documents
   org-startup-folded nil

   ;; automatically apply syntax highlighting:
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-ellipsis " ⮯"

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
   org-confirm-babel-evaluate nil

   org-roam-capture-ref-templates
   '(("r" "ref" plain
      "%(when-let ((txt \"%i\")) (unless (string-empty-p txt) (concat \"#+begin_quote\n\" (s-word-wrap 80 (string-trim txt)) \"\n#+end_quote\n\")))%?"
      :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
      :unnarrowed t))

   org-capture-templates
   (remove '("t" "Personal todo" entry
             (file+headline +org-capture-todo-file "Inbox")
             "* [ ] %?\n%i\n%a" :prepend t) org-capture-templates))

  (add-to-list 'org-capture-templates
               '("t" "Personal todo" entry
                 (file+headline +org-capture-todo-file "Inbox")
                 "* TODO %?\n%i\n%a" :prepend t))

  (add-to-list 'org-latex-packages-alist '("" "tikz" t))

  (eval-after-load "preview"
    '(add-to-list 'preview-default-preamble
                  "\\PreviewEnvironment{tikzpicture}" t))

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
  (add-hook! org-mode
             #'auto-fill-mode))

(defun smf/org-capture-finalize ()
  "Call Hammerspoon to switch back to previous app."
  (call-process-shell-command "/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs -c 'backFromEmacs()' &"))

;; (add-hook 'org-capture-prepare-finalize-hook #'smf/org-capture-finalize)

(defun smf/org-agenda ()
  "Convenience function to switch workspace and display the org agenda."
  (interactive)
  (smf/org-roam)
  (org-agenda-list))

(defun smf/org-roam ()
  "Switch to *org-roam* workspace or create it."
  (interactive)
  (+workspace-switch "*org-roam*" t)
  (unless (get-buffer "*Org Agenda(t)*")
    (org-todo-list)))

;==================================== bibtex ===================================

(setq bibtex-completion-bibliography `(,(expand-file-name "~/Nextcloud/refs/main.bib"))
      bibtex-completion-library-path `(,(expand-file-name "~/Nextcloud/refs/pdfs"))
      bibtex-completion-additional-search-fields '(keywords tags)
      bibtex-completion-notes-path (expand-file-name "~/Nextcloud/refs/notes/")
      bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n")

(setq citar-bibliography bibtex-completion-bibliography)

;============================== org-noter-pdftools =============================

(add-hook! 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)
