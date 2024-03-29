;;; +org.el -*- lexical-binding: t; -*-

;; needs to be set before org loads
(setq org-directory "~/Nextcloud/org/")

(defvar +smf/org-roam-workspace-name "*org-roam*")

(setq-hook! 'TeX-mode-hook TeX-engine 'luatex)

(setq +latex-viewers '(pdf-tools))

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

(defadvice! smf/org-roam-in-own-workspace-a (&rest _)
  "Open all roam buffers in their own workspace."
  :before #'org-roam-node-find
  :before #'org-roam-node-random
  :before #'org-roam-buffer-display-dedicated
  :before #'org-roam-buffer-toggle
  ;; we also cram regular orgmode buffers into roam
  :before #'org-agenda
  :before #'org-agenda-list
  :before #'org-agenda-filter
  :before #'org-todo
  :before #'org-todo-list
  (when (modulep! :ui workspaces)
    (+workspace-switch +smf/org-roam-workspace-name t)))

(defun =org-roam ()
  (interactive)
  (when (modulep! :ui workspaces)
    (+workspace-switch +smf/org-roam-workspace-name t)
    ;; try the first buffer to see if it's org-roam
    (unless (string=
             (buffer-local-value 'default-directory
                                 (window-buffer (selected-window)))
             org-roam-directory)
      ;; if the first buffer isn't org-roam, then iterate over the buffer list
      (if-let ((roam-bufs (cl-remove-if-not
                       (lambda (buf)
                         (string=
                          (buffer-local-value 'default-directory buf)
                          org-roam-directory))
                       (buffer-list))))
        ;; if we have any org-roam buffers, then switch to the first one;
        ;; otherwise call `org-roam-node-find'
          (switch-to-buffer (car roam-bufs))
        (org-roam-node-find)))))

;; oddly, the side org roam buffer displays a non-working state at first but
;; refreshing it fixes the backlinks
(defadvice! smf/org-roam-refresh (&rest _)
  :after #'org-roam-buffer-toggle
  (with-current-buffer org-roam-buffer
    (org-roam-buffer-refresh)))

(after! org-roam
  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (setq org-roam-node-display-template
        (format "${doom-hierarchy:*} %s %s %s"
                (propertize "${backlinkscount:6}" 'face 'org-footnote)
                (propertize "${doom-type:12}" 'face 'font-lock-keyword-face)
                (propertize "${doom-tags:42}" 'face 'org-tag))))

(after! org
  ;; use svg instead of png latex previews; much crisper on a retina screen
  ;; (presumably dvipng doesn't know about HiDPI)
  (setq org-preview-latex-default-process 'dvisvgm)

  (setq org-highlight-latex-and-related '(native script entities))

  (setq org-startup-with-latex-preview t)

  (setq org-latex-default-figure-position "H")

  (setq org-format-latex-options
        (plist-put org-format-latex-options :background "Transparent"))

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
   ;; set the file for capturing todos
   +org-capture-todo-file "inbox.org"
   org-default-notes-file (concat org-directory "/" +org-capture-todo-file)

   org-agenda-files (mapcar (lambda (f) (concat org-directory f))
                            (list +org-capture-todo-file
                                  "personal.org"
                                  "phd.org"))

   ;; don't auto-fold my documents
   org-startup-folded nil

   ;; automatically apply syntax highlighting:
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-ellipsis " ▾ "

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

   ;; inspired from doom's defaults
   org-todo-keywords
   '((sequence
      "TODO(t)"                  ; A task that needs doing & is ready to do
      "SOMEDAY(s)"               ; A task to revisit in the future
      "PROJ(p)"                  ; A project, which usually contains other tasks
      "WAIT(w)"                  ; Something external is holding up this task
      "HOLD(h)"                  ; This task is paused/on hold because of me
      "|"
      "DONE(d)"         ; Task successfully completed
      "CANCELED(c)"))   ; Task was cancelled, aborted or is no longer applicable

   org-todo-keyword-faces
   '(("SOMEDAY"  . +org-todo-onhold)
     ("HOLD"     . +org-todo-onhold)
     ("WAIT"     . +org-todo-onhold)
     ("PROJ"     . +org-todo-project)
     ("CANCELED" . +org-todo-cancel))

   org-agenda-deadline-faces
   '((1.001 . error)
     (1.0 . org-warning)
     (0.5 . org-upcoming-deadline)
     (0.0 . org-upcoming-distant-deadline))

   ;; for some reason 'auto clips the right-most two or three characters
   org-agenda-tags-column 150

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
   (cl-remove-if (lambda (elt) (string= "t" (car elt))) org-capture-templates))

  (add-to-list 'org-capture-templates
               '("t" "Personal todo" entry
                 (file+headline +org-capture-todo-file "Inbox")
                 "* HOLD %?\n%i\n%a" :prepend t)) ;; on hold until filed

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
  (add-hook! (org-mode org-roam-mode)
             #'auto-fill-mode
             (display-line-numbers-mode -1)))

(defun smf/org-capture-finalize ()
  "Call Hammerspoon to switch back to previous app."
  (call-process-shell-command "/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs -c 'backFromEmacs()' &"))

;; (add-hook 'org-capture-prepare-finalize-hook #'smf/org-capture-finalize)

;;; org-noter-pdftools

(add-hook! 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)

;;* org modern

(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

(custom-set-faces!
  '(org-document-title :height 1.2))

;; Org files can be rather nice to look at, particularly with some of the
;; customisations here. This comes at a cost however, expensive font-lock.
;; Feeling like you’re typing through molasses in large files is no fun, but
;; there is a way I can defer font-locking when typing to make the experience
;; more responsive.
(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook! 'org-mode #'locally-defer-font-lock)

(add-hook! 'org-mode #'+org-pretty-mode)

(appendq! +ligatures-extra-symbols
          `(:checkbox      "☐"
            :pending       "■"
            :checkedbox    "☑"
            :list_property "∷"
            :em_dash       "—"
            :ellipses      "…"
            :arrow_right   "→"
            :arrow_left    "←"
            :title         "🅣"
            :subtitle      "ⓣ"
            :author        "✎"
            :date          "🇩"
            :property      "⛭"
            :options       "⌥"
            :startup       "⏻"
            :macro         "🄼"
            :html_head     "🅷"
            :html          "🅗"
            :latex_class   "🄻"
            :latex_header  "🅻"
            :beamer_header "🅑"
            :latex         "🅛"
            :bibliography  ""
            :print_bib     ""
            :attr_latex    "🄛"
            :attr_html     "🄗"
            :attr_org      "⒪"
            :begin_quote   "❝"
            :end_quote     "❞"
            :caption       "☰"
            :header        "›"
            :results       "⇢"
            :begin_export  "⏩"
            :end_export    "⏪"
            :properties    "🟍"
            :end           "∎"
            :priority_a   ,(propertize "⚑" 'face 'nerd-icons-red)
            :priority_b   ,(propertize "⬆" 'face 'nerd-icons-orange)
            :priority_c   ,(propertize "■" 'face 'nerd-icons-yellow)
            :priority_d   ,(propertize "⬇" 'face 'nerd-icons-green)
            :priority_e   ,(propertize "" 'face 'nerd-icons-blue)))

(set-ligatures! 'org-mode
  :merge t
  :checkbox      "[ ]"
  :pending       "[-]"
  :checkedbox    "[X]"
  :list_property "::"
  :em_dash       "---"
  :ellipsis      "..."
  :arrow_right   "->"
  :arrow_left    "<-"
  :title         "#+title:"
  :subtitle      "#+subtitle:"
  :author        "#+author:"
  :date          "#+date:"
  :property      "#+property:"
  :options       "#+options:"
  :startup       "#+startup:"
  :macro         "#+macro:"
  :html_head     "#+html_head:"
  :html          "#+html:"
  :latex_class   "#+latex_class:"
  :latex_header  "#+latex_header:"
  :bibliography  "#+bibliography:"
  :print_bib     "#+print_bibliography:"
  :beamer_header "#+beamer_header:"
  :latex         "#+latex:"
  :attr_latex    "#+attr_latex:"
  :attr_html     "#+attr_html:"
  :attr_org      "#+attr_org:"
  :begin_quote   "#+begin_quote"
  :end_quote     "#+end_quote"
  :caption       "#+caption:"
  :header        "#+header:"
  :begin_export  "#+begin_export"
  :end_export    "#+end_export"
  :results       "#+RESULTS:"
  :properties    ":PROPERTIES:"
  :end           ":END:"
  :priority_a    "[#A]"
  :priority_b    "[#B]"
  :priority_c    "[#C]"
  :priority_d    "[#D]"
  :priority_e    "[#E]")

(plist-put +ligatures-extra-symbols :name "⁍")

(use-package! org-modern
  :after (org org-agenda org-roam)
  :init
  (global-org-modern-mode)
  :config
  (setq org-modern-star ["◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"]
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•"))
        org-modern-todo-faces
        '(("TODO"     :inverse-video t :inherit org-todo)
          ("SOMEDAY"  :inverse-video t :inherit +org-todo-onhold)
          ("PROJ"     :inverse-video t :inherit +org-todo-project)
          ("HOLD"     :inverse-video t :inherit +org-todo-onhold)
          ("WAIT"     :inverse-video t :inherit +org-todo-onhold)
          ("CANCELED" :inverse-video t :inherit +org-todo-cancel))
        org-modern-block nil
        org-modern-progress nil
        org-modern-keyword nil

        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞")
          ("export" "⏩" "⏪")))

  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))

(after! spell-fu
  (pushnew 'org-modern-tag (alist-get 'org-mode +spell-excluded-faces-alist)))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

;;* org latex

(after! org-src
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))

(after! ox-latex
  (setq org-latex-src-block-backend 'engraved))

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package! laas
  :hook ((LaTeX-mode org-mode) . laas-mode)
  :config
  (defun laas-tex-fold-maybe ()
    (unless (equal "/" aas-transient-snippet-key)
      (+latex-fold-last-macro-a)))
  (add-hook 'aas-post-snippet-expand-hook #'laas-tex-fold-maybe))

(when (>= emacs-major-version 28)
  (add-hook! 'latex-mode #'TeX-latex-mode))

;;; org clock prettify

;; compactify the org-clock modeline into an emoji and move the org task name
;; into a tooltip; stolen from
;; https://github.com/dsedivec/dot-emacs-d/blob/master/recipes/org-compact-clock-in-mode-line.el

(defadvice! smf/org-clock-compact-clock-string (orig-fun)
  :around #'org-clock-get-clock-string
  (require 'mu4e)
  (if org-clock-effort
      (funcall orig-fun)
    ;; should move the `save-window-excursion' into `+mu4e--get-string-width'
    (save-window-excursion
      (let* ((clocked-time (org-clock-get-clocked-time))
             (icon (nerd-icons-mdicon
                    "nf-md-camera_timer" ;; or nf-weather-time_4
                    :face 'org-done
                    :height 0.8
                    :v-adjust -0.15))
             (icon-width (+mu4e--get-string-width icon))
             (space-width (+mu4e--get-string-width " "))
             (space-factor (- 2 (/ (float icon-width) space-width)))
             (space (propertize
                     " " 'display `(space . (:width ,space-factor)))))
        (concat
         space
         icon
         space
         (propertize
          (org-duration-from-minutes clocked-time)
          'face '(org-done :height 0.85)
          ;; 'display '(raise 0.2)
          ))))))

(defadvice! smf/org-clock-mode-line-task-name-in-tooltip (&rest _args)
  :after #'org-clock-update-mode-line
  (let* ((start-idx (if (and org-clock-task-overrun-text
                             (string-prefix-p org-clock-task-overrun-text
                                              org-mode-line-string))
                        (length org-clock-task-overrun-text)
                      0))
         (old-help-echo (get-text-property start-idx 'help-echo
                                           org-mode-line-string))
         (old-help-echo (string-replace "Org mode clock is running.\n"
                                        "" old-help-echo))
         (todo-state (save-excursion
                       (with-current-buffer (org-clocking-buffer)
                         (goto-char (marker-position org-clock-hd-marker))
                         (org-get-todo-state)))))
    (put-text-property start-idx (- (length org-mode-line-string) start-idx)
                       'help-echo (concat (propertize (concat todo-state org-clock-heading "\n")
                                                      'face (org-get-todo-face todo-state))
                                          old-help-echo)
                       org-mode-line-string)))
