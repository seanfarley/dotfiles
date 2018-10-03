;;; ~/.doom.d/+org-starter.el -*- lexical-binding: t; -*-

(def-package! org-starter
  :init
  (setq-default
   ;; set up root org directory
   org-directory "~/Nextcloud/org"

   ;; set the file for capturing todos
   org-default-notes-file (concat org-directory "/inbox.org"))

  :config
  (when (all-the-icons-faicon "mercury")
    ;; icons for each org file, only works in gui mode
    (setq-default
     org-agenda-category-icon-alist
     `(("hg" ,(list (propertize (all-the-icons-faicon "mercury")
                                'face `(:family ,(all-the-icons-faicon-family) :height 1.3)
                                )) nil nil :height (16) :width (16) :ascent center)
       ("personal" ,(list (all-the-icons-faicon "user")) nil nil :ascent center)
       ("phd" ,(list (all-the-icons-faicon "superscript")) nil nil :ascent center))))


  (setq-default
   ;; add a nice, little template to use along with some shortcuts
   org-capture-templates
   '(("t" "Tasks" entry
      (file+headline (lambda () org-default-notes-file) "Inbox")
      "* TODO %?\n  Captured %<%Y-%m-%d %H:%M>\n  %a\n\n  %i")))

  ;; ;; shortcut to launch file for refiling
  ;; (smf/add-launcher "o" (lambda ()
  ;;                         (interactive)
  ;;                         (find-file org-default-notes-file)))

  (org-starter-def org-directory
    :files
    ("hg.org" :agenda t :refile (:maxlevel . 4))
    ("personal.org" :agenda t :refile (:maxlevel . 4))
    ("phd.org" :agenda t :refile (:maxlevel . 4))
    ("entertainment.org" :agenda nil))

  ;; after persp-mode is loaded, open all our org files and add them to the main
  ;; workspace
  (after! persp-mode
    (defun smf/org-init ()
      "method to load org files and agenda when emacs starts"
      (interactive)
      ;; load all org files
      (org-starter-load-all-files-in-path)
      (split-window-right)
      ;; start org-agenda
      (org-agenda nil "a")
      (display-buffer "*Org Agenda(a)*" t)

      ;; add the org files to the main workspace
      (persp-add-buffer
       (mapcar (lambda (f)
                 (file-name-nondirectory f)) org-starter-known-files)
       (persp-get-by-name +workspaces-main))

      ;; also add the agenda
      (persp-add-buffer "*Org Agenda(a)*" (persp-get-by-name +workspaces-main)))

    ;; once the scratch buffer has loaded it should be late enough to load the org
    ;; agenda files as well
    (add-hook 'doom-init-ui-hook #'smf/org-init)))
