;;; ~/projects/dotfiles/doom/+ivy.el -*- lexical-binding: t; -*-

(after! ivy
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t))

(def-package! ivy-rich
  :init
  ;; define function for getting the icon
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))

  ;; redefining this here to change the switch-buffer widths; better max
  ;; lengths for my screen
  (setq-default
   ivy-rich--display-transformers-list
   '(ivy-switch-buffer
     (:columns
      ((ivy-rich-switch-buffer-icon (:width 4))
       ;; return the candidate itself
       (ivy-rich-candidate (:width 40))
       ;; return the buffer size
       (ivy-rich-switch-buffer-size (:width 7))
       ;; return the buffer indicators
       (ivy-rich-switch-buffer-indicators
        (:width 4 :face error :align right))
       ;; return the major mode info
       (ivy-rich-switch-buffer-major-mode (:width 20 :face warning))
       ;; return project name using `projectile'
       (ivy-rich-switch-buffer-project (:width 25 :face success))
       ;; return file path relative to project root or
       ;; `default-directory' if project is nil
       (ivy-rich-switch-buffer-path
        (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path
                             x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand)))

     counsel-M-x
     (:columns
      ;; the original transfomer
      ((counsel-M-x-transformer (:width 40))
       ;; return the docstring of the command
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))

     counsel-describe-function
     (:columns
      ;; the original transformer
      ((counsel-describe-function-transformer (:width 40))
       ;; return the docstring of the function
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))

     counsel-describe-variable
     (:columns
      ;; the original transformer
      ((counsel-describe-variable-transformer (:width 40))
       ;; return the docstring of the variable
       (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))

     counsel-recentf
     (:columns
      ;; return the candidate itself
      ((ivy-rich-candidate (:width 0.8))
       ;; return the last modified time of the file
       (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))))

  ;; (setq-default ivy-rich-switch-buffer-name-max-length 40
  ;;               ivy-rich-switch-buffer-project-max-length 25
  ;;               ivy-rich-switch-buffer-mode-max-length 20)

  ;; align the virtual buffers (apparently, this is temporarily removed)
  ;; (setq-default ivy-virtual-abbreviate 'full
  ;;               ivy-rich-switch-buffer-align-virtual-buffer t)

  ;; abbreviate paths using abbreviate-file-name (e.g. replace “/home/username”
  ;; with “~”)
  (setq-default ivy-rich-path-style 'abbreviate)

  :config
  (ivy-rich-mode))
