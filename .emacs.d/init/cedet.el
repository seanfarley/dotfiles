; CEDET
(load-file "~/.emacs.d/plugins/cedet/common/cedet.el")

; bug fix for 1.1 (fixed in trunk)
(unless (boundp 'x-max-tooltip-size)
  (setq x-max-tooltip-size '(80 . 40)))

; semantic loading
(semantic-load-enable-gaudy-code-helpers)
(require 'semanticdb)
(require 'semantic-ia)
(require 'semantic-gcc)
(require 'eieio-opt)

; Gtags
(when (require 'gtags)
  (global-set-key "\C-cf" 'gtags-find-file)
  (global-set-key "\M-." 'gtags-find-tag)
  (global-set-key (kbd "C->") 'gtags-find-tag-from-here)
  (global-set-key "\M-*" 'gtags-pop-stack)
  (define-key gtags-mode-map (kbd "C-c r") 'gtags-find-rtag)
  (define-key gtags-select-mode-map "p" 'previous-line)
  (define-key gtags-select-mode-map "n" 'next-line)
  (define-key gtags-select-mode-map "q" 'gtags-pop-stack)
  (define-key gtags-select-mode-map "\C-m" 'gtags-select-tag)
  (define-key gtags-select-mode-map " " 'gtags-select-tag)
  (define-key gtags-select-mode-map "\C-o" 'gtags-select-tag-other-window)
  (message "Loaded gtags")
  (global-semantic-tag-folding-mode)
  (global-semantic-idle-scheduler-mode 1) ;The idle scheduler with automatically reparse buffers in idle time.
  (global-semantic-idle-completions-mode 1) ;Display a tooltip with a list of possible completions near the cursor.
  (global-semantic-idle-summary-mode 1) ;Display a tag summary of the lexical token under the cursor.
  (when (require 'semanticdb-global)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode))
)

; custom function to compile "standard" things
(defun compile-pkg (&optional command startdir)
  "Compile a package, moving up to the parent directory
  containing configure.ac, if it exists. Start in startdir if defined,
  else start in the current directory."
  (interactive)

  (let ((dirname) (dir-buffer nil))
    (setq startdir (expand-file-name (if startdir startdir ".")))
    (setq command  (if command command compile-command))

    (setq dirname (upward-find-file "config.log" startdir))
    (setq dirname (if dirname dirname (upward-find-file "configure.log" startdir)))
    (setq dirname (if dirname dirname (upward-find-file "Makefile" startdir)))
    (setq dirname (if dirname dirname (expand-file-name ".")))
    ; We've now worked out where to start. Now we need to worry about
    ; calling compile in the right directory
    (save-excursion
      (setq dir-buffer (find-file-noselect dirname))
      (set-buffer dir-buffer)
      (compile command)
      (kill-buffer dir-buffer))))

(defun upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
  manage to find it, return the containing directory. Else if we
  get to the toplevel directory and still can't find it, return
  nil. Start at startdir or . if startdir not given"

  (let ((dirname (expand-file-name (if startdir startdir ".")))
        (found nil) ; found is set as a flag to leave loop if we find it
        (top nil))  ; top is set when we get to / so that we only check it once

    ; While we've neither been at the top last time nor have we found the file.
    (while (not (or found top))
      ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/") (setq top t))

      ; Check for the file
      (if (file-exists-p (expand-file-name filename dirname)) (setq found t)
        ; If not, move up a directory
        (setq dirname (expand-file-name ".." dirname))))
    ; return statement
    (if found dirname nil)))

(defun std-compile ()
  "Like 'compile', but uses compile-pkg"
  (interactive)
  (compile-pkg compile-command))

(defun anything-semantic-construct-candidates (tags depth)
  (apply 'append (mapcar (lambda (tag)
    (if (listp tag)
      (let ((type (semantic-tag-type tag))
            (class (semantic-tag-class tag)))
      (if (or (and (stringp type)
                   (string= type "class"))
              (eq class 'function)
              (eq class 'variable))
          (cons (cons (concat (make-string (* depth 2) ?\s)
                              (semantic-format-tag-summarize tag nil t)) tag)
                (anything-semantic-construct-candidates (semantic-tag-components tag) (1+ depth)))))))
                         tags)))

(defvar anything-c-source-semantic
  '((name . "Semantic Tags")
    (init . (lambda ()
              (setq anything-semantic-candidates
                    (condition-case nil
                        (anything-semantic-construct-candidates (semantic-fetch-tags) 0)
                      (error nil)))))
    (candidates . (lambda ()
                    (if anything-semantic-candidates
                        (mapcar 'car anything-semantic-candidates))))
    (action . (("Goto tag" . (lambda (candidate)
                               (let ((tag (cdr (assoc candidate anything-semantic-candidates))))
                                 (semantic-go-to-tag tag))))))))

; -------------------------
; Hooks and keybindings
; -------------------------

; c-mode-common-hook
(defun my-c-mode-hook ()
  (gtags-mode t)
  (linum-mode 1)
  (define-key c-mode-map (kbd "C-c c") 'std-compile)
)
(add-hook 'c-mode-hook 'my-c-mode-hook)

