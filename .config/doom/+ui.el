;;; +ui.el -*- lexical-binding: t; -*-

;;; theme styling

(setq doom-theme 'doom-nord)

(when (version< "29.0.50" emacs-version)
  (pixel-scroll-precision-mode))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12))

;; BUG below line doesn't check `ns-use-native-fullscreen'
;; (progn (setq ns-use-native-fullscreen nil) (add-to-list 'initial-frame-alist '(fullscreen . fullboth)))

;; automatically start in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . fullboth))

;; auto scroll the compilation buffer
(setq compilation-scroll-output t)

;;; mru logic

;; keep persp names in order of most recently used
(after! persp-mode
  (add-hook 'persp-before-switch-functions
            #'(lambda (new-persp-name w-or-f)
                (let ((cur-persp-name (safe-persp-name (get-current-persp))))
                  (when (member cur-persp-name persp-names-cache)
                    (setq persp-names-cache
                          (cons cur-persp-name
                                (delete cur-persp-name persp-names-cache)))))))

  (add-hook 'persp-renamed-functions
            #'(lambda (persp old-name new-name)
                (setq persp-names-cache
                      (cons new-name (delete old-name persp-names-cache)))))

  (add-hook 'persp-before-kill-functions
            #'(lambda (persp)
                (setq persp-names-cache
                      (delete (safe-persp-name persp) persp-names-cache))))

  (add-hook 'persp-created-functions
            #'(lambda (persp phash)
                (when (and (eq phash *persp-hash*)
                           (not (member (safe-persp-name persp)
                                        persp-names-cache)))
                  (setq persp-names-cache
                        (cons (safe-persp-name persp) persp-names-cache)))))

  ;; the above keeps the persp cache in MRU order

  (defun smf/workspace-mru ()
    "Returns the mru order of workspaces.

The function filters out workspaces that start with a '*'."
    (cl-remove-if (lambda (it)
                    (or (string-prefix-p "*" it)
                        (string= "none" it)))
                  persp-names-cache))

  (defun smf/switch-to-last-workspace ()
    "Function to switch to last used workspace.

By 'last used,' I mean the last workspace that isn't an app. For
simplicity, just test if the workspace begins with an asterik."
    (when-let ((mru-list (smf/workspace-mru)))
      (+workspace-switch (car mru-list))))

  (defun smf/activate-emacs ()
    "When activating emacs, only switch to last workspace when in an app."
    (when (string-prefix-p "*" (+workspace-current-name))
      (smf/switch-to-last-workspace)))

  (defun smf/workspace-other ()
    (interactive)
    (when-let ((mru-list (smf/workspace-mru))
               (prev-workspace +workspace--last)
               (cur-workspace (+workspace-current-name)))
      ;; if we're coming from a previous *foo* workspace, the first entry in
      ;; `mru-list' is the current workspace; so use the second item

      (if (string= (nth 0 mru-list) cur-workspace)
          (+workspace/switch-to (nth 1 mru-list))
        (+workspace/switch-to (nth 0 mru-list))))))

;; https://www.n16f.net/blog/eye-level-window-centering-in-emacs/
(defcustom g-recenter-window-eye-level 0.2
  "The relative position of the line considered as eye level in the
current window, as a ratio between 0 and 1.")

(defun g-recenter-window ()
  "Scroll the window so that the current line is at eye level."
  (interactive)
  (let ((line (round (* (window-height) g-recenter-window-eye-level))))
    (recenter line)))

(global-set-key (kbd "C-l") 'g-recenter-window)

;; from https://stackoverflow.com/a/73760499
(defcustom auto-hide-compile-buffer-delay 0
  "Time in seconds before auto hiding compile buffer."
  :group 'compilation
  :type 'number)

(defun hide-compile-buffer-if-successful (buffer string)
  (setq compilation-total-time (time-subtract nil compilation-start-time))
  (setq time-str (concat " (Time: " (format-time-string "%s.%3N" compilation-total-time) "s)"))

  (if
      (with-current-buffer buffer
        (setq warnings (eval compilation-num-warnings-found))
        (setq warnings-str (concat " (Warnings: " (number-to-string warnings) ")"))
        (setq errors (eval compilation-num-errors-found))

        (if (eq errors 0) nil t))

      ;;If Errors then
      (message (concat "Compiled with Errors" warnings-str time-str))

    ;;If Compiled Successfully or with Warnings then
    (progn
      (bury-buffer buffer)
      (run-with-timer auto-hide-compile-buffer-delay nil 'delete-window (get-buffer-window buffer 'visible))
      (message (concat "Compiled Successfully" warnings-str time-str)))))

(make-variable-buffer-local 'compilation-start-time)

(defun compilation-started (proc)
  (setq compilation-start-time (current-time)))

(add-hook 'compilation-start-hook 'compilation-started)
(add-hook 'compilation-finish-functions 'hide-compile-buffer-if-successful)
