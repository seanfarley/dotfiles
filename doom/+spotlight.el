
(setq locate-command "mdfind")
(setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)

(defun counsel-mac-app-action-default (app)
  "Launch DESKTOP-SHORTCUT."
  (call-process-shell-command
   (format "open '%s'" app)))

(defun counsel-launch-cmd-mdfind (input)
  "Return a shell command based on INPUT."
  (counsel-require-program "mdfind")
  ;; TODO create a spotlight-clone using below but keep this method to
  ;; 'kMDItemKind == Application'

  ;; replace spaces with '*' so that mdfind behaves like ivy
  (format (concat "mdfind -onlyin /Applications "
                  "-onlyin %s 'kMDItemDisplayName == \"*%s*\"c'")
          (getenv "HOME")
          (s-replace " " "*" input)))

;;;###autoload
(defun counsel-mac-app (&optional initial-input)
  "Call the \"mdfind\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (let ((counsel-locate-cmd #'counsel-launch-cmd-mdfind))
    (ivy-read "Launch: " #'counsel-locate-function
              :initial-input initial-input
              :dynamic-collection t
              :history 'counsel-locate-history
              :action #'counsel-mac-app-action-default
              :unwind #'counsel-delete-process
              :caller 'counsel-locate)))

(defun smf/test-frame-lose-focus ())


(cl-defun smf/ivy-read-frame (prompt collection
                           &key
                             predicate require-match initial-input
                             history preselect def keymap update-fn sort
                             action multi-action
                             unwind re-builder matcher
                             dynamic-collection caller
                             hammerspoon experimental-bury)
  "Wrapper that will launch a pop-up frame with `ivy-read'.

This code disallows multiple pop-ups by using the same buffer
name."
  (let ((buffer-name "*modal-ivy*"))
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (let ((frame (make-frame '((auto-raise . t)
                                   ;; (left-fringe . 0)
                                   ;; (right-fringe . 0)
                                   ;; (line-spacing . 3)
                                   ;; (menu-bar-lines . 0)
                                   (left . 0.5)
                                   (top . 0.5)
                                   (minibuffer . only)
                                   (undecorated . t)
                                   (unsplittable . t)
                                   (vertical-scroll-bars . nil)))))
          (select-frame-set-input-focus frame)
          (let ((ivy-height 20)
                (ivy-fixed-height-minibuffer nil)
                (ivy-add-newline-after-prompt nil)
                (ivy-count-format ""))
            (ivy-read prompt collection
                      :predicate predicate
                      :require-match require-match
                      :initial-input initial-input
                      :history history
                      :preselect preselect
                      :def def
                      :keymap keymap
                      :update-fn update-fn
                      :sort sort
                      :action action
                      :multi-action multi-action
                      :unwind (lambda ()
                                (when unwind (funcall unwind))
                                (when experimental-bury
                                  ;; need to have a way to bury emacs so that it returns to the previous application
                                  (call-process-shell-command "osascript -e 'tell application \"System Events\"' -e 'set frontProcess to first process whose frontmost is true' -e 'set visible of frontProcess to false' -e 'end tell' &" nil 0))
                                (when hammerspoon
                                  (call-process-shell-command "/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs -c 'backFromEmacs()'&" nil 0))
                                (kill-buffer buffer-name)
                                (delete-frame)
                                (other-window 1))
                      :re-builder re-builder
                      :matcher matcher
                      :dynamic-collection dynamic-collection
                      :caller caller)))))))


(defun smf/test-frame ()
  (smf/ivy-read-frame
   "Emacs acronyms: "
             '("Emacs: Escape-Meta-Alt-Control-Shift "
               "Emacs: Eight Megabytes And Constantly Swapping "
               "Emacs: Even a Master of Arts Comes Simpler ")
             :action (lambda (funny-quote)
                       (call-process-shell-command (format "osascript -e 'tell app \"System Events\" to display dialog \"%s\" buttons {\"OK\"}' &" funny-quote) nil 0))
             :hammerspoon t))

(defun counsel-mac-app-frame ()
  (let ((counsel-locate-cmd #'counsel-launch-cmd-mdfind))
    (smf/ivy-read-frame "Launch: " #'counsel-locate-function
              :dynamic-collection t
              :history 'counsel-locate-history
              :action #'counsel-mac-app-action-default
              :unwind #'counsel-delete-process
              :caller 'counsel-locate
              :hammerspoon t)))

(defun smf/foo (prompt &rest args)
  (smf/ivy-read-frame
   (if prompt prompt "DMenu: ")
   args
   :experimental-bury t))
