; -------------------------
; ediff
; -------------------------

(if (locate-library "ediff")
    (progn
      (autoload 'ediff-files "ediff")
      (autoload 'ediff-buffers "ediff")

      (defvar ediff-after-quit-hooks nil
        "* Hooks to run after ediff or emerge is quit.")

      (defadvice ediff-quit (after edit-after-quit-hooks activate)
        (run-hooks 'ediff-after-quit-hooks))

      (setq hg-mergetool-emacsclient-ediff-active nil)

      (setq ediff-window-setup-function 'ediff-setup-windows-plain)
      (setq ediff-split-window-function 'split-window-horizontally)
      (setq ediff-keep-variants nil)

      (defun local-ediff-before-setup-hook ()
        (setq local-ediff-saved-frame-configuration (current-frame-configuration))
        (setq local-ediff-saved-window-configuration (current-window-configuration))
        ;; (local-ediff-frame-maximize)
        (if hg-mergetool-emacsclient-ediff-active
            (raise-frame)))

      (defun local-ediff-quit-hook ()
        (set-frame-configuration local-ediff-saved-frame-configuration)
        (set-window-configuration local-ediff-saved-window-configuration))

      (defun local-ediff-suspend-hook ()
        (set-frame-configuration local-ediff-saved-frame-configuration)
        (set-window-configuration local-ediff-saved-window-configuration))

      (add-hook 'ediff-before-setup-hook 'local-ediff-before-setup-hook)
      (add-hook 'ediff-quit-hook 'local-ediff-quit-hook 'append)
      (add-hook 'ediff-suspend-hook 'local-ediff-suspend-hook 'append)

      ;; Useful for ediff merge from emacsclient.
      (defun hg-mergetool-emacsclient-ediff (local remote base merged)
        (setq hg-mergetool-emacsclient-ediff-active t)
        (if (file-readable-p base)
            (ediff-merge-files-with-ancestor local remote base nil merged)
          (ediff-merge-files local remote nil merged))
        (recursive-edit))

      (defun hg-mergetool-emacsclient-ediff-after-quit-hook ()
        (exit-recursive-edit))

      (add-hook 'ediff-after-quit-hooks 'hg-mergetool-emacsclient-ediff-after-quit-hook 'append)
))
