; Ctrl-Tab cycling
; from http://www.emacswiki.org/cgi-bin/wiki/ControlTABbufferCycling
(defvar stesla-hated-buffers '("KILL" "*Apropos*" "*Completions*" "*grep*"
                                ".newsrc-dribble" ".bbdb" "sent-mail" "*vc*"
                               "*Compile-Log*" "*Help*" "*Messages*" ".aux"))

(defvar stesla-hated-buffer-regexps
  '("^ " "*Buffer" "^\\*trace" "^\\*tramp" "^\\*"))

(setq iswitchb-buffer-ignore
  (append stesla-hated-buffer-regexps  stesla-hated-buffers))

(defmacro stesla-buffer-regexp-mapcar (regexp buffers)
  "Find BUFFERS whose name matches REGEXP"
  `(mapcar (lambda (this-buffer)
             (if (string-match ,regexp (buffer-name this-buffer))
                 this-buffer))
           ,(if (symbolp buffers) (symbol-value buffers) buffers)))

(defmacro stesla-hated-buffer-from-regexps (regexps)
  "Generate a one-dimensional list of buffers that match REGEXPS"
  (append
   '(append)
   (mapcar (lambda (regexp)
             `(delete nil (stesla-buffer-regexp-mapcar ,regexp
                                                       (buffer-list))))
           (if (symbolp regexps) (symbol-value regexps) regexps))))

(defun stesla-delete-from-list (delete-these from-list)
  "Delete DELETE-THESE from FROM-LIST."
  (cond
   ((car delete-these)
    (if (member (car delete-these) from-list)
        (stesla-delete-from-list (cdr delete-these)
                                (delete (car delete-these) from-list))
      (stesla-delete-from-list (cdr delete-these) from-list)))
   (t from-list)))

(defun stesla-hated-buffers ()
  "List of buffers I never want to see."
  (delete nil
          (append
           (mapcar 'get-buffer stesla-hated-buffers)
           (stesla-hated-buffer-from-regexps stesla-hated-buffer-regexps))))

;; `stesla-rotate-buffers': Like `bury-buffer' but with the capability to
;; exclude certain specified buffers.
(defun stesla-rotate-buffers (&optional n)
  "Switch to the Nth next buffer.  Negative arguments move backwards."
  (interactive)
  (unless n
    (setq n 1))
  (let ((my-buffer-list
         (stesla-delete-from-list (stesla-hated-buffers)
                                 (buffer-list (selected-frame)))))
    (switch-to-buffer
     (if (< n 0)
       (nth (+ (length my-buffer-list) n) my-buffer-list)
       (bury-buffer)
       (nth n my-buffer-list)))))

(global-set-key (kbd "C-<tab>") 'stesla-rotate-buffers)
(global-set-key (kbd "C-S-<tab>") (lambda ()
                                    (interactive)
                                    (stesla-rotate-buffers -1)))


