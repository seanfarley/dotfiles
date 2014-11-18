; -------------------------
; Custom functions
; -------------------------

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "H-;") 'comment-or-uncomment-region-or-line)

; Whole-line or region
(require 'whole-line-or-region)
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and
   we are not at the end of the line, then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts
   comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(whole-line-or-region-mode)
(global-set-key "\M-;" 'comment-dwim-line)

; Wrap region with a tag
(defun my-insert-tags (tag)
  (interactive "sTag: ")
  (if (region-active-p)
      (let ((beg (region-beginning)))
        (save-excursion
          (goto-char (region-end))
          (insert "</" (car (split-string tag)) ">")
          (goto-char beg)
          (insert "<" tag ">")))
    (insert "<" tag ">")
    (save-excursion
      (insert "</" (car (split-string tag)) ">"))))

;; Change cursor color according to mode; inspired by
;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
(setq djcb-read-only-color       "gray")
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see thep docs for set-cursor-type

(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color       "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color          "gray")
(setq djcb-normal-cursor-type    'box)

(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."

  (cond
    (buffer-read-only
      (set-cursor-color djcb-read-only-color)
      (setq cursor-type djcb-read-only-cursor-type))
    (overwrite-mode
      (set-cursor-color djcb-overwrite-color)
      (setq cursor-type djcb-overwrite-cursor-type))
    (t
      (set-cursor-color djcb-normal-color)
      (setq cursor-type djcb-normal-cursor-type))))

(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; C-x k == C-x # when editing emacsclient is waiting
(add-hook 'server-switch-hook
          (lambda ()
            (local-set-key (kbd "H-k") '(lambda ()
                                            (interactive)
                                            (if server-buffer-clients
                                                (server-edit)
                                              (kill-this-buffer))))))

(defun delete-to-end-of-buffer (add-to-kill-ring-p)
  "Deletes from point to end of buffer. If prefix argument is
given, kill the region, adding it to the kill ring."
  (interactive "P")
  (if add-to-kill-ring-p
      (kill-region (point) (point-max))
    (delete-region (point) (point-max))))

(global-set-key (kbd "C-M-d") 'delete-to-end-of-buffer)

(defun underline-with-char (char)
  (interactive (list (read-from-minibuffer "Char: ")))
  (when (= 0 (length char))
    (error "Need a character"))
  (setq char (aref char 0))             ; Ignore everything but the first char.
  (save-excursion
    (goto-char (point-at-eol))
    (insert "\n"
            (make-string (- (point-at-eol)
                            (point-at-bol))
                         char))))

(defun os-x-version ()
  "Returns whether we are running OS X Lion or Mountain Lion"
  (let ((major-version-number (substring (shell-command-to-string "sw_vers -productVersion") 0 5)))
    (cond
     ((string-equal major-version-number "10.7.") 'os-x-lion)
     ((string-equal major-version-number "10.8.") 'os-x-mountain-lion)
     ((string-equal major-version-number "10.9.") 'os-x-mountain-lion)
     ((string-equal major-version-number "10.10") 'os-x-mountain-lion))))

(defun find-keychain-password-mountain-lion (type account server)
  (condition-case nil
      (first
       (process-lines
        "security" (concat "find-" type "-password")
        "-w" ; display only the password to stdout
        "-a" account
        "-s" server)) ; TODO: handle case when keychain does not return an entry
    (error "")))

(defun find-keychain-password-lion (type account server)
  (let ((password-line (first
                        (process-lines
                         "security"  (concat "find-" type "-password")
                         "-g" ; display only the password to stdout
                         "-a" account
                         "-s" server
                         ))))
    (string-match "password: \"\\(.*\\)\"" password-line)
    (match-string 1 password-line)))

(defun find-keychain-password (type account server)
  (let ((os-x-version (os-x-version)))
    (cond
     ((eq os-x-version 'os-x-lion)
      (find-keychain-password-lion type account server))
     ((eq os-x-version 'os-x-mountain-lion)
      (find-keychain-password-mountain-lion type account server)))))

(defun find-keychain-internet-password (account server)
  (find-keychain-password "internet" account server))

(defun find-keychain-generic-password (account server)
  (find-keychain-password "generic" account server))

(defun my-delete-backward-to-ws ()
  (interactive)
  (if (and transient-mark-mode mark-active)
    (kill-region (point) (mark))
    (progn
      (delete-region (point) (save-excursion (skip-syntax-backward " ") (point)))
      (delete-region (point) (save-excursion (skip-syntax-backward "^ ") (point))))))
(global-set-key "\C-w" 'my-delete-backward-to-ws)

;; auto bury the compilation buffer if successful
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)


;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
