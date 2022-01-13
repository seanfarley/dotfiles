;;; ~/projects/dotfiles/doom/+twitter.el -*- lexical-binding: t; -*-

;; couldn't get 'after!' to work
;; (after! twittering-mode
(with-eval-after-load 'twittering-mode
  ;; from https://github.com/hayamiz/twittering-mode/issues/136
  (defun +twitter/rerender-all ()
    (interactive)
    (dolist (buf (doom-buffers-in-mode 'twittering-mode (buffer-list)))
      (with-current-buffer buf
        (twittering-rerender-timeline-all buf t)
        (setq-local line-spacing 0.2)
        (goto-char (point-min)))))

  (defun *twittering-generate-format-table (status-sym prefix-sym)
    `(("%" . "%")
      ("}" . "}")
      ("#" . (cdr (assq 'id ,status-sym)))
      ("'" . (when (cdr (assq 'truncated ,status-sym))
               "..."))
      ("c" .
       (let ((system-time-locale "C"))
         (format-time-string "%a %b %d %H:%M:%S %z %Y"
                             (cdr (assq 'created-at ,status-sym)))))
      ("d" . (cdr (assq 'user-description ,status-sym)))
      ("f" .
       (twittering-make-string-with-source-property
        (cdr (assq 'source ,status-sym)) ,status-sym))
      ("i" .
       (when (and twittering-icon-mode window-system)
         (let ((url
                (cond
                 ((and twittering-use-profile-image-api
                       (eq twittering-service-method 'twitter)
                       (or (null twittering-convert-fix-size)
                           (member twittering-convert-fix-size '(48 73))))
                  (let ((user (cdr (assq 'user-screen-name ,status-sym)))
                        (size
                         (if (or (null twittering-convert-fix-size)
                                 (= 48 twittering-convert-fix-size))
                             "normal"
                           "bigger")))
                    (format "http://%s/%s/%s.xml?size=%s" twittering-api-host
                            (twittering-api-path "users/profile_image") user size)))
                 (t
                  (cdr (assq 'user-profile-image-url ,status-sym))))))
           (twittering-make-icon-string nil nil url))))
      ("I" .
       (let* ((entities (cdr (assq 'entity ,status-sym)))
              text)
         (mapc (lambda (url-info)
                 (setq text (or (cdr (assq 'media-url url-info)) "")))
               (cdr (assq 'media entities)))
         (if (string-equal "" text)
             text
           (let ((twittering-convert-fix-size 360))
             (twittering-make-icon-string nil nil text)))))
      ("j" . (cdr (assq 'user-id ,status-sym)))
      ("L" .
       (let ((location (or (cdr (assq 'user-location ,status-sym)) "")))
         (unless (string= "" location)
           (concat " [" location "]"))))
      ("l" . (cdr (assq 'user-location ,status-sym)))
      ("p" . (when (cdr (assq 'user-protected ,status-sym))
               "[x]"))
      ("r" .
       (let ((reply-id (or (cdr (assq 'in-reply-to-status-id ,status-sym)) ""))
             (reply-name (or (cdr (assq 'in-reply-to-screen-name ,status-sym))
                             ""))
             (recipient-screen-name
              (cdr (assq 'recipient-screen-name ,status-sym))))
         (let* ((pair
                 (cond
                  (recipient-screen-name
                   (cons (format "sent to %s" recipient-screen-name)
                         (twittering-get-status-url recipient-screen-name)))
                  ((and (not (string= "" reply-id))
                        (not (string= "" reply-name)))
                   (cons (format "in reply to %s" reply-name)
                         (twittering-get-status-url reply-name reply-id)))
                  (t nil)))
                (str (car pair))
                (url (cdr pair))
                (properties
                 (list 'mouse-face 'highlight 'face 'twittering-uri-face
                       'keymap twittering-mode-on-uri-map
                       'uri url
                       'front-sticky nil
                       'rear-nonsticky t)))
           (when (and str url)
             (concat " " (apply 'propertize str properties))))))
      ("R" .
       (let ((retweeted-by
              (or (cdr (assq 'retweeting-user-screen-name ,status-sym)) "")))
         (unless (string= "" retweeted-by)
           (concat " (retweeted by " retweeted-by ")"))))
      ("S" .
       (twittering-make-string-with-user-name-property
        (cdr (assq 'user-name ,status-sym)) ,status-sym))
      ("s" .
       (twittering-make-string-with-user-name-property
        (cdr (assq 'user-screen-name ,status-sym)) ,status-sym))
      ("U" .
       (twittering-make-fontified-tweet-unwound ,status-sym))
      ;; ("D" .
      ;;  (twittering-make-fontified-tweet-unwound ,status-sym))
      ("T" .
       ,(twittering-make-fontified-tweet-text
         `(twittering-make-fontified-tweet-text-with-entity ,status-sym)
         twittering-regexp-hash twittering-regexp-atmark))
      ("t" .
       ,(twittering-make-fontified-tweet-text
         `(twittering-make-fontified-tweet-text-with-entity ,status-sym)
         twittering-regexp-hash twittering-regexp-atmark))
      ("u" . (cdr (assq 'user-url ,status-sym)))))

  (advice-add #'twittering-generate-format-table :override #'*twittering-generate-format-table)

  (defface twitter-divider
    `((t (:underline (:color "grey"))))
    "The vertical divider between tweets."
    :group 'twittering-mode)

  ;; add retweet info to twittering mode; hard to format into 80 columns
  (setq twittering-status-format
        "%FACE[font-lock-function-name-face]{  @%s}  %FACE[italic]{%@}  %FACE[error]{%FIELD-IF-NONZERO[❤ %d]{favorite_count}}  %FACE[warning]{%FIELD-IF-NONZERO[↺ %d]{retweet_count}}
%RT{%FOLD[   ]{%FILL{%FACE[shadow]{retweeted by }%FACE[font-lock-function-name-face]{@%s}}}\n}%FOLD[   ]{%FILL{%t}%QT{
%FOLD[   ]{%FACE[font-lock-function-name-face]{@%s}\t%FACE[shadow]{%@}
%FOLD[ ]{%FILL{%t}}
%FOLD[ ]{%I}
}}}

%FACE[twitter-divider]{                                                                                                }
"
        twittering-initial-timeline-spec-string
        '(":home" ":mentions")))
