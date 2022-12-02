;;; +biblio.el -*- lexical-binding: t; -*-

;;; bibtex

(after! bibtex
  (setq bibtex-field-delimiters 'double-quotes
        ;; betterbib doesn't align the equal signs, so we disable that in emacs,
        ;; as well
        bibtex-align-at-equal-sign nil ; betterbib
        bibtex-text-indentation 5      ; betterbib
        bibtex-field-indentation 1     ; betterbib
        bibtex-completion-bibliography `(,(expand-file-name "~/Nextcloud/refs/main.bib"))
        bibtex-completion-library-path `(,(expand-file-name "~/Nextcloud/refs/pdfs"))
        bibtex-completion-additional-search-fields '(keywords tags)
        bibtex-completion-notes-path (expand-file-name "~/Nextcloud/refs/notes/")
        bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"))

(after! (citar bibtex)
  (setq citar-bibliography bibtex-completion-bibliography
        citar-library-paths bibtex-completion-library-path
        citar-symbols
        `((file ,(all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-faicon "sticky-note" :face 'all-the-icons-blue :v-adjust -0.1) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))
        citar-symbol-separator " ")

  (map! :map citar-map
        "a" #'citar-add-file-to-library))

(after! embark
  ;; define the keymap
  (defvar smf/citar-embark-become-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c n b") 'citar-open-notes)
      (define-key map (kbd "x") 'biblio-arxiv-lookup)
      (define-key map (kbd "c") 'biblio-crossref-lookup)
      (define-key map (kbd "i") 'biblio-ieee-lookup)
      (define-key map (kbd "h") 'biblio-hal-lookup)
      (define-key map (kbd "s") 'biblio-dissemin-lookup)
      (define-key map (kbd "b") 'biblio-dblp-lookup)
      (define-key map (kbd "g") 'biblio-gscholar-lookup)
      map)
    "Citar Embark become keymap for biblio lookup.")

  ;; tell embark about the keymap
  (add-to-list 'embark-become-keymaps #'smf/citar-embark-become-map))

(defadvice! smf/biblio-vertico ()
  :override #'biblio--completing-read-function
  #'completing-read-default)

(defun smf/biblio--selection-insert-at-end-of-bibfile-callback (bibtex entry)
  "Add BIBTEX (from ENTRY) to end of a user-specified bibtex file."
  (let* ((bib-file (car bibtex-completion-bibliography))
         (buf (or (get-file-buffer bib-file)
                  (find-buffer-visiting bib-file)))
         (need-close (not buf))
         (buf (find-file-noselect bib-file)))

    (with-current-buffer buf
      (goto-char (point-max))
      (insert "\n")
      (insert bibtex)
      (save-buffer))
    (when need-close (kill-buffer buf))
    (message "Inserted bibtex entry for %S."
             (biblio--prepare-title (biblio-alist-get 'title entry)))))

(defun smf/biblio-selection-insert-end-of-bibfile ()
  "Insert BibTeX of current entry at the end of user-specified bibtex file."
  (interactive)
  (biblio--selection-forward-bibtex
   #'smf/biblio--selection-insert-at-end-of-bibfile-callback))

(map! :map biblio-selection-mode-map
  "a" #'smf/biblio-selection-insert-end-of-bibfile)

(defadvice! smf/add-biblio-binding (&rest _)
  :after #'biblio-selection-mode

  ;; removes the change-buffer string and adds append-to-main-bibfile
  (setq-local
   header-line-format
   '(:eval
     (concat
      (ignore-errors
        (propertize " " 'display '(space :align-to 0) 'face 'fringe))
      (substitute-command-keys
       (biblio-join "   "
         "\\[biblio--selection-help]: Help"
         "\\[biblio--selection-insert],\\[biblio--selection-insert-quit]: Insert BibTex"
         "\\[biblio--selection-copy],\\[biblio--selection-copy-quit]: Copy BibTeX"
         "\\[biblio--selection-extended-action]: Extended action"
         "\\[biblio--selection-browse]: Open in browser"
         (format "\\[smf/biblio-selection-insert-end-of-bibfile]: Append to %s"
                 (abbreviate-file-name (car bibtex-completion-bibliography)))))))))


(defun smf/citar-find-article-id (citekey)
  "Find the id of CITEKEY-OR-CITEKEYS.

If the bibtex entry does not have a DOI or PubMedID, then use
betterbib to update your bibtex database."
  (interactive (list (citar-select-ref)))
  (let* ((entry (citar-get-entry citekey))
         (doi (citar-get-value 'doi entry))
         (pmid (citar-get-value 'pmid entry))
         (id (or doi pmid)))
    (unless id
      (message "No DOI or PMID found! Run 'betterbib' to find missing ids."))
    (when id
      id)))

(defun smf/xwidget-webkit-ddos-download (url script save-file &optional wait-for)
  "Use webkit's javascript to redirect from sites protected by DDOS services.

SCRIPT is raw javascript that should return an unprotected URL to
directly download to SAVE-FILE.

WAIT-FOR is the time to wait for the redirection to load. Default
wait time is 0.3 seconds.

If there is nothing being downloading, then you can try
increasing the WAIT-FOR time. If that still doesn't work, make
sure to check the url with `xwidget-webkit-browse-url' to see if
there is a captcha."
  (require 'xwidget)
  (let*
      ((bufname (generate-new-buffer-name "*xwidget-webkit-ddos-download*"))
       (buf (get-buffer-create bufname))
       (wait-for (or wait-for .3))
       xw)

    (with-current-buffer buf
      ;; The xwidget id is stored in a text property, so we need to have
      ;; at least character in this buffer.
      ;; Insert invisible url, good default for next `g' to browse url.
      (let ((start (point)))
        (insert url)
        (put-text-property start (+ start (length url)) 'invisible t)
        (setq xw (xwidget-insert
                  start 'webkit bufname
                  (xwidget-window-inside-pixel-width (selected-window))
                  (xwidget-window-inside-pixel-height (selected-window)))))
      (set-xwidget-query-on-exit-flag xw nil)
      (xwidget-put xw 'callback #'xwidget-webkit-callback)
      (xwidget-webkit-mode)
      (xwidget-webkit-goto-uri xw url)

      ;; needed for the ddos protection (written in javascript) to finish
      ;; redirecting
      (sleep-for wait-for)

      (xwidget-webkit-execute-script
       xw
       script
       (lambda (url)
         (url-copy-file url save-file)

         ;; must kill buffer in the callback, else
         ;; it won't have enough time to run the
         ;; callback
         (kill-buffer buf))))))

(defun smf/citar-insert-file-key (citekey)
  "Insert BibTeX field named file to CITE-KEY.

Uses citar to find the bibtex file and then inserts the string
pdf/`cite-key'.pdf for the value of the field.

This is a Frankenstein'd version of `citar--open-entry' but
modified to not pop up the file."
  (when (citar-get-entry citekey)
    (let (buf need-close)
      (catch 'break
        (dolist (bib-file (citar--bibliography-files))
          (setq buf (or (get-file-buffer bib-file)
                        (find-buffer-visiting bib-file))
                need-close (not buf)
                buf (find-file-noselect bib-file))

          (with-current-buffer buf
            (widen)
            (goto-char (point-min))
            (when (re-search-forward
                   (concat "^@\\(" parsebib--bibtex-identifier
                           "\\)[[:space:]]*[\\(\\{][[:space:]]*"
                           (regexp-quote citekey) "[[:space:]]*,") nil t)
              ;; if successful, break out of loop
              (throw 'break t))
            ;; if here then the file didn't have the entry so try the next file
            (setq buf nil)
            (when need-close (kill-buffer)))))

      (when buf
        (with-current-buffer buf
          (bibtex-end-of-entry)
          (beginning-of-line)
          (forward-line -1)
          (beginning-of-line)
          (bibtex-make-field "file" t)
          (backward-char)
          (insert (format "pdfs/%s.pdf" citekey))
          (save-buffer)
          (when need-close (kill-buffer)))))))

(defadvice! smf/citar-add-file-to-library (orig-fn &optional citekey)
  :around #'citar-add-file-to-library
  (cl-letf* ((old-copy-file (symbol-function 'copy-file))
             ;; prefer to trash source file once it's imported
             ((symbol-function 'copy-file)
              (lambda (file destfile confirm &rest _)
                (funcall old-copy-file file destfile confirm)
                ;; `move-file-to-trash' doesn't work for some reason
                (osx-trash-move-file-to-trash file))))
    (funcall orig-fn citekey)
    (smf/citar-insert-file-key citekey)))

(defun smf/citar-scihub-browse (citekey)
  "Open Sci-Hub in browser for CITEKEY-OR-CITEKEYS.

If the bibtex entry does not have a DOI or PubMedID, then use
betterbib to update your bibtex database."
  (interactive (list (citar-select-ref)))
  (when-let* ((id (smf/citar-find-article-id citekey))
              (url (format "https://sci-hub.se/%s" id)))
    (browse-url url)))

(defun smf/citar-scihub-import (citekey)
  "Search Sci-Hub for CITEKEY-OR-CITEKEYS.

If the bibtex entry does not have a DOI or PubMedID, then use
betterbib to update your bibtex database."
  (interactive (list (citar-select-ref)))
  (when-let* ((id (smf/citar-find-article-id citekey))
              (url (format "https://sci-hub.se/%s" id))
              (save-file (format "%s/%s.pdf"
                                 (car bibtex-completion-library-path)
                                 citekey)))
    (smf/xwidget-webkit-ddos-download url li2011boundary
                                      "document.querySelector('#pdf').src"
                                      save-file)))

(after! citar
  (map! :map citar-map
        "h" #'smf/citar-scihub-import
        "H" #'smf/citar-scihub-browse))
