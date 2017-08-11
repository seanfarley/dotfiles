;;; org-logbook.el --- Display logbook closing notes in an agenda view

;; Copyright (C) 2017  Sean Farley <sean@farley.io>

;; Author: Sean Farley <sean@farley.io>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'org-agenda)

(defun smf/org-element-notes (el)
  "Return a list of notes for a given element EL."
  (car ;; remove the empty surrounding list (or maybe I'm missing something
       ;; that could be in this list?)
   (org-element-map el 'drawer
     (lambda (p)
       (org-element-map p 'paragraph
         (lambda (pd)
           (org-no-properties (org-element-interpret-data pd))))))))

(defun smf/org-marker-notes (marker)
  "Return a list of notes at a given mark MARKER."
  ;; (if (and org-return-follows-link
  ;;          (not (org-get-at-bol 'org-marker))
  ;;          (org-in-regexp org-bracket-link-regexp))
  ;;     (org-open-link-from-string (match-string 0))
    (let* ((buffer (marker-buffer marker))
           (pos (marker-position marker)))

      (with-current-buffer buffer
        (save-excursion
          (goto-char pos)
          (unless (org-at-heading-p)
            (org-back-to-heading 'invisible-ok)) ;; holy hell it took forever to
                                                 ;; find this method
          (let* ((el (org-element-at-point))
                 (beg (org-element-property :begin el))
                 (end (org-element-property :end el))
                 (el-text (buffer-substring beg end)))
            (with-temp-buffer
              (insert el-text)
              (smf/org-element-notes (org-element-parse-buffer))))))))

;; code copied from `org-agenda' but replaced "entry-text" with "logbook" and
;; remove max lines logic

(defvar org-agenda-logbook-mode nil)

(defun org-agenda-logbook-show-here ()
  "Add some text from the logbook as context to the current line."
  (let (m txt o)
    (setq m (org-get-at-bol 'org-hd-marker))
    (unless (marker-buffer m)
      (error "No marker points to a logbook here"))
    (setq txt (apply #'concatenate 'string (smf/org-marker-notes m)))
    (setq txt (concat "\n" txt))
    ;; (when (string-match "\\S-" txt)
    (when (string-match "CLOSING" txt)
      (setq o (make-overlay (point-at-bol) (point-at-eol)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'org-overlay-type 'agenda-logbook-content)
      (overlay-put o 'after-string txt))))

(defun org-agenda-logbook-show ()
  "Add logbook context for all agenda lines."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line 1)
    (while (not (bobp))
      (when (org-get-at-bol 'org-hd-marker)
        (org-agenda-logbook-show-here))
      (beginning-of-line 0))))

(defun org-agenda-logbook-hide ()
  "Remove any shown logbook context."
  (delq nil
        (mapcar (lambda (o)
                  (if (eq (overlay-get o 'org-overlay-type)
                          'agenda-logbook-content)
                      (progn (delete-overlay o) t)))
                (overlays-in (point-min) (point-max)))))

(defun org-agenda-logbook-mode (&optional arg)
  "Toggle logbook mode in an agenda buffer.

  ARG is a prefix for number of lines to show."
  (interactive "P")
  (if (or org-agenda-tag-filter
          org-agenda-category-filter
          org-agenda-regexp-filter
          org-agenda-top-headline-filter)
      (user-error "Can't show logbook in filtered views")
    (setq org-agenda-logbook-mode (or (integerp arg)
                                      (not org-agenda-logbook-mode)))
    (org-agenda-logbook-hide)
    (and org-agenda-logbook-mode (org-agenda-logbook-show))
    (org-agenda-set-mode-name)
    (message "Logbook mode is %s"
             (if org-agenda-logbook-mode "on" "off"))))

(provide 'org-logbook)

;;; org-logbook.el ends here
