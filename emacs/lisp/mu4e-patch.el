;;; mu4e-view-treat-patch.el --- colorize patch-like emails in mu4e

;; Copyright (C) 2011 Frank Terbeck.
;; Copyright (C) 2018 Sean Farley.

;;; Commentary:

;; This is adapted from Frank Terbeck's gnus-article-treat-patch.el but has
;; been adapted to work with mu4e.

;;; Code:

(require 'diff-mode)

;; Color handling and faces
(defun mu4e~patch-color-line (use-face)
  "Set text overlay to `USE-FACE' for the current line."
  (overlay-put (make-overlay (point-at-bol) (point-at-eol)) 'face use-face))

;; Faces
(defgroup mu4e-patch-faces nil
  "Type faces (fonts) used in mu4e-patch."
  :group 'mu4e
  :group 'faces)

(defface mu4e-patch-three-dashes
  '((t :foreground "brightblue"))
  "Face for the three dashes in a diff header."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-scissors
  '((t :foreground "brown"))
  "Face for the scissors 8< lines."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-diff-index
  '((t :foreground "brightmagenta"))
  "Face for the diff index."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-diff-hunk
  '((t :foreground "brightblue"))
  "Face for the diff hunk."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-diff-equals
  '((t :foreground "brightmagenta"))
  "Face for the line of equal signs that some diffs have."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-commit-message
  '((t :foreground "white"))
  "Face for the commit message."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-diff-stat-file
  '((t :foreground "yellow"))
  "Face for the file stats."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-diff-stat-bar
  '((t :foreground "magenta"))
  "Face for the stat bar separator."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-diff-stat-num
  '((t :foreground "white"))
  "Face for the stat number column."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-misc
  '((t :foreground "magenta"))
  "Face for the \"misc line\" part of the diff."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-commit-comment
  '((t :inherit default))
  "Face for the commit part of the diff (between two ---'s after
the commit message)."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-diff-header
  '((t :inherit diff-header))
  "Face for the diff hunk headers."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-diff-added
  '((t :inherit diff-added))
  "Face for the diff lines that are added."
  :group 'mu4e-patch-faces)

(defface mu4e-patch-diff-removed
  '((t :inherit diff-removed))
  "Face for the diff lines that are removed."
  :group 'mu4e-patch-faces)

;; Pseudo-headers
(defvar mu4e~patch-pseudo-headers
  '(("^Acked-by: "      'mu4e-header-key-face 'mu4e-header-value-face)
    ("^C\\(c\\|C\\): "  'mu4e-header-key-face 'mu4e-header-value-face)
    ("^From: "          'mu4e-header-key-face 'mu4e-header-value-face)
    ("^Link: "          'mu4e-header-key-face 'mu4e-header-value-face)
    ("^Reported-by: "   'mu4e-header-key-face 'mu4e-header-value-face)
    ("^Reviewed-by: "   'mu4e-header-key-face 'mu4e-header-value-face)
    ("^Signed-off-by: " 'mu4e-header-key-face 'mu4e-header-value-face)
    ("^Subject: "       'mu4e-header-key-face 'mu4e-header-value-face)
    ("^Suggested-by: "  'mu4e-header-key-face 'mu4e-header-value-face))
  "List of lists of regular expressions (with two face names)
which are used to determine the highlighting of pseudo headers in
the commit message (such as \"Signed-off-by:\").

The first face if used to highlight the header's name; the second
highlights the header's value.")

(defun mu4e~pseduo-header-get (line)
  "Check if `LINE' is a pseudo header.

If so return its entry in `mu4e~patch-pseudo-headers'."
  (catch 'done
    (dolist (entry mu4e~patch-pseudo-headers)
      (let ((regex (car entry)))
        (if (string-match regex line)
            (throw 'done entry))))
    (throw 'done '())))

(defun mu4e~patch-pseudo-header-p (line)
  "Return t if `LINE' is a pseudo-header; nil otherwise.

`mu4e~patch-pseudo-headers' is used to determine what a
pseudo-header is."
  (if (eq (mu4e~pseduo-header-get line) '()) nil t))

(defun mu4e~patch-pseudo-header-color (line)
  "Colorize a pseudo-header `LINE'."
  (let ((data (mu4e~pseduo-header-get line)))
    (if (eq data '())
        nil
      (let* ((s (point-at-bol))
             (e (point-at-eol))
             (colon (re-search-forward ":"))
             (value (+ colon 1)))
        (overlay-put (make-overlay s colon) 'face (nth 1 data))
        (overlay-put (make-overlay value e) 'face (nth 2 data))))))

;; diff-stat
(defun mu4e~patch-diff-stat-color (line)
  "Colorize a diff-stat `LINE'."
  (let ((s (point-at-bol))
        (e (point-at-eol))
        (bar (- (re-search-forward "|") 1))
        (num (- (re-search-forward "[0-9]") 1))
        (pm (- (re-search-forward "\\([+-]\\|$\\)") 1)))

    (overlay-put (make-overlay s (- bar 1)) 'face 'mu4e-patch-diff-stat-file)
    (overlay-put (make-overlay bar (+ bar 1)) 'face 'mu4e-patch-diff-stat-bar)
    (overlay-put (make-overlay num pm) 'face 'mu4e-patch-diff-stat-num)

    (goto-char pm)
    (let* ((plus (looking-at "\\+"))
           (regex (if plus "-+" "\\++"))
           (brk (if plus
                    (re-search-forward "-" e t)
                  (re-search-forward "\\+" e t)))
           (first-face (if plus 'mu4e-patch-diff-added 'mu4e-patch-diff-removed))
           (second-face (if plus 'mu4e-patch-diff-removed 'mu4e-patch-diff-added)))

      (if (eq brk nil)
          (overlay-put (make-overlay pm e) 'face first-face)
        (progn
          (setq brk (- brk 1))
          (overlay-put (make-overlay pm brk) 'face first-face)
          (overlay-put (make-overlay brk e) 'face second-face))))))

(defun ft/gnus-diff-stat-summary-colour (line)
  "Colorize a diff-stat summary `LINE'."
  (let* ((e (point-at-eol))
         (plus (- (re-search-forward "(\\+)" e t) 2))
         (minus (- (re-search-forward "(-)" e t) 2)))
    (overlay-put (make-overlay plus (+ plus 1)) 'face 'mu4e-patch-diff-added)
    (overlay-put (make-overlay minus (+ minus 1)) 'face 'mu4e-patch-diff-removed)))

(defun ft/gnus-diff-stat-line-p (line)
  "Return t if `LINE' is a diff-stat line; nil otherwise."
  (string-match "^ *[^ ]+[^|]+| +[0-9]+\\( *\\| +[+-]+\\)$" line))

(defun ft/gnus-diff-stat-summary-p (line)
  "Return t if `LINE' is a diff-stat summary-line; nil otherwise."
  (string-match "^ *[0-9]+ file\\(s\\|\\) changed,.*insertion.*deletion" line))

;; unified-diffs
(defun ft/gnus-diff-header-p (line)
  "Return t if `LINE' is a diff-header; nil otherwise."
  (cond
   ((string-match "^\\(\\+\\+\\+\\|---\\) " line) t)
   ((string-match "^diff -" line) t)
   (t nil)))

(defun ft/gnus-index-line-p (line)
  "Return t if `LINE' is an index-line; nil otherwise."
  (cond
   ((string-match "^Index: " line) t)
   ((string-match "^index [0-9a-f]+\\.\\.[0-9a-f]+" line) t)
   (t nil)))

(defun ft/gnus-hunk-line-p (line)
  "Return t if `LINE' is a hunk-line; nil otherwise."
  (string-match "^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@" line))

(defun ft/gnus-atp-misc-diff-p (line)
  "Return t if `LINE' is a \"misc line\"; nil otherwise.

This is tested with respect to patch treatment."
  (let ((patterns '("^new file"
                    "^RCS file:"
                    "^retrieving revision ")))
    (catch 'done
      (dolist (regex patterns)
        (if (string-match regex line)
            (throw 'done t)))
      (throw 'done nil))))

(defun ft/gnus-atp-looks-like-diff (line)
  "Return t if `LINE' is a unified diff; nil otherwise.

This will test anything that even looks remotely like a line from
a unified diff"
  (or (ft/gnus-index-line-p line)
      (ft/gnus-diff-header-p line)
      (ft/gnus-hunk-line-p line)))

;; miscellaneous line handlers
(defun ft/gnus-scissors-line-p (line)
  "Return t if `LINE' is a scissors-line; nil otherwise."
  (cond
   ((string-match "^\\( *--* *\\(8<\\|>8\\)\\)+ *-* *$" line) t)
   (t nil)))

;; Patch mail detection
(defvar ft/gnus-article-patch-conditions nil
  "List of conditions that will enable patch treatment.

String values will be matched as regular expressions within the
currently processed part. Non-string value are supposed to be
code fragments, which determine whether or not to do treatment:
The code needs to return t if treatment is wanted.")

(defun ft/gnus-part-want-patch-treatment ()
  "Return t if patch treatment is wanted.

Run through `ft/gnus-article-patch-conditions' to determine
whether patch treatment is wanted or not."
  (catch 'done
    (dolist (entry ft/gnus-article-patch-conditions)
      (cond
       ((stringp entry)
        (if (re-search-forward entry nil t)
            (throw 'done t)))
       (t
        (if (eval entry)
            (throw 'done t)))))
      (throw 'done nil)))


;; The actual article treatment code
(defun ft/gnus-article-treat-patch-state-machine ()
  "Colorize a part of the mu4e-view buffer.

Implement the state machine which colorizes a part of an article
if it looks patch-like.

The state machine works like this:

  0a. The machinery starts at the first line of the article's body. Not
      the header lines. We don't care about header lines at all.

  0b. The whole thing works line by line. It doesn't do any forward or
      backward looks.

  1. Initially, we assume, that what we'll see first is part of the
     patch's commit-message. Hence this first initial state is
     \"commit-message\". There are several ways out of this state:

       a) a scissors line is found (see 2.)
       b) a pseudo-header line is found (see 3.)
       c) a three-dashes line is found (see 4.)
       d) something that looks like the start of a unified diff is
          found (see 7.)

  2. A scissors line is something that looks like a pair of scissors running
     through a piece of paper. Like this:

      ------ 8< ----- 8< ------

     or this:

      ------------>8-----------

     The function `ft/gnus-scissors-line-p' decides whether a line is a
     scissors line or not. After a scissors line was treated, the machine
     will switch back to the \"commit-mesage\" state.

  3. This is very similar to a scissors line. It'll just return to the old
     state after its being done. The `mu4e~patch-pseudo-header-p' function
     decides if a line is a pseudo header. The line will be appropriately
     coloured.

  4. A three-dashes line is a line that looks like this: \"---\". It's the
     definite end of the \"commit-message\" state. The three dashes line is
     coloured and the state switches to \"commit-comment\". (See 5.)

  5. Nothing in \"commit-comment\" will appear in the generated commit (this
     is git-am specific semantics, but it's useful, so...). It may contain
     things like random comments or - promimently - a diff stat. (See 6.)

  6. A diff stat provides statistics about how much changed in a given commit
     by files and by whole commit (in a summary line). Two functions
     `ft/gnus-diff-stat-line-p' and `ft/gnus-diff-stat-summary-p' decide if a
     line belongs to a diff stat. It's coloured appropriately and the state
     switches back to \"commit-comment\".

  7. There is a function `ft/gnus-unified-diff-line-p' which will cause the
     state to switch to \"unified-diff\" state from either \"commit-message\"
     or \"commit-comment\". In this mode there can be a set of lines types:

       a) diff-header lines (`ft/gnus-diff-header-p')
       b) index lines (`ft/gnus-index-line-p')
       c) hunk lines (`ft/gnus-hunk-line-p')
       d) equals line (\"^==*$\")
       e) context lines (\"^ \")
       f) add lines (\"^\\+\")
       g) remove lines (\"^-\")
       h) empty lines (\"^$\")

     This state runs until the end of the part."
  (catch 'ft/gnus-atp-done
    (let ((state 'commit-message)
          line do-not-move)

      (while t
        ;; Put the current line into an easy-to-handle string variable.
        (setq line
              (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (setq do-not-move nil)

        ;; Switched state machine. The "real" states are `commit-message',
        ;; `commit-comment' and `unified-diff'. The other "states" are only
        ;; single-line colourisations that return to their respective parent-
        ;; state. Each state may (throw 'ft/gnus-atp-done) to leave the state-
        ;; machine immediately.
        (setq state
              (cond

               ((eq state 'commit-message)
                (cond
                 ((ft/gnus-scissors-line-p line)
                  (mu4e~patch-color-line 'mu4e-patch-scissors)
                  'commit-message)
                 ((mu4e~patch-pseudo-header-p line)
                  (mu4e~patch-pseudo-header-color line)
                  'commit-message)
                 ((string= line "---")
                  (mu4e~patch-color-line 'mu4e-patch-three-dashes)
                  'commit-comment)
                 ((ft/gnus-atp-looks-like-diff line)
                  (setq do-not-move t)
                  'unified-diff)
                 (t
                  (mu4e~patch-color-line 'mu4e-patch-commit-message)
                  'commit-message)))

               ((eq state 'commit-comment)
                (cond
                 ((ft/gnus-diff-stat-line-p line)
                  (mu4e~patch-diff-stat-color line)
                  'commit-comment)
                 ((ft/gnus-diff-stat-summary-p line)
                  (ft/gnus-diff-stat-summary-colour line)
                  'commit-comment)
                 ((ft/gnus-atp-looks-like-diff line)
                  (setq do-not-move t)
                  'unified-diff)
                 (t
                  (mu4e~patch-color-line 'mu4e-patch-commit-comment)
                  'commit-comment)))

               ((eq state 'unified-diff)
                (cond
                 ((ft/gnus-diff-header-p line)
                  (mu4e~patch-color-line 'mu4e-patch-diff-header)
                  'unified-diff)
                 ((ft/gnus-index-line-p line)
                  (mu4e~patch-color-line 'mu4e-patch-diff-index)
                  'unified-diff)
                 ((ft/gnus-hunk-line-p line)
                  (mu4e~patch-color-line 'mu4e-patch-diff-hunk)
                  'unified-diff)
                 ((string-match "^==*$" line)
                  (mu4e~patch-color-line 'mu4e-patch-diff-equals)
                  'unified-diff)
                 ((string-match "^$" line)
                  'unified-diff)
                 ((string-match "^ " line)
                  (mu4e~patch-color-line 'ft/gnus-diff-context)
                  'unified-diff)
                 ((ft/gnus-atp-misc-diff-p line)
                  (mu4e~patch-color-line 'mu4e-patch-misc)
                  'unified-diff)
                 ((string-match "^\\+" line)
                  (mu4e~patch-color-line 'mu4e-patch-diff-added)
                  'unified-diff)
                 ((string-match "^-" line)
                  (mu4e~patch-color-line 'mu4e-patch-diff-removed)
                  'unified-diff)
                 (t 'unified-diff)))))

        (if (not do-not-move)
            (if (> (forward-line) 0)
                (throw 'ft/gnus-atp-done t)))))))

(defun ft/gnus-article-treat-patch ()
  "Highlight mail parts, that look like patches.

Well, usually they *are* patches - or possibly, when you take
git's format-patch output, entire commit exports - including
comments). This treatment assumes the use of unified diffs. Here
is how it works:

The most fancy type of patch mails look like this:

  From: ...
  Subject: ...
  Other-Headers: ...

  Body text, which can be reflecting the commit message but may
  optionally be followed by a so called scissors line, which
  looks like this (in case of a scissors line, the text above is
  not part of the commit message):

  -------8<----------

  If there really was a scissors line, then it's usually
  followed by repeated mail-headers. Which do not *have* to
  be the same as the one from the sender.

  From: ...
  Subject: ...

  More text. Usually part of the commit message. Likely
  multiline.  What follows may be an optional diffstat. If
  there is one, it's usually preceded by a line that contains
  only three dashes and nothing more. Before the diffstat,
  however, there may be a set of pseudo headers again, like
  these:

  Acked-by: Mike Dev <md@other.tld>
  Signed-off-by: Joe D. User <jdu@example.com>

  ---
  ChangeLog                    |    5 ++++-
  1 file changed, 4 insertions(+), 1 deletions(-)

  Now, there is again room for optional text, which is not
  part of the actual commit message. May be multiline. Actually,
  anything between the three-dashes line and the diff content
  is ignored as far as the commit message goes.

  Now for the actual diff part.  I want this to work for as
  many unified diff formats as possible.  What comes to mind
  is the format used by git and the format used by cvs and
  quilt.

  CVS style looks like this:

  Index: foo/bar.c
  ============================================================
  --- boo.orig/foo/bar.c       2010-02-24 ....
  +++ boo/foo/bar.c            2010-02-28 ....
  @@ -1823,7 +1823,7 @@
  <hunk>

  There may be multiple hunks. Each file gets an \"Index:\" and
  equals line.  Now the git format looks like this:

  diff --git a/ChangeLog b/ChangeLog
  index 6ffbc8c..36e5c17 100644
  --- a/ChangeLog
  +++ b/ChangeLog
  @@ -3,6 +3,9 @@
  <hunk>

  Again, there may be multiple hunks.

  When all hunks and all files are done, there may be additional
  text below the actual text.

And that's it.

You may define the look of several things: pseudo headers, scissor
lines, three-dashes-line, equals lines, diffstat lines, diffstat
summary. Then there is added lines, removed lines, context lines,
diff-header lines and diff-file-header lines, for which we are
borrowing the highlighting faces for from `diff-mode'."
  (if (ft/gnus-part-want-patch-treatment)
      (save-excursion
        (progn
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            (ft/gnus-article-treat-patch-state-machine))))))

(provide 'mu4e-patch)
;;; mu4e-patch ends here
