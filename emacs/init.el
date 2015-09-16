;;; init.el - Load Emacs

;;; Commentary:

;; This file uses org-babel to load my literate configuration.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'ob-tangle)

(org-babel-load-file (expand-file-name "README.org" (file-name-directory
                                                    (or (buffer-file-name) load-file-name))))
