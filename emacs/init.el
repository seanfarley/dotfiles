;;; init.el - Load Emacs

;;; Commentary:

;; This file uses org-babel to load my literate configuration.

(setq gc-cons-threshold 200000000)

(defun smf/join-dirs (prefix suffix)
  "Joins `prefix' and `suffix' into a directory"
  (file-name-as-directory (concat prefix suffix)))

(defconst smf/dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name))
  "Base path for customised Emacs configuration")

(setq package-user-dir
      (smf/join-dirs (smf/join-dirs smf/dotfiles-dir ".elpa") emacs-version))

(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa"        . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("elpy"         . "http://jorgenschaefer.github.io/packages/")
        ("org"          . "http://orgmode.org/elpa/")))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
  If NO-REFRESH is non-nil, the available package lists will not be
  re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'ob-tangle)

(org-babel-load-file (expand-file-name "README.org" (file-name-directory
                                                    (or (buffer-file-name) load-file-name))))
