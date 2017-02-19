;;; init.el - Load Emacs

;;; Commentary:

;; This file uses org-babel to load my literate configuration.

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

;; Set the initial state to non-refreshed. This can also be set back
;; to nil if we want to run a refresh on the next install.
(defvar smf/refreshed-package-list nil)

(defun smf/ensure-refreshed ()
  "Ensure the package list has been refreshed this startup."
  (unless smf/refreshed-package-list
    (package-refresh-contents)
    (setq smf/refreshed-package-list t)))

(defun smf/package-ensure-installed (package)
  "Install a missing PACKAGE if it isn't already."
  (unless (package-installed-p package)
    (package-install package)))

;; Now that we have some helpers defined, we wrap package-install to make sure
;; that the first install of each session will refresh the package list.
(advice-add 'package-install
            :before
            (lambda (&rest args)
              (smf/ensure-refreshed)))

(smf/package-ensure-installed 'use-package)
(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))

(require-package 'benchmark-init)
(benchmark-init/activate)

(package-install 'org-plus-contrib)

(require 'org)
(require 'ob-tangle)

(org-babel-load-file (expand-file-name "README.org" (file-name-directory
                                                    (or (buffer-file-name) load-file-name))))
