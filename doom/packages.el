(package! exec-path-from-shell :disable t)
(package! banner-comment)
(package! bitwarden :recipe (:fetcher github :repo "seanfarley/emacs-bitwarden"))
(package! doom-modeline)
(package! emr)
(package! forge)
(package! goto-chg)
(package! ivy-rich)
(package! lispy)
(package! mu4e-alert)
(package! mu4e-patch :recipe (:fetcher github :repo "seanfarley/mu4e-patch"))
(package! org-starter)
(package! persistent-scratch)
(package!
  pdf-tools :recipe
  ;; https://github.com/melpa/melpa/blob/master/recipes/pdf-tools
  (:fetcher
   github
   :repo "dzop/pdf-tools"
   :branch "vector-graphics"
   :files ("lisp/*.el"
           "README"
           ("build" "Makefile")
           ("build" "server")
           (:exclude "lisp/tablist.el" "lisp/tablist-filter.el"))))
(package! resize-window)
(package! smart-jump)
(package! srefactor)
(package! unfill)
(package! webpaste)
(package! whole-line-or-region)
(package! yequake :recipe (:fetcher github :repo "alphapapa/yequake"))
