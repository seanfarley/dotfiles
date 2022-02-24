(package! ace-jump-zap)
(package! banner-comment)
(package! biblio)
(package! bitwarden :recipe '(:host github :repo "seanfarley/emacs-bitwarden"))
(package! col-highlight :recipe '(:host github :repo "emacsmirror/col-highlight"))
(package! dot-mode)
(package! elisp-slime-nav)
(package! embrace)
(package! emr)
(package! fancy-narrow)
(package! flycheck-package)
(package! focus)
(package! goto-chg)
(package! grip-mode)
(package! highlight-sexp :recipe (:host github
                                  :repo "daimrod/highlight-sexp"))
(package! ialign)
(package! lice)
(package! lsp-focus :recipe '(:host github :repo "emacs-lsp/lsp-focus"))
(package! lsp-origami)
(package! lsp-treemacs)
(package! message-view-patch)
(package! nerd-font :recipe '(:host github :repo "twlz0ne/nerd-fonts.el"))
(package! org-link-minor-mode :recipe '(:host github
                                        :repo "seanohalpin/org-link-minor-mode"))
(package! org-noter-pdftools)
(package! org-recent-headings)
(package! org-roam-bibtex)
(package! org-sidebar)
(package! org-web-tools)
;; (package! ox-report) ;; if I get back into contracting
(package! package-lint)
(package! page-break-lines)
;; ugh, get rid of this pipenv shit
(package! pipenv :disable t)
(package! prog-fill)
(package! resize-window)
(package! simple-httpd)
(package! sphinx-doc)
(package! srefactor)
(package! ssh-config-mode)
(package! sx)
(package! tree-sitter)
(package! tree-sitter-langs)
(package! twauctex :recipe '(:host github :repo "jeeger/twauctex"))
(package! unfill)
(package! webpaste)
(package! whole-line-or-region)
(package! which-key-posframe)
;; too annoying for technical writing which uses a lot of passive voice
(package! writegood-mode :disable t)
(package! ztree)
