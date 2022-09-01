(package! ace-jump-zap)
(package! benchmark-init)
(package! biblio)
(package! bitwarden :recipe '(:host github :repo "seanfarley/emacs-bitwarden"))
(package! col-highlight :recipe '(:host github :repo "emacsmirror/col-highlight"))
(package! dot-mode)
(package! elisp-slime-nav)
(package! fancy-narrow)
(package! flycheck-eldev)
(package! flycheck-package)
(package! focus)
(package! goto-chg)
(package! grip-mode)
(package! highlight-sexp :recipe (:host github
                                  :repo "daimrod/highlight-sexp"))
(package! lsp-focus :recipe '(:host github :repo "emacs-lsp/lsp-focus"))
(package! lsp-origami)
(package! lsp-treemacs)
(package! message-view-patch)
(package! nerd-font :recipe '(:host github :repo "twlz0ne/nerd-fonts.el"))
(package! outline-minor-faces)
(package! org-noter-pdftools)
(package! org-recent-headings)
(package! org-roam-bibtex)
(package! org-sidebar)
(package! org-web-tools)
;; (package! ox-report) ;; if I get back into contracting
(package! package-lint)
(package! page-break-lines)
(package! prog-fill)
(package! resize-window)
(package! ssh-config-mode)
(package! sx)
(package! tramp-yadm :recipe (:local-repo "~/projects/tramp-yadm"
                              :build (:not compile)))
(package! twauctex :recipe '(:host github :repo "jeeger/twauctex"))
(package! unfill)
(package! webpaste)
(package! whole-line-or-region)
(package! which-key-posframe)
;; too annoying for technical writing which uses a lot of passive voice
(package! writegood-mode :disable t)
(package! ztree)

;; I always forget this
;; (package! mypackage :recipe (:local-repo "packages/mypackage"
;;                              :build (:not compile)))

;; TODO port to doom org +pretty
(package! org-fragtog)
(package! engrave-faces)
(package! org-modern)
;; still not implemented in org-modern, see: https://github.com/minad/org-modern/issues/5
(package! valign :recipe (:host github :repo "casouri/valign"))
(package! laas :recipe (:host github
                        :repo "tecosaur/LaTeX-auto-activating-snippets"))
