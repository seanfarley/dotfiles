;;; packages.el -*- lexical-binding: t; -*-

(package! benchmark-init)
(package! biblio)
(package! biblio-gscholar :recipe (:host github
                                   :repo "seanfarley/biblio-gscholar.el"))
(package! bitwarden :recipe (:host github :repo "seanfarley/emacs-bitwarden"))
(package! col-highlight :recipe '(:host github :repo "emacsmirror/col-highlight"))
(package! color-identifiers-mode)
(package! dot-mode)
(package! meson-mode)
(package! page-break-lines)
(package! ssh-config-mode)
(package! tramp-yadm :recipe (:local-repo "~/projects/tramp-yadm"
                              :build (:not compile)))
(package! unfill)
(package! whole-line-or-region)

;; TODO port to doom org +pretty
(package! org-fragtog)
(package! engrave-faces)
(package! org-modern)
(package! laas :recipe (:host github
                        :repo "tecosaur/LaTeX-auto-activating-snippets"))
