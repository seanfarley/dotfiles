;;; packages.el -*- lexical-binding: t; -*-

(package! benchmark-init)
(package! biblio)
(package! biblio-gscholar :recipe (:host github
                                   :repo "seanfarley/biblio-gscholar.el"))
(package! bitwarden :recipe (:host github :repo "seanfarley/emacs-bitwarden"))
(package! col-highlight :recipe '(:host github :repo "emacsmirror/col-highlight"))
(package! color-identifiers-mode)
(package! meson-mode)
(package! ssh-config-mode)
(package! yadm :recipe (:local-repo "~/projects/yadm.el"))

(package! unfill)
(package! whole-line-or-region)

;; REVIEW https://github.com/doomemacs/doomemacs/pull/7727 this is temporary
;; until doom refactors and moves to a new window manager
(package! buffer-name-relative :pin "b1e878e97df0cd02348e1aed95738c0c3a32f148")

;; TODO port to doom org +pretty
(package! org-fragtog)
(package! engrave-faces)
(package! org-modern)
(package! laas :recipe (:host github
                        :repo "tecosaur/LaTeX-auto-activating-snippets"))
