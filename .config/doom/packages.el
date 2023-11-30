;;; packages.el -*- lexical-binding: t; -*-

(package! benchmark-init)
(package! biblio)
(package! biblio-gscholar :recipe (:host github
                                   :repo "seanfarley/biblio-gscholar.el"))
(package! bitwarden :recipe '(:host github :repo "seanfarley/emacs-bitwarden"))
(package! col-highlight :recipe '(:host github :repo "emacsmirror/col-highlight"))
(package! color-identifiers-mode)
(package! dot-mode)
(package! fancy-narrow)
(package! meson-mode)
(package! message-view-patch)
(package! mu4e-alert :disable t) ;; no longer needed after mu 1.10
(package! org-sidebar)
;; (package! ox-report) ;; if I get back into contracting
(package! package-lint)
(package! page-break-lines)
(package! prog-fill)
(package! ssh-config-mode)
(package! tramp-yadm :recipe (:local-repo "~/projects/tramp-yadm"
                              :build (:not compile)))
(package! unfill)
(package! webpaste)
(package! whole-line-or-region)
;; too annoying for technical writing which uses a lot of passive voice
(package! writegood-mode :disable t)

;; ;; I always forget this
;; (package! mypackage :recipe (:local-repo "packages/mypackage"
;;                              ;; :files (:defaults "extra")
;;                              :build (:not compile)))

;; TODO port to doom org +pretty
(package! org-fragtog)
(package! engrave-faces)
(package! org-modern)
(package! laas :recipe (:host github
                        :repo "tecosaur/LaTeX-auto-activating-snippets"))
