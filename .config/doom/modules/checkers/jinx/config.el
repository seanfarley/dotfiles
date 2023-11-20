;;; checkers/jinx/config.el -*- lexical-binding: t; -*-

(use-package! jinx
  :init (global-jinx-mode)
  :custom
  (jinx-include-modes '(text-mode prog-mode))

  ;; (jinx-include-faces
  ;;  '((prog-mode font-lock-doc-face)
  ;;    (conf-mode font-lock-comment-face)))

  :bind
  (("M-$" . jinx-correct)))
