;;; ~/projects/dotfiles/doom/+ui.el -*- lexical-binding: t; -*-

;; optionally specifiy :weight 'light
(setq doom-font (font-spec :family "FuraCode Nerd Font" :size 14))

(after! hl-line
  ;; doom set this for a tiny speed boost but it breaks highlighting in mu4e
  ;; header-view
  (setq hl-line-sticky-flag t
        global-hl-line-sticky-flag t))

;; icomoon allows use of custom ranges so just upload there and make sure that
;; range matches the range below
(when (fboundp 'set-fontset-font)
  (add-hook! 'doom-init-ui-hook
    (set-fontset-font "fontset-default" '(#xe900 . #xe902) "smf-custom-icons")))

;; ;; example to test custom font; along with how to type raw unicode
;; (insert       (propertize "\ue900"
;;                   'face '(:family "smf-custom-icons")
;;                   'rear-nonsticky t
;;                   'display '(raise -0.1)
;;                   'font-lock-ignore t))

(defun smf/init-mac-ui ()
  (when (display-graphic-p)
    ;; automatically start in fullscreen
    (when (fboundp 'toggle-frame-fullscreen)
      (toggle-frame-fullscreen))

    (when (eq system-type 'darwin)
      (if (fboundp 'mac-auto-operator-composition-mode)
          (mac-auto-operator-composition-mode))

      ;; don't use the native fullscreen crap
      (setq-default ns-use-native-fullscreen nil))))

(add-hook 'doom-init-ui-hook #'smf/init-mac-ui)
