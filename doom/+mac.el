;;; ~/projects/dotfiles/doom/+mac.el -*- lexical-binding: t; -*-

(when (and (eq system-type 'darwin) (display-graphic-p))

  ;; automatically start in fullscreen
  (when (fboundp 'toggle-frame-fullscreen)
    (toggle-frame-fullscreen))

  (if (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode))

  ;; don't use the native fullscreen crap
  (setq-default ns-use-native-fullscreen nil))
