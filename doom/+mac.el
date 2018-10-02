;;; ~/projects/dotfiles/doom/+mac.el -*- lexical-binding: t; -*-

(when (and (eq system-type 'darwin) (display-graphic-p))

  ;; automatically start in fullscreen
  (when (fboundp 'toggle-frame-fullscreen)
    (toggle-frame-fullscreen))

  ;; set mac opt as meta
  (setq-default ns-alternate-modifier 'meta)
  (setq-default mac-option-modifier 'meta)
  (setq-default ns-right-alternate-modifier nil)

  ;; set command as super
  (setq-default ns-command-modifier 'super)
  (setq-default mac-command-modifier 'super)

  ;; set fn as hyper
  (setq-default ns-function-modifier 'hyper)
  (setq-default mac-function-modifier 'hyper)

  ;; don't use the native fullscreen crap
  (setq-default ns-use-native-fullscreen nil))
