;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu
  :recipe (:files (:defaults "extensions/*.el"))
  :pin "f658ae627508ba8104b640b4242a2a99716c58ea")
(when (modulep! +icons)
  (package! kind-icon :pin "f57ba2241c3097236f8a2f7df016ea6683526502"))
(package! orderless)
(package! corfu-doc
  :recipe (:host github :repo "galeo/corfu-doc")
  :pin "0e6125cd042506a048feb7b6446a5653eccfcff5")
(package! cape :pin "1558f8b1dea46fcd9c8c04739656ed0b8027c38a")
(package! yasnippet-capf
  :recipe (:host github :repo "elken/yasnippet-capf")
  :pin "a0a6b1c2bb6decdad5cf9b74202f0042f494a6ab")
(package! package-capf
  :recipe (:host github :repo "elken/package-capf")
  :pin "005b611a32fe6e49f9e74693d3fe99ba129dfff7")

(when (modulep! :tools debugger +lsp)
  ;; dap-ui-reply-company hardcodes company functions so we need to load company
  ;; and then pass dap-ui-reply-company through cape
  (package! company :pin "9c12b02620ed8a7ae5369fc90217f1c730e48fa6"))

(when (modulep! :os tty)
  (package! popon
    :recipe (:type git :repo "https://codeberg.org/akib/emacs-popon")
    :pin "bf8174cb7e6e8fe0fe91afe6b01b6562c4dc39da")
  (package! corfu-terminal
    :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
    :pin "501548c3d51f926c687e8cd838c5865ec45d03cc")
  (package! corfu-doc-terminal
    :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
    :pin "da5042082d2296946972599f6d95bbbffaf63fba"))
