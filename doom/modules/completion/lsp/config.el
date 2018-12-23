;;; completion/lsp/config.el -*- lexical-binding: t; -*-

(def-package! lsp-mode
  :commands (lsp-mode lsp-define-stdio-client)
  :config
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (set-lookup-handlers! 'lsp-ui-mode
                        :definition #'lsp-ui-peek-find-definitions
                        :references #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t))

(def-package! lsp-typescript
  :when (featurep! +javascript)
  :hook ((js2-mode typescript-mode) . lsp-typescript-enable))

(def-package! company-lsp
  :after lsp-mode
  :config
  (set-company-backend! 'lsp-mode 'company-lsp)
  (setq company-lsp-enable-recompletion t))

(def-package! lsp-go
  :when (featurep! +go)
  :hook (go-mode . lsp-go-enable))

(def-package! lsp-css
  :when (featurep! +css)
  :hook ((css-mode less-mode scss-mode) . lsp-css-enable))

(def-package! lsp-rust
  :when (featurep! +rust)
  :hook (rust-mode . lsp-rust-enable)
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

(def-package! cquery
  :when (featurep! +cpp)
  ;; :hook ((c-mode c++-mode objc-mode) . +setup-cquery)
  :hook ((c-mode c++-mode objc-mode) . lsp)
  :init
  (require 'cquery)
  (setq cquery-extra-init-params '(:index (:comments 2)
                                          :cacheFormat "msgpack"
                                          :completion (:detailedLabel t))
        cquery-sem-highlight-method 'overlay) ;; set to 'font-lock if highlighting slowly
  (defun +setup-cquery ()
    (setq-local company-transformers nil)
    (setq-local company-lsp-cache-candidates nil)
    (condition-case nil
        (lsp-cquery-enable)
      (user-error nil))))


(def-package! lua-mode
  :when (featurep! +lua)
  ;; :hook ((lsp-mode) . lsp)
  :init
  (require 'lua-mode)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "lua-lsp")
                    :major-modes '(lua-mode)
                    :server-id 'lua)))

(def-package! lsp-ocaml
  :when (featurep! +ocaml)
  :hook ((tuareg-mode reason-mode) . lsp-ocaml-enable))

(def-package! lsp-intellij
  :when (featurep! +java)
  :hook (java-mode . lsp-intellij-enable))


(when (featurep! +python)
  (after! python

    ;; dir containing Microsoft.Python.LanguageServer.dll
    (setq ms-pyls-dir (expand-file-name
                       "~/sandbox/python-language-server/output/bin/Release/"))
  
    ;; this gets called when we do lsp-describe-thing-at-point in lsp-methods.el
    ;; we remove all of the "&nbsp;" entities that MS PYLS adds this is mostly
    ;; harmless for other language servers
    (defun render-markup-content (kind content)
      (message kind)
      (replace-regexp-in-string "\\\\_" "_"
                                (replace-regexp-in-string "&nbsp;" " " content)))
    (setq lsp-render-markdown-markup-content #'render-markup-content)

    ;; monkey patch lsp-ui functions to deal with MS PYLS markdown damage; e.g.
    ;; \_ -> _

    (defun smf/lsp-ui-sideline--extract-info (orig contents)
      "Extract the line to print from CONTENTS.
CONTENTS can be differents type of values:
MarkedString | MarkedString[] | MarkupContent (as defined in the LSP).
We prioritize string with a language (which is probably a type or a
function signature)."
      (when contents
        (cond
         ((stringp contents) contents)
         ((sequencep contents) ;; MarkedString[]
          (seq-find (lambda (it) (and (hash-table-p it)
                                      (lsp-ui-sideline--get-renderer (gethash "language" it))))
                    contents))
         ;; cpbotha: with numpy functions, e.g. np.array for example,
         ;; kind=markdown and docs are in markdown, but in default
         ;; lsp-ui-20181031 this is rendered as plaintext see
         ;; https://microsoft.github.io/language-server-protocol/specification#markupcontent

         ;; not only that, MS PyLS turns all spaces into &nbsp; instances,
         ;; which we remove here this single additional cond clause fixes all
         ;; of this for hover

         ;; as if that was not enough: import pandas as pd - pandas is returned
         ;; with kind plaintext but contents markdown, whereas pd is returned
         ;; with kind markdown. fortunately, handling plaintext with the
         ;; markdown viewer still looks good, so here we are.
         ((member (gethash "kind" contents) '("markdown" "plaintext"))
          (replace-regexp-in-string "\\\\_" "_"
                                    (replace-regexp-in-string "&nbsp;" " "
                                                              (lsp-ui-doc--extract-marked-string contents))))

         ((gethash "kind" contents) (gethash "value" contents)) ;; MarkupContent
         ((gethash "language" contents) ;; MarkedString
          (and (lsp-ui-sideline--get-renderer (gethash "language" contents))
               (gethash "value" contents))))))

    (advice-add 'lsp-ui-sideline--extract-info :around
                #'smf/lsp-ui-sideline--extract-info)

    (defun smf/lsp-ui-doc--extract (orig contents)
      "Extract the documentation from CONTENTS.
CONTENTS can be differents type of values:
MarkedString | MarkedString[] | MarkupContent (as defined in the LSP).
We don't extract the string that `lps-line' is already displaying."
      (when contents
        (cond
         ((stringp contents) contents)
         ((sequencep contents) ;; MarkedString[]
          (mapconcat 'lsp-ui-doc--extract-marked-string
                     (lsp-ui-doc--filter-marked-string contents)
                     "\n\n"
                     ;; (propertize "\n\n" 'face '(:height 0.4))
                     ))

         ;; cpbotha: with numpy functions, e.g. np.array for example,
         ;; kind=markdown and docs are in markdown, but in default
         ;; lsp-ui-20181031 this is rendered as plaintext see
         ;; https://microsoft.github.io/language-server-protocol/specification#markupcontent

         ;; not only that, MS PyLS turns all spaces into &nbsp; instances,
         ;; which we remove here this single additional cond clause fixes all
         ;; of this for hover

         ;; as if that was not enough: import pandas as pd - pandas is returned
         ;; with kind plaintext but contents markdown, whereas pd is returned
         ;; with kind markdown. fortunately, handling plaintext with the
         ;; markdown viewer still looks good, so here we are.
         ((member (gethash "kind" contents) '("markdown" "plaintext"))
          (replace-regexp-in-string "\\\\_" "_"
                                    (replace-regexp-in-string "&nbsp;" " "
                                                              (lsp-ui-doc--extract-marked-string contents))))

         ((gethash "kind" contents) (gethash "value" contents)) ;; MarkupContent
         ((gethash "language" contents) ;; MarkedString
          (lsp-ui-doc--extract-marked-string contents)))))

    (advice-add 'lsp-ui-doc--extract :around
                #'smf/lsp-ui-doc--extract)

    ;; it's crucial that we send the correct Python version to MS PYLS, else it
    ;; returns no docs in many cases furthermore, we send the current Python's
    ;; (can be virtualenv) sys.path as searchPaths
    (defun get-python-ver-and-syspath (workspace-root)
      "return list with pyver-string and json-encoded list of python search paths."
      (let ((python (executable-find python-shell-interpreter))
            (ver "import sys; print(\"%s.%s\" % (sys.version_info[0], sys.version_info[1]));")
            (sp (concat "import json; sys.path.insert(0, '" workspace-root "'); print(json.dumps(sys.path))")))
        (with-temp-buffer
          (call-process python nil t nil "-c" (concat ver sp))
          (subseq (split-string (buffer-string) "\n") 0 2))))

    ;; I based most of this on the vs.code implementation:
    ;; https://github.com/Microsoft/vscode-python/blob/master/src/client/activation/languageServer/languageServer.ts#L219
    ;; (it still took quite a while to get right, but here we are!)
    (defun ms-pyls-extra-init-params (workspace)
      (destructuring-bind (pyver pysyspath) (get-python-ver-and-syspath (lsp--workspace-root workspace))
        `(:interpreter (
                        :properties (
                                     :InterpreterPath ,(executable-find python-shell-interpreter)
                                     :DatabasePath ,ms-pyls-dir
                                     :Version ,pyver))
                       ;; preferredFormat "markdown" or "plaintext"
                       ;; experiment to find what works best -- over here mostly plaintext
                       :displayOptions (
                                        :preferredFormat "plaintext"
                                        :trimDocumentationLines :json-false
                                        :maxDocumentationLineLength 0
                                        :trimDocumentationText :json-false
                                        :maxDocumentationTextLength 0)
                       :searchPaths ,(json-read-from-string pysyspath))))

    (lsp-define-stdio-client lsp-python "python"
                             #'projectile-project-root
                             `("dotnet" ,(concat ms-pyls-dir
                                                 "Microsoft.Python.LanguageServer.dll"))
                             :extra-init-params #'ms-pyls-extra-init-params)

    (add-hook! python-mode #'lsp-python-enable)))

(when (featurep! +sh)
  (after! sh-script
    (lsp-define-stdio-client lsp-sh
                            "sh"
                            #'projectile-project-root
                            '("bash-language-server" "start"))
    (add-hook 'sh-mode-hook #'lsp-sh-enable)))

