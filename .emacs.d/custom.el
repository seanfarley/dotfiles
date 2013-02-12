; -------------------------
; Customizations generated but tweaked
; -------------------------

; Customized variable generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ns-right-command-modifier (quote meta))
 '(safe-local-variable-values (quote ((c-offsets-alist (inexpr-class . +) (inexpr-statement . +) (lambda-intro-cont . +) (inlambda . c-lineup-inexpr-block) (template-args-cont c-lineup-template-args +) (incomposition . +) (inmodule . +) (innamespace . +) (inextern-lang . +) (composition-close . 0) (module-close . 0) (namespace-close . 0) (extern-lang-close . 0) (composition-open . 0) (module-open . 0) (namespace-open . 0) (extern-lang-open . 0) (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +) (objc-method-args-cont . c-lineup-ObjC-method-args) (objc-method-intro . [0]) (friend . 0) (cpp-define-intro c-lineup-cpp-define +) (cpp-macro-cont . +) (cpp-macro . [0]) (inclass . +) (stream-op . c-lineup-streamop) (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist) (arglist-cont c-lineup-gcc-asm-reg 0) (arglist-intro . +) (catch-clause . 0) (else-clause . 0) (do-while-closure . 0) (label . 2) (access-label . -) (substatement-label . 2) (substatement . +) (statement-case-open . 0) (statement-case-intro . +) (statement-block-intro . +) (statement-cont . +) (statement . 0) (brace-entry-open . 0) (brace-list-entry . 0) (brace-list-intro . +) (brace-list-close . 0) (brace-list-open . 0) (block-close . 0) (inher-cont . c-lineup-multi-inher) (inher-intro . +) (member-init-cont . c-lineup-multi-inher) (member-init-intro . +) (annotation-var-cont . +) (annotation-top-cont . 0) (topmost-intro-cont . c-lineup-topmost-intro-cont) (topmost-intro . 0) (knr-argdecl . 0) (func-decl-cont . +) (inline-close . 0) (inline-open . +) (class-close . 0) (class-open . 0) (defun-block-intro . +) (defun-close . 0) (defun-open . 0) (string . c-lineup-dont-change) (arglist-close . c-lineup-arglist) (substatement-open . 0) (case-label . 0) (block-open . 0) (c . 1) (comment-intro . 0) (knr-argdecl-intro . -)) (c-cleanup-list scope-operator brace-else-brace brace-elseif-brace brace-catch-brace empty-defun-braces list-close-comma defun-close-semi) (c-hanging-semi&comma-criteria c-semi&comma-no-newlines-before-nonblanks) (c-hanging-colons-alist (member-init-intro before) (inher-intro) (case-label after) (label after) (access-label after)) (c-hanging-braces-alist (substatement-open after) (brace-list-open after) (brace-entry-open) (defun-open after) (class-open after) (inline-open after) (block-open after) (block-close . c-snug-do-while) (statement-case-open after) (substatement after)) (c-comment-only-line-offset . 0) (c-tab-always-indent . t))))
 '(whitespace-style (quote (face tabs trailing space-before-tab indentation empty space-after-tab tab-mark))))

; Tame the echo-area font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(echo-area ((((type ns)) (:stipple nil :strike-through nil :underline nil :slant normal :weight normal :width normal))))
 '(whitespace-indentation ((((class color) (background dark)) (:background "#073642" :foreground "white")) (((class color) (background light)) (:background "#eee8d5" :foreground "black")) (t (:inverse-video t)))))
