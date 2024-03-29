

(add-to-list 'purpose-user-mode-purposes '(cider-repl-mode . shell))
(add-to-list 'purpose-user-mode-purposes '(clojure-mode . programming))
(add-to-list 'purpose-user-mode-purposes '(emacs-lisp-mode . programming))
(add-to-list 'purpose-user-mode-purposes '(java-mode . programming))
(add-to-list 'purpose-user-mode-purposes '(treemacs-mode . navigation))
(add-to-list 'purpose-user-regexp-purposes '("lsp-ui.*" . lsp-ui-doc))

(setq purpose-use-default-configuration t) ; not really necessary, default is t
(purpose-compile-user-configuration) ;
