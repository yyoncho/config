
(load-file "/home/kyoncho/Sources/lsp/lsp-java/lsp-java.el")
(setq lsp-java-server-install-dir "/home/kyoncho/Sources/lsp/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/")
(setq lsp-java--workspace-folders (directory-files "~/Sources/cm" t "^[[:alpha:])_]"))

(require 'lsp-mode)
(require 'lsp-ui-flycheck)
(require 'lsp-java)

(add-hook 'java-mode-hook #'lsp-java-start)
;; (add-hook 'java-mode-hook #'lsp-java-enable)

;; (require 'lsp-ui)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq lsp-print-io t)
(require 'company-lsp)

(push 'company-lsp company-backends)
