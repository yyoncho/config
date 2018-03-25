
(setq lsp-java-server-install-dir
      "/home/kyoncho/Sources/lsp/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/")

(require 'lsp-mode)
(require 'lsp-ui-flycheck)
(require 'lsp-java)

(mapcar 'load-file
        (append
         (directory-files "/home/kyoncho/Sources/lsp/lsp-java" t ".*.el")
         (remove
          "/home/kyoncho/Sources/lsp/lsp-mode/lsp-flycheck.el"
          (directory-files "/home/kyoncho/Sources/lsp/lsp-mode" t ".*.el"))))

(load-file "/home/kyoncho/Sources/lsp/lsp-java/lsp-java.el")
;; (remove-hook 'java-mode-hook #'lsp-mode)
(add-hook 'java-mode-hook #'lsp-java-start)
(setq lsp-java--workspace-folders (list "/home/kyoncho/Sources/tick42-gds/"))
(setq lsp-print-io t)
(setq lsp-print-io nil)

(require 'lsp-ui)
(remove-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'company-lsp)

(push 'company-lsp company-backends)

(setq lsp-ui-sideline-enable nil)
;; (setq lsp-ui-sideline-ignore-duplicate nil)
;; (setq lsp-ui-sideline-show-symbol nil)
;; (setq lsp-ui-sideline-show-hover nil)
;; (setq lsp-ui-sideline-show-flycheck nil)
;; (setq lsp-ui-sideline-show-code-actions nil)
