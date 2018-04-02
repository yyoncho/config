
(setq lsp-java-server-install-dir
      "/home/kyoncho/Sources/lsp/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/")

(require 'lsp-mode)
(require 'lsp-ui-flycheck)
(require 'lsp-java)

(mapcar 'load-file
        (append
         (remove
          "/home/kyoncho/Sources/lsp/lsp-mode/lsp-flycheck.el"
          (directory-files "/home/kyoncho/Sources/lsp/lsp-mode" t ".*.el"))
         (directory-files "/home/kyoncho/Sources/lsp/lsp-java" t ".*.el")))

(load-file "/home/kyoncho/Sources/lsp/lsp-java/lsp-java.el")
;; (remove-hook 'java-mode-hook #'lsp-mode)
(add-hook 'java-mode-hook #'lsp-java-enable)
(remove-hook 'java-mode-hook #'lsp-java-start)

(setq lsp-java--workspace-folders (list "/home/kyoncho/Sources/tick42-gds/"))

(setq lsp-print-io t)
(setq lsp-print-io nil)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'company-lsp)

(push 'company-lsp company-backends)
(lsp-ui-sideline-mode t)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-ignore-duplicate nil)
(setq lsp-ui-sideline-show-symbol nil)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-flycheck nil)
(setq lsp-ui-sideline-show-code-actions t)

(defun lsp-java-organize-imports ()
  "Organize java imports."
  (interactive)
  (lsp--send-execute-command
   "java.edit.organizeImports"
   (list (lsp--path-to-uri buffer-file-name))))
