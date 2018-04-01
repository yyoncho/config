;; (setq lsp-java-server-install-dir "/home/kyoncho/.vscode/extensions/redhat.java-0.21.0/server/")
;; (setq lsp-java-server-install-dir "/home/kyoncho/Sources/lsp/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository")
(setq lsp-java-server-install-dir "/home/kyoncho/Sources/lsp/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository")


(mapcar 'load-file
        (append
         (remove
          "/home/kyoncho/Sources/lsp/lsp-mode/lsp-flycheck.el"
          (directory-files "/home/kyoncho/Sources/lsp/lsp-mode" t ".*.el"))
         (directory-files "/home/kyoncho/Sources/lsp/lsp-java" t ".*.el")
         (directory-files "/home/kyoncho/Sources/lsp/lsp-ui" t ".*.el")))


(load-file "/home/kyoncho/Sources/lsp/lsp-java/lsp-java.el")

(require 'lsp-mode)
(require 'lsp-ui-flycheck)
(require 'lsp-java)

;; (remove-hook 'java-mode-hook #'lsp-mode)
;; (remove-hook 'java-mode-hook #'lsp-java-start)

(add-hook 'java-mode-hook #'lsp-java-enable)

;; (remove-hook 'java-mode-hook #'lsp-ui-mode)

(add-hook 'java-mode-hook #'flycheck-mode)

(setq lsp-java--workspace-folders (list "/home/kyoncho/Documents/Sources/demo/2/"
                                        "/home/kyoncho/Documents/Sources/demo/temp/"))

(setq lsp-print-io t)
(setq lsp-print-io nil)

(require 'lsp-ui)
(require 'lsp-ui-imenu)
(require 'lsp-imenu)
(remove-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'company-lsp)
;; (setq  lsp-print-io nil)
(setq  lsp-print-io t)
(push 'company-lsp company-backends)



(spacemacs/set-leader-keys-for-major-mode 'java-mode
  "rtf" 'jr-convert-to-field
  "raf" 'jr-add-field
  "rC" 'jr-create-constructor-from-fields
  ;; "ab" 'java-ide-add-breakpoint
  ;; "dc" 'java-ide-debug-class
  ;; "dt" 'java-ide-debug-test
  "fp" 'my/find-pom-file
  "a" 'lsp-java-execute-code-action
  )

;; (setq lsp--workspaces (make-hash-table :test #'equal))
