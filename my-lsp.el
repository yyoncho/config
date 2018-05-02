(mapcar 'load-file
        (append
         (remove
          "/home/kyoncho/Sources/lsp/lsp-mode/lsp-flycheck.el"
          (directory-files "/home/kyoncho/Sources/lsp/lsp-mode" t ".*.el"))
         (directory-files "/home/kyoncho/Sources/lsp/lsp-java" t ".*.el")
         (directory-files "/home/kyoncho/Sources/lsp/company-lsp" t ".*.el")
         (directory-files "/home/kyoncho/Sources/lsp/lsp-ui" t ".*.el")))

(setq lsp-java-server-install-dir "/home/kyoncho/Sources/lsp/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/")

(setq lsp-java--workspace-folders
      (list "/home/kyoncho/Sources/tick42-gds/"
            "/home/kyoncho/Sources/cm/java-server-backend/"
            "/home/kyoncho/Sources/cm/java-storage-common/"
            "/home/kyoncho/Sources/cm/java-storage-file/"
            "/home/kyoncho/Sources/cm/java-server-app/"
            "/home/kyoncho/Sources/cm/java-server-core/"
            "/home/kyoncho/Sources/cm/java-configmanager-it/"
            "/home/kyoncho/Sources/java-entitlement-system/"))

(setq lsp-inhibit-message t)
(require 'lsp-mode)
(require 'lsp-ui-flycheck)
(require 'lsp-java)

(add-hook 'java-mode-hook 'lsp-java-enable)
(add-hook 'java-mode-hook 'flycheck-mode)
(add-hook 'java-mode-hook
          (lambda ()
            (add-to-list 'spacemacs-jump-handlers
                         '(xref-find-definitions :async true))))

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-sideline-mode)

(require 'company-lsp)
(push 'company-lsp company-backends)
(setq lsp-print-io nil)
;; (setq lsp-print-io t)

(defun lsp-update-and-run ()
  "Request code action to automatically fix issues reported by
the diagnostics."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-request-async (lsp--make-request
                            "textDocument/codeAction"
                            (lsp--text-document-code-action-params))
                           (lambda (actions)
                             (setq lsp-code-actions actions)
                             (condition-case nil
                                 (call-interactively 'lsp-execute-code-action)
                               (quit "Quit")))))

(spacemacs/set-leader-keys-for-major-mode 'java-mode
  "rr" 'lsp-update-and-run
  "rs" 'lsp-rename
  "roi" 'lsp-java-organize-imports
  "fp" 'my/find-pom-file)

(push 'company-lsp company-backends)

(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-symbol nil)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-flycheck t)
(setq lsp-ui-sideline-show-code-actions t)
(setq lsp-highlight-symbol-at-point nil)
(setq lsp-enable-codeaction nil)
(setq lsp-ui-doc-use-childframe nil)
(setq lsp-ui-doc-use-window t)
