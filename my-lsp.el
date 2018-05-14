
(use-package lsp-mode
  :load-path "~/Sources/lsp/lsp-mode/"
  :init (setq lsp-inhibit-message t
              lsp-print-io nil
              lsp-highlight-symbol-at-point nil))

(use-package company-lsp
  :load-path "~/Sources/lsp/company-lsp/"
  :after  company
  :ensure nil
  :config
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t)
  (push 'company-lsp company-backends)
  (push 'java-mode company-global-modes))

(use-package lsp-ui
  :load-path "~/Sources/lsp/lsp-ui/"
  :config
  (setq lsp-ui-flycheck-report-all-buffers t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-flycheck t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-use-window t))

(use-package lsp-java
  :load-path "~/Sources/lsp/lsp-java/"
  :requires (lsp-ui-flycheck lsp-ui-sideline)
  :ensure nil
  :hook ((java-mode . lsp-java-enable)
         (java-mode . flycheck-mode)
         (java-mode . smartparens-mode)
         (java-mode . company-mode)
         (java-mode . evil-cleverparens-mode)
         (java-mode . evil-smartparens-mode)
         (java-mode . (lambda () (lsp-ui-flycheck-enable 1)))
         (java-mode . (lambda ()
                        (add-to-list 'spacemacs-jump-handlers
                                     '(xref-find-definitions :async true))))
         (java-mode . lsp-ui-sideline-mode))
  :config
  (setq lsp-java-server-install-dir (expand-file-name "~/Sources/lsp/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/")
        lsp-java--workspace-folders (list "/home/kyoncho/Sources/tick42-gds/"
                                          "/home/kyoncho/Sources/cm/java-server-backend/"
                                          "/home/kyoncho/Sources/cm/java-storage-common/"
                                          "/home/kyoncho/Sources/cm/java-storage-file/"
                                          "/home/kyoncho/Sources/cm/java-server-app/"
                                          "/home/kyoncho/Sources/cm/java-server-core/"
                                          "/home/kyoncho/Sources/cm/java-configmanager-it/"
                                          "/home/kyoncho/Sources/java-entitlement-system/"))
  (spacemacs/set-leader-keys-for-major-mode 'java-mode
    "rr" 'lsp-update-and-run
    "rs" 'lsp-rename
    "ft" 'helm-lsp-workspace-symbol
    "roi" 'lsp-java-organize-imports
    "fp" 'my/find-pom-file))

(defun lsp-update-and-run ()
  "Request code action to automatically fix issues reported by
the diagnostics."
  (interactive)
  (lsp--cur-workspace-check)
  (with-demoted-errors
      "%s"
    (lsp--send-request-async
     (lsp--make-request
      "textDocument/codeAction"
      (lsp--text-document-code-action-params))
     (lambda (actions)
       (setq lsp-code-actions actions)
       (condition-case nil
           (call-interactively 'lsp-execute-code-action)
         (quit "Quit"))))))
