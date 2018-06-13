(use-package lsp-mode
  :load-path "~/Sources/lsp/lsp-mode/"
  :init (setq lsp-inhibit-message t
              lsp-print-io nil
              lsp-eldoc-render-all nil
              lsp-highlight-symbol-at-point nil))

(use-package company-lsp
  :load-path "~/Sources/lsp/company-lsp/"
  :after  company
  :ensure nil
  :hook ((java-mode . (lambda () (push 'company-lsp company-backends))))
  :config
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t)
  (push 'java-mode company-global-modes))

(use-package lsp-ui
  :load-path "~/Sources/lsp/lsp-ui/"
  :ensure nil
  :config
  (setq lsp-ui-flycheck-report-all-buffers nil
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-delay 0.2
        lsp-ui-sideline-show-flycheck t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-use-window nil))

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
         (java-mode . (lambda ()
                        (setq tab-width 4)
                        (lsp-ui-flycheck-enable 1)))
         (java-mode . (lambda ()
                        (add-to-list 'spacemacs-jump-handlers
                                     '(xref-find-definitions :async true))))
         (java-mode . lsp-ui-sideline-mode))
  :config
  (setq lsp-java-server-install-dir (expand-file-name "~/Sources/lsp/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/")
        ;; lsp-java--workspace-folders (list "/home/kyoncho/Sources/demo/2/"
        ;;                                   "/home/kyoncho/Sources/lsp/spring-boot-and-java-10/")
        lsp-java-favorite-static-members '("org.junit.Assert.*"
                                           "org.junit.Assume.*"
                                           "java.util.Collections.*"
                                           "org.junit.jupiter.api.Assertions.*"
                                           "org.junit.jupiter.api.Assumptions.*"
                                           "org.junit.jupiter.api.DynamicContainer.*"
                                           "org.junit.jupiter.api.DynamicTest.*")
        lsp-java-format-settings-url "file:///home/kyoncho/Documents/tick42.xml"
        lsp-java-format-settings-profile "Tick42")
  (spacemacs/set-leader-keys-for-major-mode 'java-mode
    "rr" 'lsp-update-and-run
    "rs" 'lsp-rename
    "roi" 'lsp-java-organize-imports
    "rcl" 'lsp-java-create-local
    "rcf" 'lsp-java-create-field
    "hh" 'lsp-describe-thing-at-point
    "rcp" 'lsp-java-create-parameter
    "rel" 'lsp-java-extract-to-local-variable
    "rec" 'lsp-java-extract-to-constant
    "rws" 'helm-lsp-workspace-symbol
    "rfu" 'xref-find-references
    "fp" 'my/find-pom-file))

(use-package helm-lsp
  :load-path "~/Sources/lsp/helm-lsp/")

;; (defun my/find-pom-file ()
;;   "Find file in upper dirs"
;;   (interactive)
;;   (if-let* (pf (expand-file-name
;;                 (concat (locate-dominating-file
;;                          (if (string= (file-name-nondirectory (buffer-file-name)) "pom.xml")
;;                              (file-name-directory
;;                               (directory-file-name (file-name-directory (buffer-file-name))))
;;                            (buffer-file-name))
;;                          "pom.xml")
;;                         "pom.xml")))
;;       (find-file pf)
;;     (message "Unable to find pom.xml")))

(defun my/find-pom-file ()
  "Find file in upper dirs"
  (interactive)
  (if-let* ((pf (expand-file-name
                 (concat (locate-dominating-file
                          (if (string= (file-name-nondirectory (buffer-file-name)) "pom.xml")
                              (file-name-directory
                               (directory-file-name (file-name-directory (buffer-file-name))))
                            (buffer-file-name))
                          "pom.xml")
                         "pom.xml"))))
      (find-file pf)
    (message "Unable to find pom.xml")))

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
