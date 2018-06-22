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
        lsp-ui-sideline-update-mode 'point))

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
  (setq ;; lsp-java-server-install-dir (expand-file-name "~/Sources/lsp/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/")
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
    "ra" 'lsp-execute-code-action
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

(use-package dap-mode
  :init (progn
          (add-hook 'lsp-after-open-hook 'dap-ui-mode))
  :load-path "~/Sources/lsp/dap-mode/"
  :config
  (progn
    (require 'dap-java)
    (require 'dap-ui)
    (require 'gdb-mi)
    (dap-turn-on-dap-mode)
    (spacemacs/set-leader-keys-for-major-mode 'java-mode
      "dq" 'dap-disconnect
      "dn" 'dap-next
      "xj" 'dap-java-debug
      "xl" 'dap-debug-last-configuration
      "di" 'dap-step-in
      "dc" 'dap-continue
      "db" 'dap-toggle-breakpoint
      "do" 'dap-step-out
      "ee" 'dap-eval
      "er" 'dap-eval-region
      "es" 'dap-eval-dwim
      "ls" 'dap-ui-list-sessions
      "ss" 'dap-switch-session
      "st" 'dap-switch-thread
      "sf" 'dap-switch-stack-frame)
    (setq lsp-java-bundles (thread-first "eclipse.jdt.ls/plugins/com.microsoft.java.debug.plugin-0.9.0.jar"
                             locate-user-emacs-file
                             expand-file-name
                             list))))
