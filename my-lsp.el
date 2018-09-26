(use-package lsp-java
  :requires (lsp-ui-flycheck lsp-ui-sideline)
  :load-path "~/Sources/lsp/lsp-java/"
  :ensure nil
  :hook ((java-mode . smartparens-mode)
         (java-mode . evil-cleverparens-mode)
         (java-mode . evil-smartparens-mode)
         (java-mode . rainbow-delimiters-mode)
         (java-mode . my/configure-java))
  :config
  ;; (setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044,quiet=y"))
  ;; (setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044,quiet=y"))
  (setq lsp-java-favorite-static-members '("org.junit.Assume.*"
                                           "java.util.Collections.*"
                                           "org.junit.jupiter.api.Assertions.*"
                                           "org.junit.jupiter.api.Assumptions.*"
                                           "org.junit.jupiter.api.DynamicContainer.*"
                                           "org.junit.jupiter.api.DynamicTest.*")
        lsp-java-format-settings-url nil ; "file:///home/kyoncho/Documents/tick42.xml"
        lsp-java-format-settings-profile "Tick42"

        lsp-java-completion-guess-arguments t)
  (spacemacs/set-leader-keys-for-major-mode 'java-mode
    "fa" 'lsp-workspace-folders-add
    "fr" 'lsp-workspace-folders-remove
    "fs" 'lsp-workspace-folders-switch
    "fp" 'my/find-pom-file)

  (defun my/configure-java ()
    "Configure java"
    (interactive)
    (setq tab-width 4)))

(use-package helm-lsp
  :load-path "~/Sources/lsp/helm-lsp/")

(use-package lsp-java
  :load-path "~/Sources/lsp/lsp-java/")

(use-package company-lsp
  :load-path "~/Sources/lsp/company-lsp")

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
  :load-path "~/Sources/lsp/dap-mode/"
  :config
  (require 'dap-java)
  (require 'lsp-java)
  (require 'dap-ui)
  (dap-turn-on-dap-mode)

  (dap-ui-mode 1)

  (add-hook 'dap-ui-sessions-mode-hook 'evil-evilified-state)
  (add-hook 'dap-ui-breakpoints-ui-list-mode-hook 'evil-evilified-state)
  (add-hook 'bui-after-redisplay-hook 'evil-evilified-state)
  (add-hook 'dap-ui-breakpoints-ui-list-displayed-hook 'evil-evilified-state)

  (add-hook 'dap-ui-repl-mode-hook 'company-mode)

  (defun my/show-and-copy-class-name ()
    "Show and copy classname."
    (interactive)
    (if-let ((class-name (->> (list :cursorOffset (point)
                                    :sourceUri (lsp--path-to-uri (buffer-file-name)))
                              (lsp-send-execute-command "che.jdt.ls.extension.findTestByCursor")
                              first)))
        (message (kill-new class-name))
      (error "No classname")))

  (spacemacs/set-leader-keys-for-major-mode 'java-mode
    "dq" 'dap-disconnect
    "dQ" 'dap-delete-all-sessions
    "dn" 'dap-next
    "di" 'dap-step-in
    "dc" 'dap-continue
    "do" 'dap-step-out
    "ee" 'dap-eval
    "er" 'dap-eval-region
    "es" 'dap-eval-thing-at-point
    "ss" 'dap-switch-session
    "'"  'dap-ui-repl
    "st" 'dap-switch-thread
    "sf" 'dap-switch-stack-frame
    "go" 'dap-go-to-output-buffer
    "sD" 'dap-delete-session
    "xd" 'dap-debug
    "xr" 'dap-debug-recent
    "xl" 'dap-debug-last
    "xj" 'dap-java-debug
    "eis" 'dap-ui-inspect-thing-at-point
    "eie" 'dap-ui-inspect
    "eir" 'dap-ui-inspect-region
    "is" 'dap-ui-inspect-thing-at-point
    "tt" 'dap-java-debug-test-method
    "tc" 'dap-java-debug-test-class

    "ll" 'dap-ui-locals
    "lb" 'dap-ui-breakpoints

    ;; breakpoints
    "bb" 'dap-breakpoint-toggle
    "bd" 'dap-breakpoint-delete
    "ba" 'dap-breakpoint-add
    "bc" 'dap-breakpoint-condition
    "bl" 'dap-breakpoint-log-message
    "bh" 'dap-breakpoint-hit-condition
    "." 'hydra-dap)

  (setq lsp-java-bundles ()))
