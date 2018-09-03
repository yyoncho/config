(use-package lsp-java
  :requires (lsp-ui-flycheck lsp-ui-sideline)
  :ensure nil
  :hook ((java-mode . smartparens-mode)
         (java-mode . evil-cleverparens-mode)
         (java-mode . evil-smartparens-mode)
         (java-mode . rainbow-delimiters-mode)
         (java-mode . my/configure-java))
  :config
  ;; (setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044,quiet=y"))
  (setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044,quiet=y"))
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

  (setq lsp-java-bundles (list
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.sat4j.pb_2.3.5.v201404071733.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.sat4j.core_2.3.5.v201308161310.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.ecf.identity_3.8.0.v20161203-2153.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.ecf.provider.filetransfer_3.2.300.v20161203-1840.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.concurrent_1.1.0.v20130327-1442.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.ecf_3.8.0.v20170104-0657.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.ecf.filetransfer_5.0.0.v20160817-1024.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.apache.felix.scr_2.0.10.v20170501-2007.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.transport.ecf_1.1.300.v20161004-0244.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.tukaani.xz_1.5.0.v20170111-1717.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.engine_2.5.0.v20170319-2002.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.repository_2.3.301.v20170906-1259.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.ds_1.5.0.v20170307-1429.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.metadata.repository_1.2.401.v20170906-1259.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.metadata_2.3.200.v20170511-1106.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.garbagecollector_1.0.300.v20160504-1450.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.jarprocessor_1.0.500.v20160504-1450.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.artifact.repository_1.1.650.v20170928-1405.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.director_2.3.300.v20160504-1450.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.director.app_1.0.500.v20160419-0834.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.publisher.eclipse_1.2.201.v20170906-1259.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.publisher_1.4.200.v20170511-1216.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.pde.build_3.9.300.v20170515-0912.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.update.configurator_3.3.400.v20160506-0750.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.team.core_3.8.100.v20170516-0820.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.repository.tools_2.1.400.v20170511-1216.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.core_2.4.101.v20170906-1259.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.pde.core_3.11.100.v20170517-0724.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.touchpoint.natives_1.2.200.v20170511-1216.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.equinox.p2.touchpoint.eclipse_2.1.501.v20170906-1259.jar"
                          "/home/kyoncho/.vscode/extensions/testtt/server/org.eclipse.jdt.ls.importer.pde_1.0.0.20171101-2108.jar"
                          "/home/kyoncho/.vscode/extensions/vscjava.vscode-java-debug-0.11.0/server/com.microsoft.java.debug.plugin-0.11.0.jar"
                          "/home/kyoncho/.vscode/extensions/vscjava.vscode-java-test-0.8.0/server/com.microsoft.java.test.plugin-0.8.0.jar")))
