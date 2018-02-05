;; (setq lsp-java-server-install-dir "/home/kyoncho/.vscode/extensions/redhat.java-0.21.0/server/")
;; (setq lsp-java-server-install-dir "/home/kyoncho/Sources/lsp/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository")
(setq lsp-java-server-install-dir "/home/kyoncho/Sources/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository")

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

(load-file "/home/kyoncho/Documents/Sources/lsp/lsp-java/lsp-java.el")
(require 'lsp-ui)
(require 'lsp-ui-imenu)
(require 'lsp-imenu)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'company-lsp)
;; (setq  lsp-print-io nil)
(setq  lsp-print-io t)
(push 'company-lsp company-backends)

(defun lsp-java-organize-imports ()
  "Execute code action ACTION."
  (interactive)
  (lsp--send-execute-command
   "java.edit.organizeImports"
   (list (lsp--path-to-uri buffer-file-name))))

;; (defun lsp-java-organize-imports ()
;;   "Execute code action ACTION."
;;   (interactive)
;;   (lsp--send-execute-command
;;    "java.show.implementations"
;;    (list (lsp--path-to-uri buffer-file-name))))

(defun lsp-compile ()
  "Execute code action ACTION."
  (interactive)
  (lsp--send-execute-command
   "java.workspace.compile"
   (list)))

(defun lsp-java-execute-code-action ()
  "Execute code action ACTION."
  (interactive)
  (let* ((actions (seq-group-by #'lsp--command-get-title lsp-code-actions))
         (action (second (assoc (completing-read "Select code action: " actions)
                                actions))))
    (lsp--apply-workspace-edit (first (gethash "arguments" action)))))

(spacemacs/set-leader-keys-for-major-mode 'java-mode
  "rtf" 'jr-convert-to-field
  "raf" 'jr-add-field
  "rC" 'jr-create-constructor-from-fields
  "ab" 'java-ide-add-breakpoint
  "dc" 'java-ide-debug-class
  "dt" 'java-ide-debug-test
  "fp" 'my/find-pom-file
  "a" 'lsp-java-execute-code-action
  )

(defun lsp-java--ls-command ()
  (let ((server-jar (lsp-java--locate-server-jar))
        (server-config (lsp-java--locate-server-config))
        (root-dir (lsp-java--get-root)))
    `( "java"
       "-Declipse.application=org.eclipse.jdt.ls.core.id1"
       "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044"
       ;; "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044"
       ;; -agentlib:jdwp=transport=dt_socket,address=9999,server=y,suspend=y
       "-Dosgi.bundles.defaultStartLevel=4"
       "-Declipse.product=org.eclipse.jdt.ls.core.product"
       ;; "-Xdebug"
       "-Dlog.protocol=true"
       "-Dlog.level=ALL"
       "-noverify"
       "-Xmx1G"
       "-jar"
       ,server-jar
       "-configuration"
       ,server-config
       "-data"
       "/home/kyoncho/workspace-emacs")))

(defun lsp-java--get-root ()
  "Retrieves the root directory of the java project root if available.
The current directory is assumed to be the java project’s root otherwise."
  (cond
   ((and (featurep 'projectile) (projectile-project-p)) (projectile-project-root))
   ((vc-backend default-directory) (expand-file-name (vc-root-dir)))
   (t (let ((project-types '("pom.xml" "build.gradle" ".project")))
        (or (seq-some (lambda (file) (locate-dominating-file default-directory file)) project-types)
            default-directory)))))



;; (setq lsp-response-timeout 1000)
(lsp-define-stdio-client lsp-java "java" #'lsp-java--get-root  (lsp-java--ls-command)
                         :ignore-regexps
                         '("^SLF4J: "
                           "^Listening for transport dt_socket at address: ")
                         :extra-init-params (list :workspaceFolders (list (lsp--path-to-uri (lsp-java--get-root)))
                                                  :settings '((java (jdt (ls (vmargs . "-noverify -Xmx1G -XX:+UseG1GC -XX:+UseStringDeduplication"))) (errors (incompleteClasspath (severity . "warning"))) (configuration (updateBuildConfiguration . "interactive") (maven)) (trace (server . "off")) (import (gradle (enabled . t)) (maven (enabled . t)) (exclusions . ["**/node_modules/**" "**/.metadata/**" "**/archetype-resources/**" "**/META-INF/maven/**"])) (referencesCodeLens (enabled . :json-false)) (signatureHelp (enabled . :json-false)) (implementationsCodeLens (enabled . :json-false)) (format (enabled . t)) (saveActions (organizeImports . :json-false)) (contentProvider) (autobuild (enabled . t)) (completion (favoriteStaticMembers . ["org.junit.Assert.*" "org.junit.Assume.*" "org.junit.jupiter.api.Assertions.*" "org.junit.jupiter.api.Assumptions.*" "org.junit.jupiter.api.DynamicContainer.*" "org.junit.jupiter.api.DynamicTest.*"]) (importOrder . ["java" "javax" "com" "org"])) (debug (logLevel . "warn") (settings (showHex . :json-false) (showStaticVariables . t) (showQualifiedNames . :json-false) (maxStringLength . 0.0) (enableHotCodeReplace . :json-false))) (test (report (position . "sideView")))))))

(provide 'lsp-java)
;;; lsp-java.el ends here

(defun lsp-java--get-definitions ()
  "Get definition of the current symbol under point.
Returns xref-item(s)."
  (lsp--send-request (lsp--make-request
                      "java.show.implementations"
                      (lsp--text-document-position-params))

                     ))

(defun lsp-----java-organize-imports ()
  "Execute code action ACTION."
  (interactive)
  (lsp--send-execute-command
   "java.show.implementations"
   (lsp--text-document-position-params)))


(remove-hook 'java-mode-hook #'lsp-mode)
(remove-hook 'java-mode-hook #'lsp-java-enable)
