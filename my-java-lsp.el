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
;; (remove-hook 'java-mode-hook #'lsp-java-start)
(add-hook 'java-mode-hook #'lsp-java-enable)
(add-hook 'java-mode-hook #'lsp-ui-mode)
(add-hook 'java-mode-hook #'flycheck-mode)

(setq lsp-java--workspace-folders (list "/home/kyoncho/Documents/Sources/demo/2/"
                                        "/home/kyoncho/Documents/Sources/demo/temp/"))

(setq lsp-print-io t)
(setq lsp-print-io nil)

(load-file "/home/kyoncho/Documents/Sources/lsp/lsp-java/lsp-java.el")
(require 'lsp-ui)
(require 'lsp-ui-imenu)
(require 'lsp-imenu)
(remove-hook 'lsp-mode-hook 'lsp-ui-mode)

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

;; (defun lsp-compile ()
;;   "Execute code action ACTION."
;;   (interactive)
;;   (lsp--send-execute-command
;;    "java.workspace.compile"
;;    (list)))

;; (defun lsp-java-execute-code-action ()
;;   "Execute code action ACTION."
;;   (interactive)
;;   (let* ((actions (seq-group-by #'lsp--command-get-title lsp-code-actions))
;;          (action (second (assoc (completing-read "Select code action: " actions)
;;                                 actions))))
;;     (lsp--apply-workspace-edit (first (gethash "arguments" action)))))

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



(provide 'lsp-java)
