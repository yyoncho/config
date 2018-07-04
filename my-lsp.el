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
        lsp-ui-sideline-show-code-actions t
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
  (setq lsp-java-server-install-dir (expand-file-name "~/Sources/lsp/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/")
        lsp-java--workspace-folders (list "/home/kyoncho/Sources/demo/2/"
                                          "/home/kyoncho/Sources/lsp/spring-boot-and-java-10/")
        lsp-java-favorite-static-members '("org.junit.Assert.*"
                                           "org.junit.Assume.*"
                                           "java.util.Collections.*"
                                           "org.junit.jupiter.api.Assertions.*"
                                           "org.junit.jupiter.api.Assumptions.*"
                                           "org.junit.jupiter.api.DynamicContainer.*"
                                           "org.junit.jupiter.api.DynamicTest.*")
        lsp-java-format-settings-url "file:///home/kyoncho/Documents/tick42.xml"
        lsp-java-format-settings-profile "Tick42"
        lsp-java-completion-guess-arguments t)
  (spacemacs/set-leader-keys-for-major-mode 'java-mode
    "gt"  'lsp-goto-type-definition
    "gr"  'xref-find-references
    "gR"  'lsp-ui-peek-find-references
    "ha"  'xref-find-apropos
    "hA"  'lsp-ui-peek-find-workspace-symbol
    "hh"  'lsp-describe-thing-at-point
    "pu"  'lsp-java-update-user-settings
    "el"  'lsp-ui-flycheck-list
    "ea"  'lsp-execute-code-action
    "qr"  'lsp-restart-workspace
    "roi" 'lsp-java-organize-imports
    "rrs" 'lsp-rename
    "rcp" 'lsp-java-create-parameter
    "rai" 'lsp-java-add-import
    "rcf" 'lsp-java-create-field
    "rec" 'lsp-java-extract-to-constant
    "rel" 'lsp-java-extract-to-local-variable
    "ram" 'lsp-java-add-unimplemented-methods
    "rem" 'lsp-java-extract-method
    "cc"  'lsp-java-build-project
    "an"  'lsp-java-actionable-notifications
    "="   'lsp-format-buffer))

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
  :load-path "~/Sources/lsp/dap-mode/"
  :config
<<<<<<< HEAD
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
;; (vhl/install-extension ')

;; (vhl/define-extension 'my-evil 'evil-paste-after 'evil-paste-before
;;                       'evil-yank-line
;;                       'evil-yank
;;                       'evil-paste-pop 'evil-move)


;; (vhl/define-extension 'my-cp-evil
;;                       'evil-cp-yank 'evil-cp-yank-sexp)

;; (vhl/install-extension 'my-cp-evil)

;; (vhl/give-advice-to-make-vhl-on-changes evil-cp-yank-sexp)
;; vhl/use-my-evil-extension-p
=======
  (require 'dap-java)
  (require 'dap-ui)
  (require 'gdb-mi)
  (dap-turn-on-dap-mode)

  (dap-ui-mode t)
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
    "ls" 'dap-ui-sessions
    "ss" 'dap-switch-session
    "st" 'dap-switch-thread
    "sf" 'dap-switch-stack-frame
    "go" 'dap-go-to-output-buffer)
  (setq lsp-java-bundles (thread-first "eclipse.jdt.ls/plugins/com.microsoft.java.debug.plugin-0.9.0.jar"
                           locate-user-emacs-file
                           expand-file-name
                           list))
  (add-hook 'dap-ui-sessions-mode-hook 'evil-evilified-state))
>>>>>>> 73bae981023edfaf12e9df93444f0a8602af6019
