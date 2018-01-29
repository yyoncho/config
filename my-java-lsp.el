

(add-to-load-path "~/Sources/lsp-java")
(add-to-load-path "~/Sources/lsp-ui")
(add-to-load-path "~/Sources/lsp-mode")


(require 'lsp-java)
(require 'lsp-mode)
(require 'lsp-flycheck)

(add-hook 'java-mode-hook #'lsp-java-enable)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)


;; (require 'company-lsp)

;; (push 'company-lsp company-backends)
(setq lsp-java-server-install-dir "~/.jdt/")
