
(setq lsp-java-server-install-dir "~/.jdt/")

(require 'lsp-mode)
(require 'lsp-ui-flycheck)
(require 'lsp-java)

;; (remove-hook 'java-mode-hook #'lsp-mode)
(remove-hook 'java-mode-hook #'lsp-java-enable)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'company-lsp)

(push 'company-lsp company-backends)
