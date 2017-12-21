;;; my-java-lsp.el --- s                             -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Ivan Yonchovski

;; Author: Ivan Yonchovski <ivan.yonchovski@tick42.com>
;; Keywords:

(require 'lsp-java)
(require 'lsp-mode)

(add-hook 'java-mode-hook #'lsp-java-enable)

(require 'company-lsp)

(push 'company-lsp company-backends)
(setq lsp-java-server-install-dir "~/.jdt/")
