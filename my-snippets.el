;;; my-snippets.el --- Completion related code       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Ivan Yonchovski

;; Author: Ivan Yonchovski <ivan.yonchovski@tick42.com>


(setq auto-insert-query nil)

(setq auto-insert-directory "~/.remote-config/config/templates/")
(add-hook 'find-file-hook 'auto-insert)
(auto-insert-mode 1)

(add-to-list 'auto-insert-alist '("\\.java" . ["default-java.el" my/autoinsert-yas-expand]))

(defun my/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))
